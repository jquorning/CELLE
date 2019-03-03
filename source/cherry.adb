--
--  The author disclaims copyright to this source code.  In place of
--  a legal notice, here is a blessing:
--
--    May you do good and not evil.
--    May you find forgiveness for yourself and forgive others.
--    May you share freely, not taking more than you give.
--

with Ada.Strings.Unbounded;

with Rules;
with Errors;
with Lemon_Bind;
with Configs;

package body Cherry is

   procedure Find_States
     (Lemp : in out Lime.Lemon_Record)
   is
      use Ada.Strings.Unbounded;
      use Lemon_Bind;
      use Symbols;
      use Rules;

      SP : Symbol_Access;
      RP : Rule_Access;
   begin
      Configlist_Init;

      --  Find the start symbol
      --  lime_partial_database_dump_c ();
      --  lime_partial_database_dump_ada ();

      if Lemp.Names.Start /= "" then
         SP := Symbol_Find (To_String (Lemp.Names.Start));
         if SP = null then
            Errors.Error_Plain
              (File_Name   => Lemp.File_Name,
               Line_Number => 0,
               Text        =>
                 "The specified start symbol '%1' Start is not in a nonterminal " &
                 "of the grammar.  '%2' will be used as the start symbol instead.",
               Arguments   => (1 => Lemp.Names.Start,
                               2 => To_Unbounded_String (From_Key (Lemp.Start_Rule.LHS.all.Name)))
              );
            Lemp.Error_Cnt := Lemp.Error_Cnt + 1;
            SP := Symbol_Access (Lemp.Start_Rule.LHS);
         end if;
      else
         SP := Symbol_Access (Lemp.Start_Rule.LHS);
      end if;

      --  Make sure the start symbol doesn't occur on the right-hand side of
      --  any rule.  Report an error if it does.  (YACC would generate a new
      --  start symbol in this case.)
      RP := Lemp.Rule;
      loop
         exit when RP = null;
         for I in RP.RHS'Range loop
            if RP.RHS (I) = SP then   --  FIX ME:  Deal with multiterminals XXX
               Errors.Error_Plain
                 (File_Name   => Lemp.File_Name,
                  Line_Number => 0,
                  Text        =>
                    "The start symbol '%1' occurs on the right-hand " &
                    "side of a rule. This will result in a parser which " &
                    "does not work properly.",
                  Arguments   => (1 => To_Unbounded_String (From_Key (SP.Name)))
                 );
               Lemp.Error_Cnt := Lemp.Error_Cnt + 1;
            end if;
         end loop;
         RP := RP.Next;
      end loop;

      --  The basis configuration set for the first state
      --  is all rules which have the start symbol as their
      --  left-hand side
      RP := Rule_Access (SP.Rule);
      loop
         exit when RP = null;
         declare
            New_CFP : Configs.Config_Access;
         begin
            RP.LHS_Start := True;
            New_CFP := Configlist_Add_Basis (RP, 0);
            Set_Add (To_String (New_CFP.Follow_Set), 0);
         end;
         RP := RP.Next_LHS;
      end loop;

      --  Compute the first state.  All other states will be
      --  computed automatically during the computation of the first one.
      --  The returned pointer to the first state is not used. */
      Get_State (Lemp);

   end Find_States;


   procedure Add_The_Accepting_Token
     (Lemp : in out Lime.Lemon_Record;
      SP   : in out Symbols.Symbol_Access)
   is
      use Ada.Strings.Unbounded;
      use Symbols;
   begin
      if Lemp.Names.Start /= "" then
         SP := Symbol_Find (To_String (Lemp.Names.Start));
         if SP = null then
            SP := Symbol_Access (Lemp.Start_Rule.LHS);
         end if;
      else
         SP := Symbol_Access (Lemp.Start_Rule.LHS);
      end if;
   end Add_The_Accepting_Token;


end Cherry;
