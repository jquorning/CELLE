--
--  The author disclaims copyright to this source code.  In place of
--  a legal notice, here is a blessing:
--
--    May you do good and not evil.
--    May you find forgiveness for yourself and forgive others.
--    May you share freely, not taking more than you give.
--

with Interfaces.C.Strings;

with Symbols;
with Rules;
with Errors;
with Lemon_Bind;

package body Cherry is

   procedure Find_States
     (Lemp : in out Lime.Lemon_Record)
   is
      use Interfaces.C.Strings;
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

      if Lemp.Start /= Null_Ptr then
         SP := Lime_Symbol_Find (Lemp.Start);
         if SP = null then
            Errors.Error_1 (Value (Lemp.File_Name), Value (Lemp.Start),
                            From_Key (Lemp.Start_Rule.LHS.all.Name));
            Lemp.Error_Cnt := Lemp.Error_Cnt + 1;
            SP := Lemp.Start_Rule.LHS;
         end if;
      else
         SP := Lemp.Start_Rule.LHS;
      end if;

      --  Make sure the start symbol doesn't occur on the right-hand side of
      --  any rule.  Report an error if it does.  (YACC would generate a new
      --  start symbol in this case.)
      RP := Lemp.Rule;
      loop
         exit when RP = null;
         for I in RP.RHS'Range loop
            if RP.RHS (I) = SP then   --  FIX ME:  Deal with multiterminals XXX
               Errors.Error_2 (Value (Lemp.File_Name), From_Key (SP.Name));
               Lemp.Error_Cnt := Lemp.Error_Cnt + 1;
            end if;
         end loop;
         RP := RP.Next;
      end loop;

      --  The basis configuration set for the first state
      --  is all rules which have the start symbol as their
      --  left-hand side
      RP := SP.Rule;
      loop
         exit when RP = null;
         declare
            New_CFP : Lime.Config_Access;
         begin
            RP.LHS_Start := 1;
            New_CFP := Configlist_Add_Basis (RP, 0);
            Set_Add (New_CFP.Follow_Set, 0);
         end;
         RP := RP.Next_LHS;
      end loop;

      --  Compute the first state.  All other states will be
      --  computed automatically during the computation of the first one.
      --  The returned pointer to the first state is not used. */
      Get_State (Lemp);

   end Find_States;

end Cherry;
