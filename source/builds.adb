--
--
--

with Ada.Strings.Unbounded;

with Rules;
with Symbols;
with Sets;
with Errors;
with Lemon_Bind;
with Configs;

package body Builds is


   procedure Find_Rule_Precedences (Lemon : in out Lime.Lemon_Record)
   is
      use Rules;
      use Symbols;

      Rule : Rule_Access;
   begin
      Rule := Lemon.Rule;
      while Rule /= null loop
         if Rule.Prec_Sym = null then
            for I in Rule.RHS.all'Range loop
               exit when Rule.Prec_Sym /= null;
               declare
                  Symbol : constant Symbol_Access := Rule.RHS (I);
               begin
                  if Symbol.Kind = Multi_Terminal then

                     for J in Symbol.Sub_Sym.First_Index .. Symbol.Sub_Sym.Last_Index loop
                        if Symbol.Sub_Sym (J).Prec >= 0 then
                           Rule.Prec_Sym := Rule_Symbol_Access (Symbol.Sub_Sym.Element (J));
                           exit;
                        end if;
                     end loop;

                  elsif Symbol.Prec >= 0 then
                     Rule.Prec_Sym := Rule_Symbol_Access (Rule.RHS (I));
                  end if;
               end;
            end loop;
         end if;
         Rule := Rule.Next;
      end loop;
   end Find_Rule_Precedences;


   procedure Find_First_Sets (Lemon : in out Lime.Lemon_Record)
   is
      use Rules;
      use Symbols;

      I_Copy   : Integer;
      Rule       : Rule_Access;
      Progress : Boolean;
   begin
      Symbols.Set_Lambda_False_And_Set_Firstset (First => Natural (Lemon.N_Terminal),
                                                 Last  => Natural (Lemon.N_Symbol - 1));
--        for I in 0 .. Lemon.N_Symbol - 1 loop
--           Lemon.Symbols (I).lambda := False;
--        end loop;

--        for I in Lemon.N_Terminal .. Lemon.N_Symbol - 1 loop
--           Lemon.Symbols (I).firstset := SetNew (void);
--        end loop;

      --  First compute all lambdas
      loop
         Progress := False;
         Rule := Lemon.Rule;
         loop
            exit when Rule = null;

            if Rule.LHS.Lambda then
               goto Continue;
            end if;

            for I in Rule.RHS'Range loop
               I_Copy := I;
               declare
                  Symbol : constant Symbol_Access := Rule.RHS (I);
               begin
                  pragma Assert (Symbol.Kind = Non_Terminal or Symbol.Lambda = False);
                  exit when Symbol.Lambda = False;
               end;
            end loop;

            if I_Copy = Rule.RHS'Last then
               Rule.LHS.Lambda := True;
               Progress := True;
            end if;

            Rule := Rule.Next;
         end loop;
         exit when not Progress;
         <<Continue>>
         null;
      end loop;

      --  Now compute all first sets
      loop
         declare
            S1 : Rule_Symbol_Access;
            S2 : Symbol_Access;
         begin
            Progress := False;
            Rule := Lemon.Rule;
            loop
               exit when Rule = null;
               S1 := Rule.LHS;

               for I in Rule.RHS'Range loop
                  S2 := Rule.RHS (I);

                  if S2.Kind = Terminal then
                     if Sets.Set_Add (S1.First_Set, Natural (S2.Index)) then
                        Progress := True;
                     end if;
                     exit;

                  elsif S2.Kind = Multi_Terminal then
                     for J in S2.Sub_Sym.First_Index .. S2.Sub_Sym.Last_Index loop
                        if
                          Sets.Set_Add (S1.First_Set,
                                        Natural (S2.Sub_Sym (J).Index))
                        then
                           Progress := True;
                        end if;
                     end loop;
                     exit;

                  elsif Symbol_Access (S1) = S2 then
                     exit when S1.Lambda = False;

                  else
                     if Sets.Set_Union (S1.First_Set, S2.First_Set) then
                        Progress := True;
                     end if;
                     exit when S2.Lambda = False;

                  end if;
               end loop;
               Rule := Rule.Next;
            end loop;
         end;
         exit when not Progress;

      end loop;
   end Find_First_Sets;


   procedure Find_States
     (Lemon : in out Lime.Lemon_Record)
   is
      use Ada.Strings.Unbounded;
      use Lemon_Bind;
      use Symbols;
      use Rules;

      Lemp   : Lime.Lemon_Record renames Lemon;
      Symbol : Symbol_Access;
      Rule : Rule_Access;
   begin
      Configlist_Init;

      --  Find the start symbol
      --  lime_partial_database_dump_c ();
      --  lime_partial_database_dump_ada ();

      if Lemp.Names.Start /= "" then
         Symbol := Find (To_String (Lemp.Names.Start));
         if Symbol = null then
            Errors.Error_Plain
              (File_Name   => Lemp.File_Name,
               Line_Number => 0,
               Text        =>
                 "The specified start symbol '%1' Start is not in a nonterminal " &
                 "of the grammar.  '%2' will be used as the start symbol instead.",
               Arguments   =>
                 (1 => Lemp.Names.Start,
                  2 => To_Unbounded_String (Name_Of (Symbol_Access (Lemp.Start_Rule.LHS))))
              );
            Lemp.Error_Cnt := Lemp.Error_Cnt + 1;
            Symbol := Symbol_Access (Lemp.Start_Rule.LHS);
         end if;
      else
         Symbol := Symbol_Access (Lemp.Start_Rule.LHS);
      end if;

      --  Make sure the start symbol doesn't occur on the right-hand side of
      --  any rule.  Report an error if it does.  (YACC would generate a new
      --  start symbol in this case.)
      Rule := Lemp.Rule;
      loop
         exit when Rule = null;
         for I in Rule.RHS'Range loop
            if Rule.RHS (I) = Symbol then   --  FIX ME:  Deal with multiterminals XXX
               Errors.Error_Plain
                 (File_Name   => Lemp.File_Name,
                  Line_Number => 0,
                  Text        =>
                    "The start symbol '%1' occurs on the right-hand " &
                    "side of a rule. This will result in a parser which " &
                    "does not work properly.",
                  Arguments   => (1 => To_Unbounded_String (Name_Of (Symbol)))
                 );
               Lemp.Error_Cnt := Lemp.Error_Cnt + 1;
            end if;
         end loop;
         Rule := Rule.Next;
      end loop;

      --  The basis configuration set for the first state
      --  is all rules which have the start symbol as their
      --  left-hand side
      Rule := Rule_Access (Symbol.Rule);
      loop
         exit when Rule = null;
         declare
            Dummy   : Boolean;
            New_CFP : Configs.Config_Access;
         begin
            Rule.LHS_Start := True;
            New_CFP := Configlist_Add_Basis (Rule, 0);
            Dummy := Sets.Set_Add (New_CFP.Follow_Set, 0);
         end;
         Rule := Rule.Next_LHS;
      end loop;

      --  Compute the first state.  All other states will be
      --  computed automatically during the computation of the first one.
      --  The returned pointer to the first state is not used. */
      Get_State (Lemp);

   end Find_States;


end Builds;
