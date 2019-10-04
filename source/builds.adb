--
--
--

with Ada.Text_IO;
with Ada.Strings.Unbounded;
with Ada.Containers;

with Reports;
with Options;
with Rules;
with Symbols;
with Symbols.IO;
with Sets;
with Errors;
with Configs;
with Actions;
with Action_Lists;
with Cherry;
with Config_Lists;
with Prop_Links;
with Debugs;

package body Builds is


   procedure Find_Rule_Precedences (Session : in out Sessions.Session_Type)
   is
      use Rules;
      use Symbols;

      Rule : Rule_Access;
   begin
      Rule := Session.Rule;
      while Rule /= null loop
         if Rule.Prec_Symbol = null then
            for Symbol of Rule.RHS loop
               exit when Rule.Prec_Symbol /= null;
               if Symbol.Kind = Multi_Terminal then

                  for J in Symbol.Sub_Symbol.First_Index .. Symbol.Sub_Symbol.Last_Index loop
                     if Symbol.Sub_Symbol (J).Precedence >= 0 then
                        Rule.Prec_Symbol := Rule_Symbol_Access (Symbol.Sub_Symbol.Element (J));
                        exit;
                     end if;
                  end loop;

               elsif Symbol.Precedence >= 0 then
                  Rule.Prec_Symbol := Symbol;
               end if;
            end loop;
         end if;
         Rule := Rule.Next;
      end loop;
   end Find_Rule_Precedences;


   procedure Find_First_Sets (Session : in out Sessions.Session_Type)
   is
      use Rules;
      use Symbols;

      Rule     : Rule_Access;
      Progress : Boolean;
   begin
      Symbols.Set_Lambda_False_And_Set_Firstset (First => Natural (Session.N_Terminal),
                                                 Last  => Natural (Session.N_Symbol - 1));
      --  First compute all lambdas
      loop
         Progress := False;
         Rule := Session.Rule;
         loop
            exit when Rule = null;

            if Rule.LHS.Lambda then
               goto Continue;
            end if;

            for Symbol of Rule.RHS loop
               pragma Assert (Symbol.Kind = Non_Terminal or Symbol.Lambda = False);
               goto Continue;
            end loop;

            Rule.LHS.Lambda := True;
            Progress := True;

            <<Continue>>
            Rule := Rule.Next;
         end loop;
         exit when not Progress;
      end loop;

      --  Now compute all first sets
      loop
         declare
            S1 : Rule_Symbol_Access;
            S2 : Symbol_Access;
         begin
            Progress := False;
            Rule := Session.Rule;
            loop
               exit when Rule = null;
               S1 := Rule.LHS;

               for Symbol of Rule.RHS loop
                  S2 := Symbol_Access (Symbol);

                  if S2.Kind = Terminal then
                     if Sets.Set_Add (S1.First_Set, Natural (S2.Index)) then
                        Progress := True;
                     end if;
                     exit;

                  elsif S2.Kind = Multi_Terminal then
                     for J in S2.Sub_Symbol.First_Index .. S2.Sub_Symbol.Last_Index loop
                        if
                          Sets.Set_Add (S1.First_Set,
                                        Natural (S2.Sub_Symbol (J).Index))
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
     (Session : in out Sessions.Session_Type)
   is
      use Ada.Strings.Unbounded;
      use Errors;
      use Symbols;
      use Rules;

      Symbol : Symbol_Access;
      Rule   : Rule_Access;
   begin
      Config_Lists.Init;

      --  Find the start symbol
      --  lime_partial_database_dump_c ();
      --  lime_partial_database_dump_ada ();

      if Session.Names.Start /= "" then
         Symbol := Find (To_String (Session.Names.Start));
         if Symbol = null then
            Errors.Parser_Error
              (E014, Line_Number => 0,
               Argument_1 => To_String (Session.Names.Start),
               Argument_2 => Name_Of (Symbol_Access (Session.Start_Rule.LHS)));
            Symbol := Symbol_Access (Session.Start_Rule.LHS);
         end if;
      else
         Symbol := Symbol_Access (Session.Start_Rule.LHS);
      end if;

      --  Make sure the start symbol doesn't occur on the right-hand side of
      --  any rule.  Report an error if it does.  (YACC would generate a new
      --  start symbol in this case.)
      Rule := Session.Rule;
      loop
         exit when Rule = null;
         for RHS_Symbol of Rule.RHS loop
            --  FIX ME:  Deal with multiterminals XXX
            if Symbol_Access (RHS_Symbol) = Symbol then
               Errors.Parser_Error (E015,
                                    Line_Number => 0,
                                    Argument_1  => Name_Of (Symbol));
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
            New_CFP := Config_Lists.Add_Basis (Rule, 0);
            Dummy := Sets.Set_Add (New_CFP.Follow_Set, 0);
         end;
         Rule := Rule.Next_LHS;
      end loop;

      --  Compute the first state.  All other states will be
      --  computed automatically during the computation of the first one.
      --  The returned pointer to the first state is not used.
      Get_First_State (Session);

   end Find_States;


   procedure Find_Actions (Session : in out Session_Type)
   is
      use Configs;
      use States;
      use Symbols;
      use Rules;
      use Actions;

      Config : Config_Access;
      Symbol : Symbol_Access;
      Rule   : Rule_Access;
   begin
      --  Add all of the reduce actions
      --  A reduce action is added for each element of the followset of
      --  a configuration which has its dot at the extreme right.

      --  Loop over all states
      for State of Session.Sorted loop
         Config := State.Config;
         while Config /= null loop  --  Loop over all configurations
            if Dot_Type (Config.Rule.RHS.Length) = Config.Dot then    --  Is dot at extreme right?
               for J in 0 .. Session.N_Terminal - 1 loop
                  if Sets.Set_Find (Config.Follow_Set, Integer (J)) then
                     --  Add a reduce action to the state "stp" which will reduce by the
                     --  rule "cfp->rp" if the lookahead symbol is "lemp->symbols[j]"
                     Action_Lists.Append (State.Action, Reduce,
                                          Element_At (J),
                                          State => null,
                                          Rule  => Config.Rule);
                  end if;
               end loop;
            end if;
            Config := Config.Next;
         end loop;
      end loop;

      --  Add the accepting token
      Cherry.Add_The_Accepting_Token (Session, Symbol);

      --  Add to the first state (which is always the starting state of the
      --  finite state machine) an action to ACCEPT if the lookahead is the
      --  start nonterminal.
      --      Action_Add (Extras.Sorted_At (Extras.Get_Extra, 0).Action,
      Action_Lists.Append (Session.Sorted (0).Action,
                           C_Accept, Symbol,
                           State => null,
                           Rule  => null);

      --  Resolve conflicts
      for State of Session.Sorted loop
         declare
            use Action_Lists.Action_DLLs;
--            Action      : Action_Access;
--            Next_Action : Action_Access;
            First_Action : Cursor;
            Next_Action  : Cursor;
         begin
            --  assert( stp->ap );
            Action_Lists.Sort (State.Action);
--            Action       := Action_Access (State.Action);
            First_Action := State.Action.First;
            while
              First_Action /= No_Element and then
              Next (First_Action) /= No_Element
            loop
--            while not (Action /= null and then Action.Next /= null) loop
               Next_Action := Next (First_Action);
               while
                 not (Next_Action /= No_Element and then
                        Element (Next_Action).Symbol = Element (First_Action).Symbol)
               loop
                  --  The two actions "ap" and "nap" have the same lookahead.
                  --  Figure out which one should be used
                  declare
                     First_Element  : Action_Record := Element (First_Action);
                     Next_Element : Action_Record := Element (Next_Action);
                  begin
                     Session.N_Conflict := Session.N_Conflict +
                       Resolve_Conflict (First_Element, Next_Element);
                     Replace_Element (State.Action, First_Action, First_Element);
                     Replace_Element (State.Action, Next_Action,  Next_Element);
                  end;
                  Next_Action := Next (Next_Action);
               end loop;
               First_Action := Next (First_Action);
            end loop;
         end;
      end loop;

      --  Report an error for each rule that can never be reduced.
      Rule := Session.Rule;
      while Rule /= null loop
         Rule.Can_Reduce := False;
         Rule := Rule.Next;
      end loop;

      for State of Session.Sorted loop
         for Action of State.Action loop
--         declare
--            Action : Action_Access;
--         begin
--            Action := Action_Access (Extras.Sorted_At (Extras.Get_Extra,
--                                                       Symbol_Index (I)).Action);
--            while Action /= null loop
               if Action.Kind = Reduce then
                  Action.X.Rule.Can_Reduce := True;
               end if;
--               Action := Action.Next;
         end loop;
         --  end;
      end loop;

      Rule := Session.Rule;
      while Rule /= null loop
         if not Rule.Can_Reduce then
            Errors.Parser_Error (Errors.E301, Line_Number => Rule.Rule_Line);
         end if;
         Rule := Rule.Next;
      end loop;
   end Find_Actions;


   function Get_State (Session : in out Sessions.Session_Type)
                      return States.State_Access
   is
      use Configs;
      use States;
      use type Sessions.State_Index;

      Config : Config_Access;
      Basis  : Config_Access;
      State  : State_Access;
   begin
      --  Extract the sorted basis of the new state.  The basis was constructed
      --  by prior calls to "Configlist_addbasis()".
      Config_Lists.Sort_Basis;
      Basis := Config_Lists.Basis;

      --  Get a state with the same basis
      State := States.Find (Basis);
      if State /= null then
         --  A state with the same basis already exists!  Copy all the follow-set
         --  propagation links from the state under construction into the
         --  preexisting state, then return a pointer to the preexisting state
         declare
            X, Y : Config_Access;
         begin
            X := Basis;
            Y := State.Basis;
            while X /= null and Y /= null loop
               Prop_Links.Copy (Y.Backward_PL, X.Backward_PL);
               Prop_Links.Delete (X.Forward_PL);
               X.Forward_PL.Clear;
               X.Backward_PL.Clear;
               X := X.Basis;
               Y := Y.Basis;
            end loop;
         end;
         Config := Config_Lists.Xreturn;
         Config_Lists.Eat (Config);
      else
         --  This really is a new state.  Construct all the details
         Config_Lists.Closure (Session);  -- Compute the configuration closure
         Config_Lists.Sort;               -- Sort the configuration closure
         Config := Config_Lists.Xreturn;  -- Get a pointer to the config list
         State  := States.Create;         -- A new state structure
                                          --  MemoryCheck(stp);
         State.Basis  := Basis;               -- Remember the configuration basis
         State.Config := Config;              -- Remember the configuration closure
         --  XXX
         --  State.State_Num := Natural (Session.N_State);  -- Every state gets a sequence number
         State.State_Num := Natural (Session.Sorted.Length);  -- Every state gets a sequence number

         --  Session.N_State := Session.N_State + 1;
         State.Action.Clear;                  -- No actions, yet.
         Debugs.Debug (True, "States.Insert");
         States.Insert (State, State.Basis);  -- Add to the state table
         Build_Shifts (Session, State.all);   -- Recursively compute successor states
      end if;
      return State;
   end Get_State;


   procedure Get_First_State (Session : in out Sessions.Session_Type)
   is
      Dummy : States.State_Access;
      pragma Unreferenced (Dummy);
   begin
      Dummy := Get_State (Session);
   end Get_First_State;


   function Same_Symbol (Left, Right : in Symbols.Symbol_Access) return Boolean;
   --  Return true when two symbols are the same.

   function Same_Symbol (Left, Right : in Symbols.Symbol_Access) return Boolean
   is
      use Symbols;
      use type Ada.Containers.Count_Type;
   begin
      if Left = Right then
         return True;
      end if;

      if Left.Kind /= Multi_Terminal then
         return False;
      end if;

      if Right.Kind /= Multi_Terminal then
         return False;
      end if;

      if Left.Sub_Symbol.Length /= Right.Sub_Symbol.Length then
         return False;
      end if;

      for Index in Left.Sub_Symbol.First_Index .. Left.Sub_Symbol.Last_Index loop
         if Left.Sub_Symbol (Index) /= Right.Sub_Symbol (Index) then
            return False;
         end if;
      end loop;

      return True;
   end Same_Symbol;


   procedure Build_Shifts (Session : in out Session_Type;
                           State   : in out States.State_Record)
   is
      use type Rules.Dot_Type;
      use Configs;
      use Symbols;
      use States;

      Config     : Config_Access;  --  For looping thru the config closure of "stp"
      B_Config   : Config_Access;  --  For the inner loop on config closure of "stp"
      New_Config : Config_Access;  --
      Symbol     : Symbol_Access;  --  Symbol following the dot in configuration "cfp"
      B_Symbol   : Symbol_Access;  --  Symbol following the dot in configuration "bcfp"
      New_State  : State_Access;   --  A pointer to a successor state
   begin
      --  Each configuration becomes complete after it contibutes to a successor
      --  state.  Initially, all configurations are incomplete
      Config := State.Config;
      while Config /= null loop
         Config.Status := Incomplete;
         Config := Config.Next;
      end loop;

      --  Loop through all configurations of the state "stp"
      Config := State.Config;
      while Config /= null loop
         if Config.Status = Complete then
            goto Continue;    -- Already used by inner loop
         end if;
         if Config.Dot >= Rules.Dot_Type (Config.Rule.RHS.Length) then
            goto Continue;  -- Can't shift this config
         end if;

         Config_Lists.Reset;                       -- Reset the new config set
         Symbol := Symbol_Access (Config.Rule.RHS.Element (Config.Dot)); -- Symbol after the dot

         --  For every configuration in the state "stp" which has the symbol "sp"
         --  following its dot, add the same configuration to the basis set under
         --  construction but with the dot shifted one symbol to the right.
         B_Config := Config;
         while B_Config /= null loop
            if B_Config.Status = Complete then
               goto Continue_Config;    --  Already used
            end if;
            if B_Config.Dot >= Rules.Dot_Type (B_Config.Rule.RHS.Length) then
               goto Continue_Config; --  Can't shift this one
            end if;
            --  Get symbol after dot
            B_Symbol := Symbol_Access (B_Config.Rule.RHS.Element (B_Config.Dot));
            if not Same_Symbol (B_Symbol, Symbol) then
               goto Continue_Config;                      --  Must be same as for "cfp"
            end if;
            B_Config.Status := Complete;                   --  Mark this config as used
            New_Config := Config_Lists.Add_Basis (B_Config.Rule, B_Config.Dot + 1);
            Prop_Links.Append (New_Config.Backward_PL, B_Config);

            <<Continue_Config>>
            B_Config := B_Config.Next;
         end loop;

         --  Get a pointer to the state described by the basis configuration set
         --  constructed in the preceding loop
         New_State := Get_State (Session);

         --  The state "newstp" is reached from the state "stp" by a shift action
         --  on the symbol "sp"
         if Symbol.Kind = Multi_Terminal then
            for Sub_Symbol of Symbol.Sub_Symbol loop
               Action_Lists.Append (State.Action, Actions.Shift, Sub_Symbol,
                                    State => New_State, Rule => null);
            end loop;
         else
            Action_Lists.Append (State.Action, Actions.Shift, Symbol,
                                 State => New_State, Rule => null);
         end if;

         <<Continue>>
         Config := Config.Next;
      end loop;
   end Build_Shifts;


   procedure Find_Links (Session : in out Session_Type)
   is
      use Configs;
      use States;
      use type Sessions.State_Index;

      Config : Config_Access;
      Other  : Config_Access;
   begin
      --  Housekeeping detail:
      --  Add to every propagate link a pointer back to the state to
      --  which the link is attached.
      for State of Session.Sorted loop
         Debugs.Debug (True, "Sessions.Sorted'Length: " & Session.Sorted.Length'Image);
         Config := State.Config;
         while Config /= null loop
            Config.State := State;
            Config       := Config.Next;
         end loop;
      end loop;

      --  Convert all backlinks into forward links.  Only the forward
      --  links are used in the follow-set computation.
      for State of Session.Sorted loop
         Config := State.Config;
         while Config /= null loop
            for Link of Config.Backward_PL loop
               Other := Config_Access (Link);
               Prop_Links.Append (Other.Forward_PL, Config);
            end loop;
            Config := Config.Next;
         end loop;
      end loop;
   end Find_Links;


   procedure Find_Follow_Sets (Session : in out Sessions.Session_Type)
   is
      use Configs;
      use type Sessions.State_Index;

      Config   : Config_Access;
      Progress : Boolean;
      Change   : Boolean;
   begin


      for State of Session.Sorted loop
         Config := State.Config;
         loop
            exit when Config = null;
            Config.Status := Incomplete;
            Config := Config.Next;
         end loop;
      end loop;

      loop
         Progress := False;
         for State of Session.Sorted loop
            Config := State.Config;
            loop
               exit when Config = null;

               if Config.Status = Complete then
                  goto Continue;
               end if;

               for Link of Config.Forward_PL loop
--        for(plp=cfp->fplp; plp; plp=plp->next){ loop
                  Change := Sets.Set_Union (Link.Follow_Set,
                                            Config.Follow_Set);
                  if Change then
                     Link.Status := Incomplete;
                     Progress := True;
                  end if;
               end loop;
               Config.Status := Complete;

               <<Continue>>
               Config := Config.Next;
            end loop;
         end loop;
         exit when not Progress;
      end loop;
   end Find_Follow_Sets;


   procedure Reprint_Of_Grammar
     (Session       : in out Sessions.Session_Type;
      Base_Name     : in     String;
      Token_Prefix  : in     String;
      Terminal_Last : in     Natural)
   is
      use Ada.Text_IO;
   begin
--      if Options.RP_Flag then
--         Reports.Reprint (Session);
--      else

         --  Initialize the size for all follow and first sets
         Sets.Set_Size (Terminal_Last + 1);

         --  Find the precedence for every production rule (that has one)
         Builds.Find_Rule_Precedences (Session);
         Ada.Text_IO.Put_Line ("16 dump_symbols");
         Symbols.IO.JQ_Dump_Symbols (Session, Mode => 1);

         --  Compute the lambda-nonterminals and the first-sets for every
         --  nonterminal
         Builds.Find_First_Sets (Session);

         Ada.Text_IO.Put_Line ("17 dump_symbols");
         Symbols.IO.JQ_Dump_Symbols (Session, Mode => 1);

         Ada.Text_IO.Put_Line ("17 dump_rules");
         Debugs.JQ_Dump_Rules (Session, Mode => 1);

         --  Compute all LR(0) states.  Also record follow-set propagation
         --  links so that the follow-set can be computed later
--         Compute_LR_States (Session);
         Put_Line ("### 2-5");
         --  XXX
         --  Session.N_State := 0;
         Builds.Find_States (Session);
         Put_Line ("### 2-5-2");
         Session.Sorted := Sessions.Create_Sorted_States; --  State_Arrayof;

         --  Tie up loose ends on the propagation links
         Builds.Find_Links (Session);
         Put_Line ("### 2-6");
         --  Compute the follow set of every reducible configuration
         Builds.Find_Follow_Sets (Session);
         Put_Line ("### 2-7");
         --  Compute the action tables
         Builds.Find_Actions (Session);
         Put_Line ("### 2-8");
         --  Compress the action tables
         if not Options.Compress then
            Reports.Compress_Tables (Session);
         end if;
         Put_Line ("### 2-9");
         --  Reorder and renumber the states so that states with fewer choices
         --  occur at the end.  This is an optimization that helps make the
         --  generated parser tables smaller.
         if not Options.No_Resort then
            Reports.Resort_States (Session);
         end if;
         Put_Line ("### 2-10");
         --   Generate a report of the parser generated.  (the "y.output" file)
         if not Options.Be_Quiet then
            Reports.Report_Output (Session);
         end if;

         --  Generate the source code for the parser
         Reports.Report_Table
           (Session,
           User_Template_Name => Options.User_Template.all);

         --  Produce a header file for use by the scanner.  (This step is
         --  omitted if the "-m" option is used because makeheaders will
         --  generate the file for us.)
         Reports.Report_Header
           (Session,
            Token_Prefix,
            Base_Name, -- File_Makename (Session, ""),
            "MODULE XXX",
            Terminal_Last);
--      end if;
   end Reprint_Of_Grammar;


end Builds;
