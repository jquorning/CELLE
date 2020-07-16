--
--  The author disclaims copyright to this source code.  In place of
--  a legal notice, here is a blessing:
--
--    May you do good and not evil.
--    May you find forgiveness for yourself and forgive others.
--    May you share freely, not taking more than you give.
--

with Ada.Text_IO;
with Ada.Strings.Unbounded;
with Ada.Containers;

with Types;
with Reports;
with Options;
with Rules;
with Symbols.IO;
with Symbol_Sets;
with Errors;
with Configs;
with Actions;
with Action_Lists;
with Config_Lists;
with Prop_Links;
with Debugs;

package body Builds is


   procedure Find_Rule_Precedences (Session : in out Sessions.Session_Type)
   is
      use Rules;
      use Symbols;
   begin
      for Rule of Session.Rule loop
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
      end loop;
   end Find_Rule_Precedences;


   procedure Find_First_Sets (Session : in out Sessions.Session_Type)
   is
      use Rules;
      use Symbols;
      use Types;

      Progress : Boolean;
   begin
      Symbols.Set_Lambda_False_And_Set_Firstset
        (First => Natural (Session.Num_Terminal),
         Last  => Natural (Session.Num_Symbol - 1));

      --  First compute all lambdas

      loop
         Progress := False;
         for Rule of Session.Rule loop

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
            null;
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
            for Rule of Session.Rule loop
               S1 := Rule.LHS;

               for Symbol of Rule.RHS loop
                  S2 := Symbol_Access (Symbol);

                  if S2.Kind = Terminal then
                     if Symbol_Sets.Set_Add (S1.First_Set, S2.Index) then
                        Progress := True;
                     end if;
                     exit;

                  elsif S2.Kind = Multi_Terminal then
                     for J in S2.Sub_Symbol.First_Index .. S2.Sub_Symbol.Last_Index loop
                        if
                          Symbol_Sets.Set_Add (S1.First_Set,
                                               S2.Sub_Symbol (J).Index)
                        then
                           Progress := True;
                        end if;
                     end loop;
                     exit;

                  elsif Symbol_Access (S1) = S2 then
                     exit when S1.Lambda = False;

                  else
                     if Symbol_Sets.Set_Union (S1.First_Set, S2.First_Set) then
                        Progress := True;
                     end if;
                     exit when S2.Lambda = False;

                  end if;
               end loop;
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
      use Rules.Rule_Lists;

      Start_Symbol      : Symbol_Access;
      Rule              : Rule_Access;
      Dummy_First_State : States.State_Access;
   begin
      Config_Lists.Init;

      --  Find the start symbol

      if Session.Names.Start = "" then
         Start_Symbol := Symbol_Access (Element (Session.Start_Rule).LHS);
      else
         Start_Symbol := Find (To_String (Session.Names.Start));
         if Start_Symbol = null then
            Errors.Parser_Error
              (E014, Line => 0,
               Argument_1 => To_String (Session.Names.Start),
               Argument_2 => Name_Of (Symbol_Access (Element (Session.Start_Rule).LHS)));
            Start_Symbol := Symbol_Access (Element (Session.Start_Rule).LHS);
         end if;
      end if;

      --  Make sure the start symbol doesn't occur on the right-hand side of
      --  any rule.  Report an error if it does.  (YACC would generate a new
      --  start symbol in this case.)

      for Rule of Session.Rule loop
         for RHS_Symbol of Rule.RHS loop
            --  FIX ME:  Deal with multiterminals XXX
            if Symbol_Access (RHS_Symbol) = Start_Symbol then
               Errors.Parser_Error (E015, Line => 0,
                                    Argument_1 => Name_Of (Start_Symbol));
            end if;
         end loop;
      end loop;

      --  The basis configuration set for the first state
      --  is all rules which have the start symbol as their
      --  left-hand side

      Rule := Rule_Access (Start_Symbol.Rule);
      while Rule /= null loop
         declare
            Dummy  : Boolean;
            Config : Configs.Config_Access;
         begin
            Rule.LHS_Start := True;
            Config := Config_Lists.Add_Basis (Rule, Dot => 0);
            Dummy  := Symbol_Sets.Set_Add (Config.Follow_Set, 0);
         end;
         Rule := Rule.Next_LHS;
      end loop;

      --  Compute the first state.  All other states will be
      --  computed automatically during the computation of the first one.
      --  The returned pointer to the first state is not used.

      Dummy_First_State := Get_State (Session);
      Debugs.Debug (True, "Get_State first");
   end Find_States;


   procedure Find_Actions (Session : in out Sessions.Session_Type)
   is
      use Configs;
      use States;
      use Symbols;
      use Rules;
      use Actions;
      use type Types.Symbol_Index;

      Config : Config_Access;
      Symbol : Symbol_Access;
   begin

      --  Add all of the reduce actions
      --  A reduce action is added for each element of the followset of
      --  a configuration which has its dot at the extreme right.

      --  Loop over all states

      for State of Session.Sorted loop

         --  Loop over all configurations
         Config := State.Config;
         while Config /= null loop

            --  Is dot at extreme right?
            if Dot_Type (Config.Rule.RHS.Length) = Config.Dot then
               for J in 0 .. Session.Num_Terminal - 1 loop
                  if Symbol_Sets.Set_Find (Config.Follow_Set, J) then
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
      Add_The_Accepting_Token (Session, Symbol);

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
                     Session.Num_Conflict := Session.Num_Conflict +
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

      for Rule of Session.Rule loop
         Rule.Can_Reduce := False;
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
         --  end;
         end loop;
      end loop;

      for Rule of Session.Rule loop
         if not Rule.Can_Reduce then
            Errors.Parser_Error (Errors.E301, Line => Rule.Rule_Line);
         end if;
      end loop;

   end Find_Actions;


   function Get_State (Session : in out Sessions.Session_Type)
                      return States.State_Access
   is
      use Configs;
      use States;
--      use type Sessions.State_Index;

      Config : Config_Access;
      Basis  : Config_Access;
      State  : State_Access;
   begin

      --  Extract the sorted basis of the new state.  The basis was constructed
      --  by prior calls to "Config_lists.Add_Basis".

      Config_Lists.Sort_Basis;
      Basis := Config_Lists.Basis;

      --  Get a state with the same basis
      State := States.Find (Basis);
      if State /= null then
         Debugs.Debug (True, "This is not a new state");
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
         Debugs.Debug (True, "This is a new state");
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

         State.Number := State_Number (Session.Sorted.Length);
         --  Every state gets a sequence number

         --  Session.N_State := Session.N_State + 1;
         State.Action.Clear;                  -- No actions, yet.
         Debugs.Debug (True, "States.Insert");
         States.Insert (State, State.Basis);  -- Add to the state table
         Build_Shifts (Session, State.all);   -- Recursively compute successor states
      end if;
      return State;
   end Get_State;


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


   procedure Build_Shifts (Session : in out Sessions.Session_Type;
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

         --  Already used by inner loop
         --  Can't shift this config
         if
           Config.Status = Incomplete and
           Config.Dot    < Config.Rule.RHS.Last_Index
         then

            Config_Lists.Reset;  -- Reset the new config set
            Symbol := Symbol_Access (Config.Rule.RHS.Element (Config.Dot)); -- Symbol after the dot

            --  For every configuration in the state "stp" which has the symbol "sp"
            --  following its dot, add the same configuration to the basis set under
            --  construction but with the dot shifted one symbol to the right.

            B_Config := Config;
            while B_Config /= null loop

               if
                 B_Config.Status = Incomplete and
                 B_Config.Dot    < B_Config.Rule.RHS.Last_Index
               then
                  --  Get symbol after dot
                  --  Must be same as for "cfp"
                  B_Symbol := Symbol_Access (B_Config.Rule.RHS.Element (B_Config.Dot));
                  if Same_Symbol (B_Symbol, Symbol) then

                     B_Config.Status := Complete;  --  Mark this config as used
                     New_Config := Config_Lists.Add_Basis (B_Config.Rule, B_Config.Dot + 1);
                     Prop_Links.Append (New_Config.Backward_PL, B_Config);
                  end if;
               end if;

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

         end if;

         Config := Config.Next;
      end loop;
   end Build_Shifts;


   procedure Find_Links (Session : in out Sessions.Session_Type)
   is
      use Configs;
      use States;
--      use type Sessions.State_Index;

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
--      use type Sessions.State_Index;

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
                  Change := Symbol_Sets.Set_Union (Link.Follow_Set,
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
      Symbol_Sets.Set_Range (First => Types.Symbol_Index'First,
                             Last  => Types.Symbol_Index (Terminal_Last + 1));

      --  Find the precedence for every production rule (that has one)
      Builds.Find_Rule_Precedences (Session);
      if Options.Emit_Debug_Info then
         Ada.Text_IO.Put_Line ("16 dump_symbols");
         Symbols.IO.JQ_Dump_Symbols (Session, Mode => 1);
      end if;

      --  Compute the lambda-nonterminals and the first-sets for every
      --  nonterminal
      Builds.Find_First_Sets (Session);

      if Options.Emit_Debug_Info then
         Ada.Text_IO.Put_Line ("17 dump_symbols");
         Symbols.IO.JQ_Dump_Symbols (Session, Mode => 1);
      end if;
--        Ada.Text_IO.Put_Line ("17 dump_rules");
--        Debugs.JQ_Dump_Rules (Session, Mode => 1);
      if Options.Emit_Debug_Info then
         Ada.Text_IO.Put_Line ("17 dump_states");
         Debugs.Put_States (Session, Mode => 1);
      end if;

         --  Compute all LR(0) states.  Also record follow-set propagation
         --  links so that the follow-set can be computed later

      Put_Line ("### 2-5");
      --  XXX
      --  Session.N_State := 0;
      Builds.Find_States (Session);
      Put_Line ("### 2-5-2");
--      Debugs.Put_States (Session, Mode => 1);
      Sessions.Create_Sorted_States (Session);
      Put_Line ("2-5-2 dump_states");
      Debugs.Put_States (Session, Mode => 1);

      --
      --  Tie up loose ends on the propagation links
      --

      Builds.Find_Links (Session);
      Put_Line ("### 2-6");

      --
      --  Compute the follow set of every reducible configuration
      --

      Builds.Find_Follow_Sets (Session);
      Put_Line ("### 2-7");

--        Put_Line ("2-7 dump_symbols");
--        Symbols.IO.JQ_Dump_Symbols (Session, Mode => 2);
--        Put_Line ("2-7 dump_rules");
--        Debugs.JQ_Dump_Rules (Session, Mode => 1);
      Put_Line ("2-7 dump_states");
      Debugs.Put_States (Session, Mode => 1);

      --
      --  Compute the action tables
      --
      Builds.Find_Actions (Session);
      Put_Line ("### 2-8");

      --
      --  Compress the action tables
      --

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
         Make_Headers => Options.Make_Headers,
         Generate_SQL => Options.Generate_SQL,
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


   procedure Add_The_Accepting_Token
     (Session : in out Sessions.Session_Type;
      Symbol  : in out Symbols.Symbol_Access)
   is
      use Ada.Strings.Unbounded;
      use Symbols;
      use Rules.Rule_Lists;
   begin
      if Session.Names.Start = "" then
         Symbol := Symbol_Access (Element (Session.Start_Rule).LHS);
      else
         Symbol := Find (To_String (Session.Names.Start));
         if Symbol = null then
            Symbol := Symbol_Access (Element (Session.Start_Rule).LHS);
         end if;
      end if;
   end Add_The_Accepting_Token;


end Builds;
