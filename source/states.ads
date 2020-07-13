--
--  The author disclaims copyright to this source code.  In place of
--  a legal notice, here is a blessing:
--
--    May you do good and not evil.
--    May you find forgiveness for yourself and forgive others.
--    May you share freely, not taking more than you give.
--

with Types;
with Configs;
with Action_Lists;
with Action_Tables;
limited with Rules;

package States is

   subtype Action_Value is Action_Tables.Action_Value;
   subtype Offset_Type  is Types.Offset_Type;

   type State_Boolean is (Syntax_Error, False, True);
   --  Syntax_Error is used to trigger syntax error !!!

   type State_Number is new Integer;
   --  Identification number for state.

   --  Each state of the generated parser's finite state machine
   --  is encoded as an instance of the following structure.

   type State_Record is record

      Basis : Configs.Config_Access;
      --  The basis configurations for this state

      Config : Configs.Config_Access;
      --  All configurations in this set

      Number : State_Number;
      --  Sequential number for this state

      Action : Action_Lists.List;
      --  List of actions for this state

      Num_Token       : aliased Action_Value;
      Num_Nonterminal : aliased Action_Value;
      --  Number of actions on terminals and nonterminals

      Token_Offset   : Offset_Type;
      Nonterm_Offset : Offset_Type;
      --  yy_action[] offset for terminals and nonterms

      Default_Reduce : State_Boolean;
      --  Default action is to REDUCE by this rule

      Default_Reduce_Rule : access Rules.Rule_Record;
      --  The default REDUCE rule.

      Auto_Reduce : Boolean;
      --  True if this is an auto-reduce state
   end record;

   type State_Access is access all State_Record;

   procedure Initialize;
   --  Initialize states

   function Find (Config : in not null Configs.Config_Access)
                 return State_Access;
   --  Find state from Config

   function Create return State_Access;
   --  Create state

   procedure Insert (State  : in State_Access;
                     Config : in Configs.Config_Access);
   --  Insert state

   type Process_Access is access procedure (State : in State_Access);
   procedure Iterate (Process : in Process_Access);

end States;
