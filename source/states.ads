--
--  The author disclaims copyright to this source code.  In place of
--  a legal notice, here is a blessing:
--
--    May you do good and not evil.
--    May you find forgiveness for yourself and forgive others.
--    May you share freely, not taking more than you give.
--

with Configs;
with Action_Lists;
limited with Rules;

package States is

   --  Each state of the generated parser's finite state machine
   --  is encoded as an instance of the following structure.
--   type State_Action_Access is access all Actions.Action_Record;
   type State_Record is record
      Basis        : Configs.Config_Access;    --  The basis configurations for this state
      Config       : Configs.Config_Access;    --  All configurations in this set
      State_Num    : Integer;                  --  Sequential number for this state
      Action       : Action_Lists.List; -- State_Action_Access; --  List of actions for this state
      N_Tkn_Act    : Integer;
      --  Number of actions on terminals and nonterminals

      N_Nt_Act     : aliased Integer;
      --  yy_action[] offset for terminals and nonterms

      Token_Offset : Integer;               --  Default action is to REDUCE by this rule
      iNtOfst      : Integer;               --  The default REDUCE rule.
      iDfltReduce  : Boolean;               --  True if this is an auto-reduce state
      pDfltReduce  : access Rules.Rule_Record;
      autoReduce   : Integer;
   end record;

   type State_Access is access all State_Record;

   procedure Initialize;
   --  Initialize states

   function Find (Config : in not null Configs.Config_Access) return State_Access;
   --  Find state from Config

   function Create return State_Access;
   --  Create state

   procedure Insert (State  : in State_Access;
                     Config : in Configs.Config_Access);
   --  Insert state

   type Process_Access is access procedure (State : in State_Access);
   procedure Iterate (Process : in Process_Access);

--  private

--   pragma Import (C, Initialize, "State_init");
--   pragma Import (C, Find,       "State_find");
--   pragma Import (C, Create,     "State_new");
--   pragma Import (C, Insert,     "State_insert");

end States;
