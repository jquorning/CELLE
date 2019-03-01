--
--  The author disclaims copyright to this source code.  In place of
--  a legal notice, here is a blessing:
--
--    May you do good and not evil.
--    May you find forgiveness for yourself and forgive others.
--    May you share freely, not taking more than you give.
--

with Configs;
limited with Actions;
limited with Rules;

package States is

   --  Each state of the generated parser's finite state machine
   --  is encoded as an instance of the following structure.

   type State_Record is record
      BP           : access Configs.Config_Record;  --  The basis configurations for this state
      CFP          : access Configs.Config_Record;  --  All configurations in this set
      State_Num    : aliased Integer;               --  Sequential number for this state
      AP           : access Actions.Action_Record;  --  List of actions for this state
      N_Tkn_Act    : aliased Integer;
      --  Number of actions on terminals and nonterminals

      N_Nt_Act     : aliased Integer;
      --  yy_action[] offset for terminals and nonterms

      Token_Offset : aliased Integer;       --  Default action is to REDUCE by this rule
      iNtOfst      : aliased Integer;       --  The default REDUCE rule.
      iDfltReduce  : Boolean;               --  True if this is an auto-reduce state
      pDfltReduce  : access Rules.Rule_Record;
      autoReduce   : aliased Integer;
   end record;

   type State_Access is access all State_Record;

end States;
