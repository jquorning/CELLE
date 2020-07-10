--
--  The author disclaims copyright to this source code.  In place of
--  a legal notice, here is a blessing:
--
--    May you do good and not evil.
--    May you find forgiveness for yourself and forgive others.
--    May you share freely, not taking more than you give.
--
---------------------------------------------------------------------------
--
--  This module implements routines use to construct the yy_action[] table.
--
--  The state of the yy_action table under construction is an instance of
--  the following structure.
--
--  The yy_action table maps the pair (state_number, lookahead) into an
--  action_number.  The table is an array of integers pairs.  The state_number
--  determines an initial offset into the yy_action array.  The lookahead
--  value is then added to this initial offset to get an index X into the
--  yy_action array. If the aAction[X].lookahead equals the value of the
--  of the lookahead input, then the value of the action_number output is
--  aAction[X].action.  If the lookaheads do not match then the
--  default action for the state_number is returned.
--
--  All actions associated with a single state_number are first entered
--  into aLookahead[] using multiple calls to acttab_action().  Then the
--  actions for that single state_number are placed into the aAction[]
--  array with a single call to acttab_insert().  The acttab_insert() call
--  also resets the aLookahead[] array in preparation for the next
--  state number.
--

limited with States;
with Rules;
with Symbols;

package Actions is

   type Action_Kind is
     (Shift,
      C_Accept,
      Reduce,
      Error,
      SS_Conflict,   --  A shift/shift conflict
      SR_Conflict,   --  Was a reduce, but part of a conflict
      RR_Conflict,   --  Was a reduce, but part of a conflict
      SH_Resolved,   --  Was a shift.  Precedence resolved conflict
      RD_Resolved,   --  Was reduce.  Precedence resolved conflict
      Not_Used,      --  Deleted by compression
      Shift_Reduce   --  Shift first, then reduce
     );

   type Action_Record;
   type Action_Access is access all Action_Record;

   type State_Rule_Kind is (Is_State, Is_Rule);
   type X_Union (discr : State_Rule_Kind := Is_State) is record
      case discr is
         when Is_State  =>
            stp  : access States.State_Record;  --  The look-ahead symbol
         when Is_Rule   =>
            Rule : access Rules.Rule_Record;    --  The new state, if a shift
      end case;
   end record;

   pragma Unchecked_Union (X_Union);

   --  Every shift or reduce operation is stored as one of the following
   type Action_Record is
      record
         Symbol  : access Symbols.Symbol_Record;
         Kind    : Action_Kind;
         X       : X_Union;                     --  The rule, if a reduce
         spOpt   : access Symbols.Symbol_Kind;  --  SHIFTREDUCE optimization to this symbol
--         Next    : access Action_Record;        --  Next action for this state
         Collide : access Action_Record;        --  Next action with the same hash
      end record;

   type Lookahead_Action is
      record
         Lookahead : Integer;        --  Value of the lookahead token
         Action    : Integer;        --  Action to take on the given lookahead
      end record;

   type A_Lookahead_Action is access all Lookahead_Action;

   type LA_Action_Array is array (Natural range <>) of Lookahead_Action;
   type A_LA_Actions is access all LA_Action_Array;

   package Tables is

      type Table_Type is
         record
            N_Action : Integer;
            --  Number of used slots in aAction[]

            N_Action_Alloc : Integer;
            --  Slots allocated for aAction[]

            Action : A_LA_Actions;
            --  The yy_action[] table under construction

            Lookahead : A_LA_Actions;
            --  A single new transaction set

            Mn_Lookahead : Integer;
            --  Minimum aLookahead[].lookahead

            Mn_Action : Integer;
            --  Action associated with mnLookahead

            Mx_Lookahead : Integer;
            --  Maximum aLookahead[].lookahead

            N_Lookahead : Integer;
            --  Used slots in aLookahead[]

            N_Looskahead_Alloc : Integer;
            --  Slots allocated in aLookahead[]

            N_Terminal : Integer;
            --  Number of terminal symbols

            N_Symbol : Integer;
            --  Total number of symbols
         end record;

      type Table_Access is access all Table_Type;

      function Lookahead_Size (P : in Table_Type) return Integer;
      --  Return the number of entries in the yy_action table

      function Alloc (N_Symbol   : in Integer;
                      N_Terminal : in Integer) return Table_Access;
      --  Allocate a new acttab structure

      function Action_Size (P : in Table_Type) return Integer;
      --  Return the size of the action table without the trailing syntax error
      --  entries.

   end Tables;

   function Action_Cmp (Left, Right : in Action_Record)
                       return Boolean;
   --  Compare two actions for sorting purposes.  Return negative, zero, or
   --  positive if the first action is less than, equal to, or greater than
   --  the first
   --  Return True when Left is 'less than' Right.

--   function Action_Sort (Action : in Action_Access) return Action_Access;

--     --  Sort parser actions

--  --   function Action_New return Action_Access;
--     --  Allocate a new parser action

--     procedure Action_Add (Action : in out Action_Access;
--                           Kind   : in     Action_Kind;
--                           Symbol : in     Symbols.Symbol_Access;
--                           State  : in     States.State_Access;
--                           Rule   : in     Rules.Rule_Access);
--  See Action_lists.Append

   function Resolve_Conflict (Left  : in out Action_Record;
                              Right : in out Action_Record) return Integer;
   --  Resolve a conflict between the two given actions.  If the
   --  conflict can't be resolved, return non-zero.
   --
   --  NO LONGER TRUE:
   --   To resolve a conflict, first look to see if either action
   --   is on an error rule.  In that case, take the action which
   --   is not associated with the error rule.  If neither or both
   --   actions are associated with an error rule, then try to
   --   use precedence to resolve the conflict.
   --
   --  If either action is a SHIFT, then it must be apx.  This
   --  function won't work if apx->type==REDUCE and apy->type==SHIFT.

end Actions;
