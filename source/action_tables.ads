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

with Actions;

package Action_Tables is

--   subtype Lookahead_Type is Actions.Lookahead_Type;
   type Lookahead_Type  is new Integer;
   subtype Action_Value is Actions.Action_Value;

   type Lookahead_Action is
      record
         Lookahead : Lookahead_Type;
         --  Value of the lookahead token

         Action : Action_Value;
         --  Action to take on the given lookahead
      end record;

   type Action_Array
      is array (Natural range <>) of Lookahead_Action;

   type Action_Array_Access
      is access all Action_Array;

   type Table_Type is
      record
         Num_Action : Natural;
         --  Number of used slots in aAction[]

--            Num_Action_Alloc : Natural;
         --  Slots allocated for aAction[]

         Action : Action_Array_Access;
         --  The yy_action[] table under construction

         Lookahead : Action_Array_Access;
         --  A single new transaction set

         Min_Lookahead : Lookahead_Type;
         --  Minimum aLookahead[].lookahead

         Min_Action : Action_Value;
         --  Action associated with mnLookahead

         Max_Lookahead : Lookahead_Type;
         --  Maximum aLookahead[].lookahead

         Num_Lookahead : Natural;
         --  Used slots in aLookahead[]

--            Num_Lookahead_Alloc : Natural;
         --  Slots allocated in aLookahead[]

         Num_Terminal : Natural;
         --  Number of terminal symbols

         Num_Symbol : Natural;
         --  Total number of symbols

      end record;

   type Table_Access is access all Table_Type;

   function Lookahead_Size (Table : in Table_Type) return Integer;
   --  Return the number of entries in the yy_action table

   function Alloc (N_Symbol   : in Integer;
                   N_Terminal : in Integer) return Table_Access;
   --  Allocate a new acttab structure

   function Action_Size (Table : in Table_Type) return Integer;
   --  Return the size of the action table without the trailing
   --  syntax error entries.

   procedure Acttab_Action (Table     : in out Table_Type;
                            Lookahead :        Lookahead_Type;
                            Action    :        Action_Value);
   --  Add a new action to the current transaction set.
   --
   --  This routine is called once for each lookahead for a particular
   --  state.

   procedure Acttab_Insert (Table        : in out Table_Type;
                            Make_It_Safe :        Boolean;
                            Offset       :    out Integer);
   --  Add the transaction set built up with prior calls to acttab_action()
   --  into the current action table.  Then reset the transaction set back
   --  to an empty set in preparation for a new round of acttab_action() calls.
   --
   --  Return the offset into the action table of the new transaction.
   --
   --  If the makeItSafe parameter is true, then the offset is chosen so that
   --  it is impossible to overread the yy_lookaside[] table regardless of
   --  the lookaside token.  This is done for the terminal symbols, as they
   --  come from external inputs and can contain syntax errors.  When makeItSafe
   --  is false, there is more flexibility in selecting offsets, resulting in
   --  a smaller table.  For non-terminal symbols, which are never syntax errors,
   --  makeItSafe can be false.

end Action_Tables;
