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

with Auxiliary;

package body Action_Tables is

   --------------------
   -- Lookahead_Size --
   --------------------

   function Lookahead_Size (Table : in Table_Type) return Integer is
   begin
      return Table.Num_Action;
   end Lookahead_Size;

--  /* The value for the N-th entry in yy_action */
--  #define acttab_yyaction(X,N)  ((X)->aAction[N].action)

--  /* The value for the N-th entry in yy_lookahead */
--  #define acttab_yylookahead(X,N)  ((X)->aAction[N].lookahead)

--  /* Free all memory associated with the given acttab */
--  void acttab_free(acttab *p){
--    free( p->aAction );
--    free( p->aLookahead );
--    free( p );
--  }

   -----------
   -- Alloc --
   -----------

   function Alloc (N_Symbol   : in Integer;
                   N_Terminal : in Integer) return Table_Access
   is
      P : constant Table_Access := new Table_Type;
   begin
      P.Num_Symbol   := N_Symbol;
      P.Num_Terminal := N_Terminal;
      return P;
   end Alloc;

   -------------------
   -- Acttab_Action --
   -------------------

   procedure Acttab_Action (Table     : in out Table_Type;
                            Lookahead :        Lookahead_Type;
                            Action    :        Action_Value)
   is
      Additional : constant := 25;
   begin
      if Table.Num_Lookahead > Table.Lookahead'Last then
         declare
            New_Lookahead : constant Action_Array_Access :=
              new Action_Array'(0 .. Table.Lookahead'Last + Additional
                                  => Lookahead_Action'(0, 0));
         begin
            --  Copy
            New_Lookahead.all (0 .. Table.Num_Lookahead - 1) :=
              Table.Lookahead.all;
            --  Fill last part
            New_Lookahead.all (Table.Num_Lookahead ..
                                 New_Lookahead'Last) :=
              (others => Lookahead_Action'(0, 0));
            --  Assing
            Table.Lookahead := New_Lookahead;
         end;
      end if;
      if Table.Num_Lookahead = 0 then
         Table.Max_Lookahead := Lookahead;
         Table.Min_Lookahead := Lookahead;
         Table.Min_Action    := Action;
      else
         if Table.Max_Lookahead < Lookahead then
            Table.Max_Lookahead := Lookahead;
         end if;
         if Table.Min_Lookahead > Lookahead then
            Table.Min_Lookahead := Lookahead;
            Table.Min_Action    := Action;
         end if;
      end if;
      Table.Lookahead (Table.Num_Lookahead) := (Lookahead, Action);
      Table.Num_Lookahead := Table.Num_Lookahead + 1;
   end Acttab_Action;

   -------------------
   -- Acttab_Insert --
   -------------------

   procedure Acttab_Insert (Table        : in out Table_Type;
                            Make_It_Safe :        Boolean;
                            Offset       :    out Integer)
   is
      use type Actions.Action_Value;

      P : Table_Type renames Table;
      I, J, K, N, Endd : Integer;
   begin
      pragma Assert (P.Num_Lookahead > 0);

      --  Make sure we have enough space to hold the expanded action table
      --  in the worst case.  The worst case occurs if the transaction set
      --  must be appended to the current action table
      N := P.Num_Symbol + 1;
      if P.Num_Action + N > P.Action'Last then
         declare
            Additional : constant := 20;
            procedure Realloc is
               new Auxiliary.Resize_Array (Index_Type   => Natural,
                                           Element_Type => Lookahead_Action,
                                           Array_Type   => Action_Array,
                                           Array_Access => Action_Array_Access);
         begin
            Realloc (Item     => P.Action,
                     New_Last => P.Action'Last + Additional,
                     Default  => (-1, -1));
         end;
      end if;

      --  Scan the existing action table looking for an offset that is a
      --  duplicate of the current transaction set.  Fall out of the loop
      --  if and when the duplicate is found.
      --
      --  i is the index in p.aAction[] where p.mnLookahead is inserted.
      Endd := (if Make_It_Safe then Integer (P.Min_Lookahead) else 0);
      I    := P.Num_Action - 1;
      while I >= Endd loop
         if P.Action (I).Lookahead = P.Min_Lookahead then
            --  All lookaheads and actions in the aLookahead[] transaction
            --  must match against the candidate aAction[i] entry.
            if P.Action (I).Action /= P.Min_Action then
               goto Continue_1;
            end if;
            J := 0;
            while J < P.Num_Lookahead - 1 loop
               K := Integer (P.Lookahead (J).Lookahead - P.Min_Lookahead) + I;
               exit when K not in 0 .. P.Num_Action - 1;
               exit when P.Lookahead (J).Lookahead /= P.Action (K).Lookahead;
               exit when P.Lookahead (J).Action    /= P.Action (K).Action;
               J := J + 1;
            end loop;
            if J < P.Num_Lookahead then
               goto Continue_1;
            end if;

            --  No possible lookahead value that is not in the aLookahead[]
            --  transaction is allowed to match aAction[i] */
            N := 0;
            for J in 0 .. P.Num_Action - 1 loop
               if P.Action (J).Lookahead < 0 then goto Continue_2; end if;
               if
                 P.Action (J).Lookahead =
                 Lookahead_Type (J + Integer (P.Min_Lookahead) - I)
               then
                  N := N + 1;
               end if;
               <<Continue_2>>
            end loop;
            exit when N = P.Num_Lookahead;  -- An exact match is found at offset i
         end if;

         <<Continue_1>>
         I := I - 1;
      end loop;

      --  If no existing offsets exactly match the current transaction, find an
      --  an empty offset in the aAction[] table in which we can add the
      --  aLookahead[] transaction.
      if I < Endd then
         --  Look for holes in the aAction[] table that fit the current
         --  aLookahead[] transaction.  Leave i set to the offset of the hole.
         --  If no holes are found, i is left at p.nAction, which means the
         --  transaction will be appended.
         I := (if Make_It_Safe then Integer (P.Min_Lookahead) else 0);
         while I < P.Action'Last + 1 - Integer (P.Max_Lookahead) loop   --  + 1 ?
            if P.Action (I).Lookahead < 0 then
               for J in 0 .. P.Num_Lookahead - 1 loop
                  K := Integer (P.Lookahead (J).Lookahead - P.Min_Lookahead) + I;
                  exit when K < 0;
                  exit when P.Action (K).Lookahead >= 0;
               end loop;
               if J < P.Num_Lookahead then
                  goto Continue_3;
               end if;
               for J in 0 .. P.Num_Action - 1 loop
                  exit when P.Action (J).Lookahead =
                    Lookahead_Type (J + Integer (P.Min_Lookahead) - I);
               end loop;
               exit when J = P.Num_Action;  -- Fits in empty slots
            end if;

            <<Continue_3>>
            I := I + 1;
         end loop;
      end if;

      --  Insert transaction set at index i.
      for J in 0 .. P.Num_Lookahead - 1 loop
         K := Integer (P.Lookahead (J).Lookahead - P.Min_Lookahead) + I;
         P.Action (K) := P.Lookahead (J);
         if K >= P.Num_Action then
            P.Num_Action := K + 1;
         end if;
      end loop;
      if Make_It_Safe and I + P.Num_Terminal >= P.Num_Action then
         P.Num_Action := I + P.Num_Terminal + 1;
      end if;
      P.Num_Lookahead := 0;

      --  Return the offset that is added to the lookahead in order to get the
      --  index into yy_action of the action
      Offset := I - Integer (P.Min_Lookahead);
   end Acttab_Insert;

   -----------------
   -- Action_Size --
   -----------------

   function Action_Size (Table : in Table_Type) return Integer is
   begin
      for N in reverse 1 .. Table.Num_Action loop
         if Table.Action (N - 1).Lookahead < 0 then
            return N;
         end if;
      end loop;
      return 0;
   end Action_Size;

end Action_Tables;
