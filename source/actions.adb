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

package body Actions is

--  /* Return the number of entries in the yy_action table */
--  #define acttab_lookahead_size(X) ((X)->nAction)
   function Lookahead_Size (P : in Action_Table) return Integer is
   begin
      return P.N_Action;
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


   function Alloc (N_Symbol   : in Integer;
                   N_Terminal : in Integer) return A_Action_Table
   is
      P : constant A_Action_Table := new Action_Table;
   begin
      P.N_Symbol   := N_Symbol;
      P.N_Terminal := N_Terminal;
      return P;
   end Alloc;

--  /* Add a new action to the current transaction set.
--  **
--  ** This routine is called once for each lookahead for a particular
--  ** state.
--  */
--  void acttab_action(acttab *p, int lookahead, int action){
--    if( p->nLookahead>=p->nLookaheadAlloc ){
--      p->nLookaheadAlloc += 25;
--      p->aLookahead = (struct lookahead_action *) realloc( p->aLookahead,
--                               sizeof(p->aLookahead[0])*p->nLookaheadAlloc );
--      if( p->aLookahead==0 ){
--        fprintf(stderr,"malloc failed\n");
--        exit(1);
--      }
--    }
--    if( p->nLookahead==0 ){
--      p->mxLookahead = lookahead;
--      p->mnLookahead = lookahead;
--      p->mnAction = action;
--    }else{
--      if( p->mxLookahead<lookahead ) p->mxLookahead = lookahead;
--      if( p->mnLookahead>lookahead ){
--        p->mnLookahead = lookahead;
--        p->mnAction = action;
--      }
--    }
--    p->aLookahead[p->nLookahead].lookahead = lookahead;
--    p->aLookahead[p->nLookahead].action = action;
--    p->nLookahead++;
--  }

--  /*
--  ** Add the transaction set built up with prior calls to acttab_action()
--  ** into the current action table.  Then reset the transaction set back
--  ** to an empty set in preparation for a new round of acttab_action() calls.
--  **
--  ** Return the offset into the action table of the new transaction.
--  **
--  ** If the makeItSafe parameter is true, then the offset is chosen so that
--  ** it is impossible to overread the yy_lookaside[] table regardless of
--  ** the lookaside token.  This is done for the terminal symbols, as they
--  ** come from external inputs and can contain syntax errors.  When makeItSafe
--  ** is false, there is more flexibility in selecting offsets, resulting in
--  ** a smaller table.  For non-terminal symbols, which are never syntax errors,
--  ** makeItSafe can be false.
--  */
--  int acttab_insert(acttab *p, int makeItSafe){
--    int i, j, k, n, end;
--    assert( p->nLookahead>0 );

--    /* Make sure we have enough space to hold the expanded action table
--    ** in the worst case.  The worst case occurs if the transaction set
--    ** must be appended to the current action table
--    */
--    n = p->nsymbol + 1;
--    if( p->nAction + n >= p->nActionAlloc ){
--      int oldAlloc = p->nActionAlloc;
--      p->nActionAlloc = p->nAction + n + p->nActionAlloc + 20;
--      p->aAction = (struct lookahead_action *) realloc( p->aAction,
--                            sizeof(p->aAction[0])*p->nActionAlloc);
--      if( p->aAction==0 ){
--        fprintf(stderr,"malloc failed\n");
--        exit(1);
--      }
--      for(i=oldAlloc; i<p->nActionAlloc; i++){
--        p->aAction[i].lookahead = -1;
--        p->aAction[i].action = -1;
--      }
--    }

--    /* Scan the existing action table looking for an offset that is a
--    ** duplicate of the current transaction set.  Fall out of the loop
--    ** if and when the duplicate is found.
--    **
--    ** i is the index in p->aAction[] where p->mnLookahead is inserted.
--    */
--    end = makeItSafe ? p->mnLookahead : 0;
--    for(i=p->nAction-1; i>=end; i--){
--      if( p->aAction[i].lookahead==p->mnLookahead ){
--        /* All lookaheads and actions in the aLookahead[] transaction
--        ** must match against the candidate aAction[i] entry. */
--        if( p->aAction[i].action!=p->mnAction ) continue;
--        for(j=0; j<p->nLookahead; j++){
--          k = p->aLookahead[j].lookahead - p->mnLookahead + i;
--          if( k<0 || k>=p->nAction ) break;
--          if( p->aLookahead[j].lookahead!=p->aAction[k].lookahead ) break;
--          if( p->aLookahead[j].action!=p->aAction[k].action ) break;
--        }
--        if( j<p->nLookahead ) continue;

--        /* No possible lookahead value that is not in the aLookahead[]
--        ** transaction is allowed to match aAction[i] */
--        n = 0;
--        for(j=0; j<p->nAction; j++){
--          if( p->aAction[j].lookahead<0 ) continue;
--          if( p->aAction[j].lookahead==j+p->mnLookahead-i ) n++;
--        }
--        if( n==p->nLookahead ){
--          break;  /* An exact match is found at offset i */
--        }
--      }
--    }

--    /* If no existing offsets exactly match the current transaction, find an
--    ** an empty offset in the aAction[] table in which we can add the
--    ** aLookahead[] transaction.
--    */
--    if( i<end ){
--      /* Look for holes in the aAction[] table that fit the current
--      ** aLookahead[] transaction.  Leave i set to the offset of the hole.
--      ** If no holes are found, i is left at p->nAction, which means the
--      ** transaction will be appended. */
--      i = makeItSafe ? p->mnLookahead : 0;
--      for(; i<p->nActionAlloc - p->mxLookahead; i++){
--        if( p->aAction[i].lookahead<0 ){
--          for(j=0; j<p->nLookahead; j++){
--            k = p->aLookahead[j].lookahead - p->mnLookahead + i;
--            if( k<0 ) break;
--            if( p->aAction[k].lookahead>=0 ) break;
--          }
--          if( j<p->nLookahead ) continue;
--          for(j=0; j<p->nAction; j++){
--            if( p->aAction[j].lookahead==j+p->mnLookahead-i ) break;
--          }
--          if( j==p->nAction ){
--            break;  /* Fits in empty slots */
--          }
--        }
--      }
--    }
--    /* Insert transaction set at index i. */
--    for(j=0; j<p->nLookahead; j++){
--      k = p->aLookahead[j].lookahead - p->mnLookahead + i;
--      p->aAction[k] = p->aLookahead[j];
--      if( k>=p->nAction ) p->nAction = k+1;
--    }
--    if( makeItSafe && i+p->nterminal>=p->nAction ) p->nAction = i+p->nterminal+1;
--    p->nLookahead = 0;

--    /* Return the offset that is added to the lookahead in order to get the
--    ** index into yy_action of the action */
--    return i - p->mnLookahead;
--  }


--  int acttab_action_size(acttab *p){
--    int n = p->nAction;
--    while( n>0 && p->aAction[n-1].lookahead<0 ){ n--; }
--    return n;
--  }


   function Action_Size (P : in Action_Table) return Integer
   is
      N : Integer := P.N_Action;
   begin
      while N > 0 and P.Action (N - 1).Lookahead < 0 loop
         N := N - 1;
      end loop;
      return N;
   end Action_Size;


   function Action_Cmp (Left, Right : in Action_Record)
                       return Integer
   is
      RC : Integer;
   begin
      RC := Integer (Left.Symbol.Index) - Integer (Right.Symbol.Index);

      if RC = 0 then
         RC := E_Action'Pos (Left.Kind) - E_Action'Pos (Right.Kind);
      end if;

      if
        RC = 0 and
        (Left.Kind = REDUCE or Left.Kind = SHIFTREDUCE)
      then
         RC := Left.X.Rule.Index - Right.X.Rule.Index;
      end if;

      if RC = 0 then
         RC := 0;
         --  RC := (int) (ap2 - ap1); -- XXX Pointer
         raise Program_Error;
      end if;
      return RC;
   end Action_Cmp;


end Actions;
