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

with Ada.Containers.Doubly_Linked_Lists;

package body Actions is

   package Action_Lists is
      new Ada.Containers.Doubly_Linked_Lists (Element_Type => Action_Record);


   function Action_Cmp (Left, Right : in Action_Record)
                       return Boolean
   is
      RC : Integer;
   begin
      RC := Integer (Left.Symbol.Index) - Integer (Right.Symbol.Index);

      if RC = 0 then
         RC := Action_Kind'Pos (Left.Kind) - Action_Kind'Pos (Right.Kind);
      end if;

      if
        RC = 0 and
        (Left.Kind = Reduce or Left.Kind = Shift_Reduce)
      then
         RC := Left.X.Rule.Index - Right.X.Rule.Index;
      end if;

      if RC = 0 then
         RC := 0;
         --  RC := (int) (ap2 - ap1); -- XXX Pointer
         raise Program_Error;
      end if;
      return RC /= 0;
   end Action_Cmp;


   --   Free_List : Action_Access := null;
--
--     function Action_New return Action_Access is
--        New_Action : Action_Access;
--     begin
--        if Free_List = null then
--           declare
--              I   : Integer;
--              amt : Integer := 100;
--           begin
--      freelist = (struct action *)calloc(amt, sizeof(struct action));
--      if( freelist==0 ){
--        fprintf(stderr,"Unable to allocate memory for a new parser action.");
--        exit(1);
--      }
--      for(i=0; i<amt-1; i++) freelist[i].next = &freelist[i+1];
--      freelist[amt-1].next = 0;
--      end;
--           end if;
--    New_Action := Free_List;
--    Free_List  := Free_List.Next;
--    return New_Action;
--  end Action_New;


   function Resolve_Conflict (Left  : in out Action_Record;
                              Right : in out Action_Record) return Integer
   is
      use Symbols;

--      Apx : Action_Access renames Left;
--      Apy : Action_Access renames Right;
      Apx : Action_Record renames Left;
      Apy : Action_Record renames Right;
      Spx : Symbol_Access;
      Spy : Symbol_Access;
      Error_Count : Natural := 0;
   begin
      pragma Assert (Apx.Symbol = Apy.Symbol);  --  Otherwise there would be no conflict

      if Apx.Kind = Shift and Apy.Kind = Shift then
         Apy.Kind := SS_Conflict;
         Error_Count := Error_Count + 1;
      end if;

      if Apx.Kind = Shift and Apy.Kind = Reduce then
         Spx := Symbol_Access (Apx.Symbol);
         Spy := Symbol_Access (Apy.X.Rule.Prec_Symbol);
         if Spy = null or Spx.Precedence < 0 or Spy.Precedence < 0 then
            --  Not enough precedence information
            Apy.Kind := SR_Conflict;
            Error_Count := Error_Count + 1;
         elsif Spx.Precedence > Spy.Precedence then    -- higher precedence wins
            Apy.Kind := RD_Resolved;
         elsif Spx.Precedence < Spy.Precedence then
            Apx.Kind := SH_Resolved;

         elsif
           Spx.Precedence = Spy.Precedence and
           Spx.Association = Right_Association
         then -- Use operator
            Apy.Kind := RD_Resolved;                             -- associativity

         elsif
           Spx.Precedence = Spy.Precedence and
           Spx.Association = Left_Association
         then  -- to break tie
            Apx.Kind := SH_Resolved;
         else
            pragma Assert (Spx.Precedence = Spy.Precedence and
                           Spx.Association = No_Association);
            Apx.Kind := Error;
         end if;
      elsif Apx.Kind = Reduce and Apy.Kind = Reduce then
         Spx := Symbol_Access (Apx.X.Rule.Prec_Symbol);
         Spy := Symbol_Access (Apy.X.Rule.Prec_Symbol);
         if
           Spx = null or Spy = null or Spx.Precedence < 0 or
           Spy.Precedence < 0 or Spx.Precedence = Spy.Precedence
         then
            Apy.Kind := RR_Conflict;
            Error_Count := Error_Count + 1;

         elsif Spx.Precedence > Spy.Precedence then
            Apy.Kind := RD_Resolved;

         elsif Spx.Precedence < Spy.Precedence then
            Apx.Kind := RD_Resolved;
         end if;
      else
         null;
         pragma Assert
           (Apx.Kind = SH_Resolved or
            Apx.Kind = RD_Resolved or
            Apx.Kind = SS_Conflict or
            Apx.Kind = SR_Conflict or
            Apx.Kind = RR_Conflict or
            Apy.Kind = SH_Resolved or
            Apy.Kind = RD_Resolved or
            Apy.Kind = SS_Conflict or
            Apy.Kind = SR_Conflict or
            Apy.Kind = RR_Conflict);
         --  The REDUCE/SHIFT case cannot happen because SHIFTs come before
         --  REDUCEs on the list.  If we reach this point it must be because
         --  the parser conflict had already been resolved.
      end if;
      return Error_Count;
   end Resolve_Conflict;

end Actions;
