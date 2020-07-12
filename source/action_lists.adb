with Symbols;
with States;
with Rules;

package body Action_Lists is

   procedure Append (Action_List : in out List;
                     Kind        : in     Actions.Action_Kind;
                     Symbol      : in     Symbols.Symbol_Access;
                     State       : in     States.State_Access;
                     Rule        : in     Rules.Rule_Access)
   is
      use Actions;
--      New_Action : Action_Access;
      Union : X_Union;
   begin
      if Kind = Shift then
         Union.State := State;
      else
         Union.Rule := Rule;
      end if;
      Action_List.Append (Action_Record'(Kind             => Kind,
                                         Symbol           => Symbol,
                                         Symbol_Kind_Link => null,
                                         X                => Union,
                                         Collide          => null));
--        New_Action := new Action_Record;  --  Action_New ();
--        New_Action.Next := Action;
--        Action := New_Action;
--        New_Action.Kind   := Kind;
--        New_Action.Symbol := Symbol;
--        New_Action.spOpt  := null;
--        if Kind = Shift then
--           New_Action.X.stp := State;
--        else
--           New_Action.X.Rule := Rule;
--        end if;
   end Append;


   package Action_Sorting is
      new Action_DLLs.Generic_Sorting ("<" => Actions.Action_Cmp);

   procedure Sort (Action_List : in out List)
--     procedure Action_Sort (Action_List : in out List)
--   function Action_Sort (Action : in Action_Access) return Action_Access
   is
   begin
      Action_Sorting.Sort (Action_List);
--        static struct action *Action_sort(
--    struct action *ap
--  ){
--    ap = (struct action *)msort((char *)ap,(char **)&ap->next,
--                                (int(*)(const char*,const char*))actioncmp);
--    return ap;
--  }

--   end Action_Sort;
   end Sort;


end Action_Lists;
