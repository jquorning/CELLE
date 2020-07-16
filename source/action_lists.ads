
with Ada.Containers.Doubly_Linked_Lists;

with Actions;
limited with Symbols;
limited with States;
--  limited with Rules;
with Rule_Lists;

package Action_Lists is

   subtype Rule_Access is Rule_Lists.Rule_Access;

   package Action_DLLs is
      new Ada.Containers.Doubly_Linked_Lists ("="          => Actions."=",
                                              Element_Type => Actions.Action_Record);

   subtype List is Action_DLLs.List;

   procedure Sort (Action_List : in out List);
   --  Action_Sort

   procedure Append (Action_List : in out List;
                     Kind        : in     Actions.Action_Kind;
                     Symbol      : in     Symbols.Symbol_Access;
                     State       : in     States.State_Access;
                     Rule        : in     Rule_Access);
   --  Action_Add
   --  Append symbol to Action_List

end Action_Lists;
