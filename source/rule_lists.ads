
with Ada.Containers.Doubly_Linked_Lists;

limited with Rules;

package Rule_Lists is

   type Rule_Access is access all Rules.Rule_Record;

   package Lists is
      new Ada.Containers.Doubly_Linked_Lists (Element_Type => Rule_Access);

--   function Element (List : Lists.List) return Natural;

end Rule_Lists;
