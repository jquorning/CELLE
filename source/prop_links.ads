
with Ada.Containers.Doubly_Linked_Lists;

limited with Configs;

package Prop_Links is

   type Config_Access is access all Configs.Config_Record;
   package Propagation_Lists is
      new Ada.Containers.Doubly_Linked_Lists (Element_Type => Config_Access);

   subtype List is Propagation_Lists.List;

   procedure Append (Prop_List : in out List;
                     Config    : in     Configs.Config_Access);

   procedure Delete (Prop_List : in out List);
   --  Delete every plink on the list

   procedure Copy (To   : in out List;
                   From : in     List);
   --  Transfer every plink on the list "from" to the list "to"

end Prop_Links;
