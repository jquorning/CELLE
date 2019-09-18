
with Configs;

package body Prop_Links is

   procedure Append (Prop_List : in out List;
                     Config    : in     Configs.Config_Access)
   is
   begin
      Propagation_Lists.Append (Prop_List, Config_Access (Config));
   end Append;


   procedure Delete (Prop_List : in out List)
   is
   begin
      Propagation_Lists.Clear (Prop_List);
   end Delete;


   procedure Copy (To   : in out List;
                   From : in     List)
   is
      use Propagation_Lists;
   begin
      for Element of From loop
         Append (To, Config_Access (Element));
      end loop;
   end Copy;


end Prop_Links;
