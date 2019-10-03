
with Ada.Text_IO;

with Symbols;

package body Sets.IO is

   procedure Put (Set : in Set_Type) is
      use Ada.Text_IO;
   begin
      if Set = Null_Set then
         Put ("<null>");
      else
         for Bit of Set.all loop
            if Bit then
               Put ("1");
            else
               Put (" ");
            end if;
         end loop;
      end if;
   end Put;

   procedure Put_Named (Session : in Sessions.Session_Type;
                        Set     : in Set_Type)
   is
      pragma Unreferenced (Session);
      use Ada.Text_IO;
      use Symbols;

      First : Boolean := True;
   begin
      Put ("[");
      for Index in Set'Range loop
         if Set_Find (Set, Index) then
            if not First then
               Put (" ");
            end if;
            Put (Name_Of (Element_At (Symbol_Index (Index))));
            First := False;
         end if;
      end loop;
      Put ("]");
   end Put_Named;

end Sets.IO;
