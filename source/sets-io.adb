
with Ada.Text_IO;

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

end Sets.IO;
