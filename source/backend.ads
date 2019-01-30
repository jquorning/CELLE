--
--
--

with Ada.Text_IO;

package Backend is

   type Line_Number_Index is new Integer;
   type Context_Type is record
      File_Template       : Ada.Text_IO.File_Type;
      File_Implementation : Ada.Text_IO.File_Type;
      Line_Number         : Line_Number_Index := 1;
   end record;

   Context : Context_Type;

end Backend;
