--
--
--

with Ada.Text_IO;

package Backend is

   type Context_Type is
      record
         File_Template : Ada.Text_IO.File_Type;
      end record;

   Context : Context_Type;

end Backend;
