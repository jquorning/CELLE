--
--
--

with Ada.Text_IO;

package Auxiliary is

   use Ada.Text_IO;

   procedure Recreate
     (File      : in out File_Type;
      Mode      : in     File_Mode;
      File_Name : in     String);
   --  Create or open file.

   function To_Ada_Symbol (Text : in String) return String;
   --  Convert into ada symbol

end Auxiliary;
