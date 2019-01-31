--
--
--

with Interfaces.C.Strings;

package Text_Out is

   type Line_Number_Index is new Integer;

   use Interfaces.C.Strings;

   procedure Implementation_Open
     (File_Name : in chars_ptr);

   procedure Close_Out;
   --  Close out file

   function Image (Value : in Integer)
                  return String;
   --  Trimmed image of Value.

   procedure Put_CP (Item : in chars_ptr);

   procedure Put_Line_CP (Item : in chars_ptr);

   procedure Put (Item : in String);

   procedure Put_Line (Item : in String);

   procedure Put_Int (Item : in Integer);

   procedure New_Line;


   procedure Put_Line_Directive
     (File_Name : in chars_ptr);

private

   pragma Export (C, Put_CP,        "lime_put");
   pragma Export (C, Put_Int,       "lime_put_int");
   pragma Export (C, Put_Line_CP,   "lime_put_line");

   pragma Export (C, Put_Line_Directive,  "lime_write_line_directive");

end Text_Out;
