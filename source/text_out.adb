--
--  The author disclaims copyright to this source code.  In place of
--  a legal notice, here is a blessing:
--
--    May you do good and not evil.
--    May you find forgiveness for yourself and forgive others.
--    May you share freely, not taking more than you give.
--

with Ada.Text_IO;
with Ada.Strings.Fixed;

with Auxiliary;

package body Text_Out is

   File_Out    : Ada.Text_IO.File_Type;
   Line_Number : Line_Number_Index := 1;

   procedure Implementation_Open
     (File_Name : in chars_ptr)
   is
      use Ada.Text_IO;
   begin
      Auxiliary.Recreate (File_Out, Out_File, Value (File_Name));
   end Implementation_Open;


   procedure Close_Out is
   begin
      Ada.Text_IO.Close (File_Out);
   end Close_Out;


   function Image (Value : in Integer) return String is
      use Ada.Strings;
   begin
      return Fixed.Trim (Integer'Image (Value), Left);
   end Image;



   procedure Put_CP (Item : in chars_ptr)
   is
--      use Text_Out;
   begin
      Put (Value (Item));
   end Put_CP;


   procedure Put_Int (Item : in Integer)
   is
--      use Text_Out;
   begin
      Put (Image (Item));
   end Put_Int;


   procedure Put_Line_CP (Item : in chars_ptr) is
--      use Text_Out;
   begin
      Put_Line (Value (Item));
   end Put_Line_CP;


   procedure Put (Item : in String) is
   begin
      Ada.Text_IO.Put (File_Out, Item);
   end Put;

   procedure Put_Line (Item : in String) is
   begin
      Ada.Text_IO.Put_Line (File_Out, Item);
      Line_Number := Line_Number + 1;
   end Put_Line;

   procedure New_Line is
   begin
      Put_Line ("");
   end New_Line;

   procedure Put_Line_Directive
     (File_Name : in chars_ptr)
   is
--      pragma Unreferenced (Line_Number);
      use Text_Out;
   begin
      Ada.Text_IO.Put_Line ("Write_line_directive 616 ca");
      Put ("#line ");
      --  Put_Int (Line_Number_Index (Line_Number));
      Put_Int (Integer (Line_Number));
      Put (" """);
--  while( *filename ){
--    if( *filename == '\\' ) putc('\\',out);
--    putc(*filename,out);
--    filename++;
--  }
      Put_CP (File_Name);
      Put_Line ("""");
   end Put_Line_Directive;

end Text_Out;
