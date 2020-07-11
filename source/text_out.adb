--
--  The author disclaims copyright to this source code.  In place of
--  a legal notice, here is a blessing:
--
--    May you do good and not evil.
--    May you find forgiveness for yourself and forgive others.
--    May you share freely, not taking more than you give.
--

with Ada.Text_IO;

with Auxiliary;

package body Text_Out is

   File_Out    : Ada.Text_IO.File_Type;
--   Line_Number : Line_Number_Index := 1;


   procedure Implementation_Open (File_Name : in String) is
   begin
      Auxiliary.Recreate (File_Out, Ada.Text_IO.Out_File, File_Name);
   end Implementation_Open;


   procedure Close_Out is
   begin
      Ada.Text_IO.Close (File_Out);
   end Close_Out;


   procedure Put_Int (Item : in Integer)
   is
      function Image is new Auxiliary.Trim_Image (Integer);
   begin
      Put (Image (Item));
   end Put_Int;


   procedure Put (Item : in String) is
   begin
      Ada.Text_IO.Put (File_Out, Item);
   end Put;

--   procedure Put_Line (Item : in String) is
--   begin
--      Ada.Text_IO.Put_Line (File_Out, Item);
--      Line_Number := Line_Number + 1;
--   end Put_Line;

   procedure New_Line is
   begin
      Put_Line ("");
   end New_Line;

   ------------------------
   -- Put_Line_Directive --
   ------------------------

   procedure Put_Line_Directive (File      : File_Type;
                                 Line      : Line_Number;
                                 File_Name : String)
   is
      use Ada.Text_IO;
   begin
      Put (File, "#line ");
      Put (File, Line_Number'Image (Line));
      Put (File, " """);
      Put (File, File_Name);
      Put (File, """");
      New_Line (File);
   end Put_Line_Directive;


end Text_Out;
