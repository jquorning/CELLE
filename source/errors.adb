--
--  The author disclaims copyright to this source code.  In place of
--  a legal notice, here is a blessing:
--
--    May you do good and not evil.
--    May you find forgiveness for yourself and forgive others.
--    May you share freely, not taking more than you give.
--

with Ada.Text_IO;

package body Errors is


   procedure Error (File_Name   : in String;
                    Line_Number : in Natural;
                    Message     : in String)
   is
      use Ada.Text_IO;
   begin
      Put (Standard_Error, File_Name);
      Put (Standard_Error, ":");
      Put (Standard_Error, Natural'Image (Line_Number));
      Put (Standard_Error, ": ");
      Put (Standard_Error, Message);
      New_Line (Standard_Error);
   end Error;


   procedure Error_1 (File_Name : in String;
                      Start     : in String;
                      Name      : in Key_Type)
   is
      Name_Image : constant String := Symbols.From_Key (Name);
   begin
      Error
        (File_Name, 0,
         "The specified start symbol '" & Start &
           "' is not in a nonterminal of the grammar.  '" &
           Name_Image & "' will be used as the start " &
           "symbol instead.");
   end Error_1;
   

   procedure Error_2 (File_Name   : in String;
                      Line_Number : in Natural;
                      Name        : in Key_Type)
   is
      Name_Image : constant String := To_String (Name);
   begin
      Error
        (File_Name, 0,
         "The start symbol '" & Name_Image & "' occurs on " &
           "the right-hand side of a rule. This will " &
           "result in a parser which does not work properly.");
   end Error_2;


end Errors;
