--
--  The author disclaims copyright to this source code.  In place of
--  a legal notice, here is a blessing:
--
--    May you do good and not evil.
--    May you find forgiveness for yourself and forgive others.
--    May you share freely, not taking more than you give.
--

package body Auxiliary.Errors is


   procedure Error (File_Name   : in String;
                    Line_Number : in Natural;
                    Message     : in String)
   is
   begin
      Put (Standard_Error, File_Name);
      Put (Standard_Error, ":");
      Put (Standard_Error, Natural'Image (Line_Number));
      Put (Standard_Error, ": ");
      Put (Standard_Error, Message);
      New_Line (Standard_Error);
   end Error;


end Auxiliary.Errors;
