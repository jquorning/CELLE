--
--  The author disclaims copyright to this source code.  In place of
--  a legal notice, here is a blessing:
--
--    May you do good and not evil.
--    May you find forgiveness for yourself and forgive others.
--    May you share freely, not taking more than you give.
--

package body Scanner_Errors is


   procedure Set_File_Name
     (File_Name : in Ada.Strings.Unbounded.Unbounded_String)
   is
   begin
      Errors.Default_File_Name := File_Name;
   end Set_File_Name;


   procedure Error (Kind        : in K_Error_Parse_One_Token;
                    Arguments   : in Argument_List;
                    Line_Number : in Natural             := Start_Line)
   is
   begin
      Errors.Error (Kind, Line_Number, Arguments);
      Error_Count := Error_Count + 1;
   end Error;


   procedure Error (Kind        : in K_Error_Parse_One_Token;
                    Line_Number : in Natural                 := Start_Line)
   is
   begin
      Error (Kind, Null_Argument_List, Line_Number);
   end Error;


end Scanner_Errors;
