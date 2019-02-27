--
--  The author disclaims copyright to this source code.  In place of
--  a legal notice, here is a blessing:
--
--    May you do good and not evil.
--    May you find forgiveness for yourself and forgive others.
--    May you share freely, not taking more than you give.
--

with Ada.Strings.Unbounded;

with Errors;

package Scanner_Errors is

   Start_Line  : Natural := 0;
   Error_Count : Natural := 0;

   use Errors;

   procedure Set_File_Name
     (File_Name : in Ada.Strings.Unbounded.Unbounded_String);

   procedure Error (Kind        : in K_Error_Parse_One_Token;
                    Arguments   : in Argument_List;
                    Line_Number : in Natural             := Start_Line);

   procedure Error (Kind        : in K_Error_Parse_One_Token;
                    Line_Number : in Natural                 := Start_Line);

end Scanner_Errors;
