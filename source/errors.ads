--
--  The author disclaims copyright to this source code.  In place of
--  a legal notice, here is a blessing:
--
--    May you do good and not evil.
--    May you find forgiveness for yourself and forgive others.
--    May you share freely, not taking more than you give.
--

with Ada.Strings.Unbounded;
with Ada.Text_IO;

with Types;

package Errors is

   use Ada.Strings.Unbounded;
   subtype Line_Number is Types.Line_Number;

   Default_File_Name : Unbounded_String := To_Unbounded_String ("<null>");
   Error_Count       : Natural          := 0;

   type Error_Id is
     (E001,
      E002,
      E003,
      E004,
      E005,
      E006,
      E007,
      E008,
      E009,
      E010,
      E011,
      E012,
      E013,
      E014,
      E015,
      E016,

      E101,
      E102,
      E103,

      E201,
      E202,
      E203,
      E204,
      E205,
      E206,
      E207,
      E208,
      E209,
      E210,
      E211,
      E212,
      E213,
      E214,
      E215,
      E216,
      E217,
      E218,

      E301,

      E401
     );

   procedure Set_File_Name
     (File_Name : in Ada.Strings.Unbounded.Unbounded_String);

   procedure Parser_Error
     (Id         : in Error_Id;
      Line       : in Line_Number;
      Argument_1 : in String := "";
      Argument_2 : in String := "");

   procedure Emit_Error (File      : in Ada.Text_IO.File_Type;
                         File_Name : in String;
                         Line      : in Line_Number;
                         Message   : in String);

end Errors;
