--
--  The author disclaims copyright to this source code.  In place of
--  a legal notice, here is a blessing:
--
--    May you do good and not evil.
--    May you find forgiveness for yourself and forgive others.
--    May you share freely, not taking more than you give.
--

with Ada.Strings.Unbounded;

package Errors is

   use Ada.Strings.Unbounded;
   Default_File_Name : Unbounded_String := To_Unbounded_String ("<null>");
   Error_Count       : Natural          := 0;

   type Argument_List is array (Positive range <>) of Unbounded_String;

   subtype Empty_Range is Positive range 1 .. 0;
   Null_Argument_List : constant Argument_List (Empty_Range) := (others => Null_Unbounded_String);

   procedure Error_Plain (File_Name   : in Unbounded_String;
                          Line_Number : in Natural;
                          Text        : in String;
                          Arguments   : in Argument_List);
   --

   type K_Message is
     (W001,
      W002,
      E001,
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

      F001,
      F002
     );
   subtype K_Warning is K_Message range W001 .. W001;
   subtype K_Error   is K_Message range E001 .. K_Message'Val (K_Message'Pos (F001) - 1);
   subtype K_Fatal   is K_Message range F001 .. K_Message'Last;

   type K_Error_Parse is new K_Error range E001 .. E213;


   procedure Set_File_Name
     (File_Name : in Ada.Strings.Unbounded.Unbounded_String);

   procedure Parser_Error
     (Kind        : in K_Error_Parse;
      Line_Number : in Natural;
      Arguments   : in Argument_List := Null_Argument_List);


end Errors;
