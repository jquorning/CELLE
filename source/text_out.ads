--
--  The author disclaims copyright to this source code.  In place of
--  a legal notice, here is a blessing:
--
--    May you do good and not evil.
--    May you find forgiveness for yourself and forgive others.
--    May you share freely, not taking more than you give.
--
-----------------------------------------------------------------------------
--  Handling of out file.
--

with Ada.Text_IO;

with Types;

package Text_Out is

   subtype Line_Number is Types.Line_Number;
   subtype File_Type   is Ada.Text_IO.File_Type;
   type Line_Number_Index is new Integer;

   procedure Implementation_Open (File_Name : in String);

   procedure Close_Out;
   --  Close out file

   procedure Put (Item : in String);
   --  Put Item line to out file.

--   procedure Put_Line (Item : in String);
   --  Put Item line to out file with new line and line number increase.

   procedure Put_Int (Item : in Integer);
   --  Put intege to out file.

   procedure New_Line;
   --  New line to the out file with line number increase.

   procedure Put_Line_Directive (File      : File_Type;
                                 Line      : Line_Number;
                                 File_Name : String);
   --  Put line directive to File. Like '#line <Line> "<File_Name>"'

end Text_Out;
