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

with Interfaces.C.Strings;

package Text_Out is

   type Line_Number_Index is new Integer;

   use Interfaces.C.Strings;

   procedure Implementation_Open
     (File_Name : in String);

   procedure Close_Out;
   --  Close out file

   procedure Put_CP (Item : in chars_ptr);
   --  Put Item line to out file.

   procedure Put_Line_CP (Item : in chars_ptr);
   --  Put Item line and new line to out file with line number increase.

   procedure Put (Item : in String);
   --  Put Item line to out file.

   procedure Put_Line (Item : in String);
   --  Put Item line to out file with new line and line number increase.

   procedure Put_Int (Item : in Integer);
   --  Put intege to out file.

   procedure New_Line;
   --  New line to the out file with line number increase.

   procedure Put_Line_Directive (File_Name : in String);
   --  Put line directive to File_Name

private

   pragma Export (C, Put_CP,        "lime_put");
   pragma Export (C, Put_Int,       "lime_put_int");
   pragma Export (C, Put_Line_CP,   "lime_put_line");

end Text_Out;
