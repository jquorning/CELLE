--
--  The author disclaims copyright to this source code.  In place of
--  a legal notice, here is a blessing:
--
--    May you do good and not evil.
--    May you find forgiveness for yourself and forgive others.
--    May you share freely, not taking more than you give.
--

with Ada.Text_IO;

with Types;

package Templates is

   subtype File_Type   is Ada.Text_IO.File_Type;
   subtype Line_Number is Types.Line_Number;

   procedure Transfer
     (File : File_Type;
      Name : String);
   --

   procedure Open
     (User_Template :        String;
      Error_Count   : in out Integer;
      Success       :    out Integer);
   --  Thisk function finds the template file and opens it. File handle
   --  is located in the context structure.

   procedure Line_Directive (File      : File_Type;
                             Line      : Line_Number;
                             File_Name : String);
   --  Put line directive to File. Like '#line <Line> "<File_Name>"'.

   procedure Print
     (File            :        File_Type;
      Line            : in out Line_Number;
      Out_Name        :        String;
      No_Line_Numbers :        Boolean;
      Include         :        String);


end Templates;
