--
--  The author disclaims copyright to this source code.  In place of
--  a legal notice, here is a blessing:
--
--    May you do good and not evil.
--    May you find forgiveness for yourself and forgive others.
--    May you share freely, not taking more than you give.
--

with Ada.Text_IO;

package Auxiliary.Line_Read is

   Line_Number   : Natural  := 0;

   function Line_Get (File : File_Type)
                     return String;
   --

end Auxiliary.Line_Read;
