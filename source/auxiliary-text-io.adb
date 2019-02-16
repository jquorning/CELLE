--
--  The author disclaims copyright to this source code.  In place of
--  a legal notice, here is a blessing:
--
--    May you do good and not evil.
--    May you find forgiveness for yourself and forgive others.
--    May you share freely, not taking more than you give.
--

package body Auxiliary.Text.IO is

   function Line_Get (File : File_Type) return String
   is
      Line : constant String := Get_Line (File);
   begin
      Line_Number := Line_Number + 1;
      return Line;
   end Line_Get;

end Auxiliary.Text.IO;
