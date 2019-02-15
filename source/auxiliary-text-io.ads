--
--  The author disclaims copyright to this source code.  In place of
--  a legal notice, here is a blessing:
--
--    May you do good and not evil.
--    May you find forgiveness for yourself and forgive others.
--    May you share freely, not taking more than you give.
--

package Auxiliary.Text.IO is

   Line_Number : Natural := Natural'First;

   function Line_Get (File : File_Type) return String;

end Auxiliary.Text.IO;
