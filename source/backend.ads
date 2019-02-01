--
--  The author disclaims copyright to this source code.  In place of
--  a legal notice, here is a blessing:
--
--    May you do good and not evil.
--    May you find forgiveness for yourself and forgive others.
--    May you share freely, not taking more than you give.
--

with Ada.Text_IO;

package Backend is

   type Context_Type is
      record
         File_Template : Ada.Text_IO.File_Type;
      end record;

   Context : Context_Type;

end Backend;
