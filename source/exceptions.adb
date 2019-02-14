--
--  The author disclaims copyright to this source code.  In place of
--  a legal notice, here is a blessing:
--
--    May you do good and not evil.
--    May you find forgiveness for yourself and forgive others.
--    May you share freely, not taking more than you give.
--

with Ada.Text_IO;

package body Exceptions is


   procedure Put_Message
     (Occurrence : Ada.Exceptions.Exception_Occurrence)
   is
      use Ada.Text_IO;
      use Ada.Exceptions;
   begin
      Put_Line (Exception_Message (Occurrence));
   end Put_Message;


end Exceptions;
