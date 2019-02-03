--
--  The author disclaims copyright to this source code.  In place of
--  a legal notice, here is a blessing:
--
--    May you do good and not evil.
--    May you find forgiveness for yourself and forgive others.
--    May you share freely, not taking more than you give.
--

with Ada.Command_Line;

with Lime;

package Cherry_Main is

   procedure Main (Lemon  : in out Lime.Lemon_Record;
                   Status :    out Ada.Command_Line.Exit_Status);

end Cherry_Main;
