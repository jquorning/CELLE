--
--  The author disclaims copyright to this source code.  In place of
--  a legal notice, here is a blessing:
--
--    May you do good and not evil.
--    May you find forgiveness for yourself and forgive others.
--    May you share freely, not taking more than you give.
--

with Lime;
with Scanner_Data;

package Scanner_Parsers is

   procedure Do_State (Lemon   : in out Lime.Lemon_Record;
                       Scanner : in out Scanner_Data.Scanner_Record);
   --  The parser state machine

end Scanner_Parsers;
