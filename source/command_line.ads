--
--  The author disclaims copyright to this source code.  In place of
--  a legal notice, here is a blessing:
--
--    May you do good and not evil.
--    May you find forgiveness for yourself and forgive others.
--    May you share freely, not taking more than you give.
--

package Command_Line is

   procedure Parse
     (Success : out Boolean);
   --  Parse command line setting variables.

   procedure Lemon_Entry_Function;
   pragma Import (C, Lemon_Entry_Function, "lemon_main");
   --  Lemon entry function.

end Command_Line;
