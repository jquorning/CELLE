--
--  The author disclaims copyright to this source code.  In place of
--  a legal notice, here is a blessing:
--
--    May you do good and not evil.
--    May you find forgiveness for yourself and forgive others.
--    May you share freely, not taking more than you give.
--

package Macros is

   procedure Append (Name : in String);
   --  This routine is called with the argument to each -D command-line option.
   --  Add the macro defined to the azDefine array.

end Macros;
