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

   procedure Preprocess (Buffer  : in out String;
                         Success :    out Boolean);
   --  Run the preprocessor over the input file text.  The macro names are defined
   --  to list by Append procedure above
   --  This routine looks for "%ifdef" and "%ifndef" and "%endif" and
   --  comments them out.  Text in between is also commented out as appropriate.

end Macros;
