--
--  The author disclaims copyright to this source code.  In place of
--  a legal notice, here is a blessing:
--
--    May you do good and not evil.
--    May you find forgiveness for yourself and forgive others.
--    May you share freely, not taking more than you give.
--
-----------------------------------------------------------------------------
--  Input file parser for the LEMON parser generator.
--

with Sessions;

package Parsers is

   procedure Parse (Session : in out Sessions.Session_Type);
   --  In spite of its name, this function is really a scanner.  It
   --  read in the entire input file (all at once) then tokenizes it.
   --  Each token is passed to the function "parseonetoken" which
   --  builds all the appropriate data structures in the global state
   --  vector "gp".

end Parsers;
