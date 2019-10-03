--
--  The author disclaims copyright to this source code.  In place of
--  a legal notice, here is a blessing:
--
--    May you do good and not evil.
--    May you find forgiveness for yourself and forgive others.
--    May you share freely, not taking more than you give.
--

with Sessions;
with Symbols;

package Cherry is

   procedure Add_The_Accepting_Token
     (Session : in out Sessions.Session_Type;
      Symbol  : in out Symbols.Symbol_Access);
   --  Add the accepting Token.

end Cherry;
