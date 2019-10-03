--
--  The author disclaims copyright to this source code.  In place of
--  a legal notice, here is a blessing:
--
--    May you do good and not evil.
--    May you find forgiveness for yourself and forgive others.
--    May you share freely, not taking more than you give.
--

with Ada.Strings.Unbounded;

package body Cherry is


   procedure Add_The_Accepting_Token
     (Session : in out Sessions.Session_Type;
      Symbol  : in out Symbols.Symbol_Access)
   is
      use Ada.Strings.Unbounded;
      use Symbols;
   begin
      if Session.Names.Start = "" then
         Symbol := Symbol_Access (Session.Start_Rule.LHS);
      else
         Symbol := Find (To_String (Session.Names.Start));
         if Symbol = null then
            Symbol := Symbol_Access (Session.Start_Rule.LHS);
         end if;
      end if;
   end Add_The_Accepting_Token;


end Cherry;
