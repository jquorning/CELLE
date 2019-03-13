--
--  The author disclaims copyright to this source code.  In place of
--  a legal notice, here is a blessing:
--
--    May you do good and not evil.
--    May you find forgiveness for yourself and forgive others.
--    May you share freely, not taking more than you give.
--

with Symbols;
with States;

package Extras is


   function Sorted_At (Extra : in Symbols.Extra_Access;
                       Index : in Symbols.Symbol_Index)
                      return States.State_Access;


end Extras;
