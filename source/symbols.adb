--
--  The author disclaims copyright to this source code.  In place of
--  a legal notice, here is a blessing:
--
--    May you do good and not evil.
--    May you find forgiveness for yourself and forgive others.
--    May you share freely, not taking more than you give.
--

with Ada.Containers.Generic_Array_Sort;

package body Symbols is

   procedure Sort is
      new Ada.Containers.Generic_Array_Sort (Index_Type   => Symbol_Index,
                                             Element_Type => Symbol_Type);

end Symbols;

