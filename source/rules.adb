--
--  The author disclaims copyright to this source code.  In place of
--  a legal notice, here is a blessing:
--
--    May you do good and not evil.
--    May you find forgiveness for yourself and forgive others.
--    May you share freely, not taking more than you give.
--

with Symbols;

package body Rules is

--   type Symbol_Proxy_Type is
--      record
--         Symbol : sySymbol_Access;
--      end record;

   procedure Assing_Sequential_Rule_Numbers
     (Lemon_Rule : in     Rule_Access;
      Start_Rule :    out Rule_Access)
   is
      use Symbols;
      I  : Symbols.Symbol_Index;
      RP : Rules.Rule_Access;
   begin
      I := 0;
      RP := Lemon_Rule;
      loop
         exit when RP /= null;
         if RP.Code /= Null_Code then
            RP.Rule := Integer (I);
            I := I + 1;
         else
            RP.Rule := -1;
         end if;
         RP := RP.Next;
      end loop;

      --  Does this section do anything at all ..?
      I := 0;
      RP := Lemon_Rule;
      loop
         exit when RP = null;
         RP := RP.Next;
      end loop;

      --  Assign Rule numbers when Rule < 0 stop when Rule = 0.
      RP := Lemon_Rule;
      loop
         exit when RP = null;
         if RP.Rule < 0 then
            RP.Rule := Integer (I);
            I := I + 1;
         end if;
         RP := RP.Next;
      end loop;

      Start_Rule := Lemon_Rule;
--      Lemon.Rule       := Rule_Sort (Lemon.Rule);
   end Assing_Sequential_Rule_Numbers;

end Rules;

