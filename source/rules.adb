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


   procedure Assing_Sequential_Rule_Numbers
     (Lemon_Rule : in     Rule_Lists.List;
      Start_Rule :    out Rule_Lists.Cursor)
   is
      use type Symbols.Symbol_Index;

      I : Symbols.Symbol_Index;
   begin

      --  Assing .Rule

      I := 0;
      for Rule of Lemon_Rule loop
         if Rule.Code = Null_Code then
            Rule.Rule := -1;
         else
            Rule.Rule := Integer (I);
            I := I + 1;
         end if;
      end loop;

      --  Assign Rule numbers when Rule < 0 stop when Rule = 0.

      for Rule of Lemon_Rule loop
         if Rule.Rule < 0 then
            Rule.Rule := Integer (I);
            I := I + 1;
         end if;
      end loop;

      Start_Rule := Lemon_Rule.First;
   end Assing_Sequential_Rule_Numbers;


   function Less_Than (Left, Right : in Rule_Access) return Boolean;

   function Less_Than (Left, Right : in Rule_Access) return Boolean is
      (Left.Rule < Right.Rule);


   function Rule_Sort (Rule : in Rule_Lists.List) return Rule_Lists.List
   is
      package Rule_List_Sorting is
         new Rule_Lists.Generic_Sorting ("<" => Less_Than);

      Copy : Rule_Lists.List := Rule;
   begin
      Rule_List_Sorting.Sort (Copy);
      return Copy;
   end Rule_Sort;


end Rules;

