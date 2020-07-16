--
--  The author disclaims copyright to this source code.  In place of
--  a legal notice, here is a blessing:
--
--    May you do good and not evil.
--    May you find forgiveness for yourself and forgive others.
--    May you share freely, not taking more than you give.
--

package body Rules is


   procedure Assing_Sequential_Rule_Numbers (Rule_List : in out Rule_Lists.Lists.List)
   is
      use Types;

      Index : Symbol_Index;
   begin

      --  Assing .Rule

      Index := 0;
      for Rule of Rule_List loop
         if Rule.Code = Null_Code then
            Rule.Number := -1;
         else
            Rule.Number := Rule_Number (Index);
            Index := Index + 1;
         end if;
      end loop;

      --  Assign Rule numbers when Rule < 0 stop when Rule = 0.

      for Rule of Rule_List loop
         if Rule.Number < 0 then
            Rule.Number := Rule_Number (Index);
            Index := Index + 1;
         end if;
      end loop;

   end Assing_Sequential_Rule_Numbers;


   function Less_Than (Left, Right : in Rule_Lists.Rule_Access) return Boolean;

   function Less_Than (Left, Right : in Rule_Lists.Rule_Access) return Boolean is
      (Left.Number < Right.Number);


   procedure Rule_Sort (Rule_List : in out Rule_Lists.Lists.List)
   is
      package Rule_List_Sorting is
         new Rule_Lists.Lists.Generic_Sorting ("<" => Less_Than);
   begin
      Rule_List_Sorting.Sort (Rule_List);
   end Rule_Sort;


end Rules;

