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
     (Lemon_Rule : in     Rule_Access;
      Start_Rule :    out Rule_Access)
   is
      use Symbols;

      I  : Symbols.Symbol_Index;
      RP : Rules.Rule_Access;
   begin

      --  Assing .Rule
      I := 0;
      RP := Lemon_Rule;
      loop
         exit when RP = null;
         if RP.Code /= Null_Code then
            RP.Rule := Integer (I);
            I := I + 1;
         else
            RP.Rule := -1;
         end if;
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
   end Assing_Sequential_Rule_Numbers;


   procedure Merge (Left   : in out Rule_Lists.List;
                    Right  : in out Rule_Lists.List;
                    Result :    out Rule_Lists.List)
   is
      use Rules.Rule_Lists;
   begin
      Result := Empty_List;
      while
        not Left.Is_Empty and
        not Right.Is_Empty
      loop
         if
           Left.First_Element.all.Rule <
           Right.First_Element.all.Rule
         then
            Result.Append (Left.First_Element);
            Left.Delete_First;
         else
            Result.Append (Right.First_Element);
            Right.Delete_First;
         end if;
      end loop;

      while not Left.Is_Empty loop
         Result.Append (Left.First_Element);
         Left.Delete_First;
      end loop;

      while not Right.Is_Empty loop
         Result.Append (Right.First_Element);
         Right.Delete_First;
      end loop;
   end Merge;

--  static struct rule *Rule_merge(struct rule *pA, struct rule *pB){
--    struct rule *pFirst = 0;
--    struct rule **ppPrev = &pFirst;
--    while( pA && pB ){
--      if( pA->iRule<pB->iRule ){
--        *ppPrev = pA;
--        ppPrev = &pA->next;
--        pA = pA->next;
--      }else{
--        *ppPrev = pB;
--        ppPrev = &pB->next;
--        pB = pB->next;
--      }
--    }
--    if( pA ){
--      *ppPrev = pA;
--    }else{
--      *ppPrev = pB;
--    }
--    return pFirst;
--  }


   function Rule_Sort (Rule : in Rule_Access) return Rule_Access
   is
   begin
      return Rule;

      --  struct rule *lime_rule_sort (struct rule *rp) */
--  { */
--    int i; */
--    struct rule *pNext; */
--    struct rule *x[32]; */
--    memset(x, 0, sizeof(x)); */
--    while( rp ){ */
--      pNext = rp->next; */
--      rp->next = 0; */
--      for(i=0; i<sizeof(x)/sizeof(x[0]) && x[i]; i++){ */
--        rp = Rule_merge(x[i], rp); */
--        x[i] = 0; */
--      } */
--      x[i] = rp; */
--      rp = pNext; */
--    } */
--    rp = 0; */
--    for(i=0; i<sizeof(x)/sizeof(x[0]); i++){ */
--      rp = Rule_merge(x[i], rp); */
--    } */
--    return rp; */
--  } */

   end Rule_Sort;


end Rules;

