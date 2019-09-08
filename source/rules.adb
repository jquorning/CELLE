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


   function Merge (Pa : in Rule_Access;
                   Pb : in Rule_Access)
                  return Rule_Access
   is
      Pac   : Rule_Access := Pa;
      Pbc   : Rule_Access := Pb;
      Dummy : aliased Rule_Record;
      Tail  : Rule_Access := Dummy'Unchecked_Access;
   begin

      Dummy.Next := null;

      while
        Pac /= null and
        Pbc /= null
      loop

         if Pac.Rule < Pbc.Rule then
            --  Put first element of pac to tail
            declare
               Node : Rule_Access;
            begin
               Node      := Pac;       --  Node is the one to move
               Pac       := Pac.Next;  --  Advance pac to next in line
               Node.Next := null; --  Tail.Next; --  null??
               Tail.Next := Node;
               Tail      := Tail.Next;
            end;
         else
            --  Put first element of pbc to tail
            declare
               Node : Rule_Access;
            begin
               Node := Pbc;
               Pbc  := Pbc.Next;
               Node.Next := null;  --  Tail.Next;
               Tail.Next := Node;
               Tail      := Tail.Next;
            end;
         end if;

      end loop;

      if Pac = null then
         Tail.Next := Pbc;
      end if;

      if Pbc = null then
         Tail.Next := Pac;
      end if;

      return Dummy.Next;

   end Merge;


   function Rule_Sort (Rule : in Rule_Access) return Rule_Access
   is
      I_Copy : Integer;
      Next : Rule_Access;
      X    : array (0 .. 31) of Rule_Access := (others => null);
      Rule2 : Rule_Access := Rule;
   begin
      while Rule2 /= null loop
         Next := Rule2.Next;
         Rule2.Next := null;
         for I in X'Range loop
            I_Copy := I;
            exit when X (0) /= null;
            Rule2 := Merge (X (I), Rule2);
            X (I) := null;
         end loop;
         X (I_Copy) := Rule2;
         Rule2 := Next;
      end loop;
      Rule2 := null;
      for I in X'Range loop
         Rule2 := Merge (X (I), Rule2);
      end loop;
      return Rule2;
   end Rule_Sort;


end Rules;

