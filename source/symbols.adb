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

   function "<" (Left, Right : in Symbol_Access)
                return Boolean;

   --  Compare two symbols for sorting purposes.  Return negative,
   --  zero, or positive if a is less then, equal to, or greater
   --  than b.
   --
   --  Symbols that begin with upper case letters (terminals or tokens)
   --  must sort before symbols that begin with lower case letters
   --  (non-terminals).  And MULTITERMINAL symbols (created using the
   --  %token_class directive) must sort at the very end. Other than
   --  that, the order does not matter.
   --
   --  We find experimentally that leaving the symbols in their original
   --  order (the order they appeared in the grammar file) gives the
   --  smallest parser tables in SQLite.


   function "<" (Left, Right : in Symbol_Access)
                return Boolean
   is
      function Value_Of (Item : in Symbol_Access) return Integer;

      function Value_Of (Item : in Symbol_Access) return Integer is
         use Interfaces.C.Strings;
         Kind : constant Symbol_Kind := Item.Kind;
         Name : constant String      := Value (Item.Name);
         Char : constant Character   := Name (Name'First);
      begin
         if Kind = MULTITERMINAL then return 3;
         elsif Char > 'Z'        then return 2;
         else                         return 1;
         end if;
      end Value_Of;

      I_Left  : constant Integer := Value_Of (Left);
      I_Right : constant Integer := Value_Of (Right);
   begin
      if I_Left = I_Right then
         return (Left.Index - Right.Index) > 0;
      else
         return (I_Left - I_Right) > 0;
      end if;
   end "<";

   procedure Sort is
      new Ada.Containers.Generic_Array_Sort
     (Index_Type   => Symbol_Index,
      Element_Type => Symbol_Access,
      Array_Type   => Symbol_Access_Array);

--      "<"          => Symbol_Compare);
   procedure Do_Sort (Container : in out Symbol_Access_Array) is
   begin
      Sort (Container);
   end Do_Sort;

end Symbols;

