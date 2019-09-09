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
     (Lemp   : in out Lime.Lemon_Record;
      Symbol : in out Symbols.Symbol_Access)
   is
      use Ada.Strings.Unbounded;
      use Symbols;
   begin
      if Lemp.Names.Start = "" then
         Symbol := Symbol_Access (Lemp.Start_Rule.LHS);
      else
         Symbol := Find (To_String (Lemp.Names.Start));
         if Symbol = null then
            Symbol := Symbol_Access (Lemp.Start_Rule.LHS);
         end if;
      end if;
   end Add_The_Accepting_Token;


   --
   --  Wrappers for Sets
   --

   function Set_New_C return Set_Type is
   begin
      return Sets.Set_New;
   end Set_New_C;


   procedure Set_Free_C (Set : in out Set_Type) is
   begin
      Sets.Set_Free (Set);
   end Set_Free_C;


   function Set_Add_C (Set  : in out Set_Type;
                       Item : in     Natural) return Integer
   is
   begin
      return Boolean'Pos (Sets.Set_Add (Set, Item));
   end Set_Add_C;


   function Set_Union_C (Set_1 : in out Set_Type;
                         Set_2 : in out Set_Type) return Integer
   is
   begin
      return Boolean'Pos (Sets.Set_Union (Set_1, Set_2));
   end Set_Union_C;


   function Set_Find_C (Set : in Set_Type;
                        Item : in Natural) return Integer
   is
   begin
      return Boolean'Pos (Sets.Set_Find (Set, Item));
   end Set_Find_C;


end Cherry;
