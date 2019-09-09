--
--  The author disclaims copyright to this source code.  In place of
--  a legal notice, here is a blessing:
--
--    May you do good and not evil.
--    May you find forgiveness for yourself and forgive others.
--    May you share freely, not taking more than you give.
--

with Sessions;
with Symbols;
with Sets;

package Cherry is

   procedure Dummy is null;

   procedure Add_The_Accepting_Token
     (Session : in out Sessions.Session_Type;
      Symbol  : in out Symbols.Symbol_Access);
   --  Add the accepting Token.


   --
   --  Wrapper for Sets
   --

   use Sets;

   function Set_New_C return Set_Type;

   procedure Set_Free_C (Set : in out Set_Type);

   function Set_Add_C (Set  : in out Set_Type;
                       Item : in     Natural) return Integer;

   function Set_Union_C (Set_1 : in out Set_Type;
                         Set_2 : in out Set_Type) return Integer;

   function Set_Find_C (Set  : in Set_Type;
                        Item : in Natural) return Integer;

private

   pragma Export (C, Add_The_Accepting_Token,
                  "cherry_add_the_accepting_token");
   pragma Export (C, Set_New_C,   "lemon_set_new");
   pragma Export (C, Set_Free_C,  "lemon_set_free");
   pragma Export (C, Set_Add_C,   "lemon_set_add");
   pragma Export (C, Set_Union_C, "lemon_set_union");
   pragma Export (C, Set_Find_C,  "lemon_set_find");

end Cherry;
