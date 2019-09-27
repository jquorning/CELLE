--
--  The author disclaims copyright to this source code.  In place of
--  a legal notice, here is a blessing:
--
--    May you do good and not evil.
--    May you find forgiveness for yourself and forgive others.
--    May you share freely, not taking more than you give.
--

with Symbols;

package Extras is


   type Extra_Access  is private;
   type Symbol_Cursor is private;


   function Get_Extra return Extra_Access;

--   function Element_At (Extra : in Extra_Access;
--                        Index : in Symbols.Symbol_Index)
--                       return Symbols.Symbol_Access;
   --  Get access to the symbol in Extra at position Index.

   function Get_Wildcard (Extra : in Extra_Access)
                         return Symbols.Symbol_Access;

   procedure Set_Wildcard (Extra    : in Extra_Access;
                           Wildcard : in Symbols.Symbol_Access);

--   function Sorted_At (Extra : in Extra_Access;
--                       Index : in Symbols.Symbol_Index)
--                      return States.State_Access;

--   function Create (Name : in String) return Symbol_Cursor;
   --  Create a new symbol with Name

   --   function Symbol_Find (Key : in Key_Type) return Symbol_Cursor;
--   function Find (Name : in String) return Symbol_Cursor;
   --  Find symbol wiht Name

--   procedure Symbol_Append (Key : in String);

   function Symbol_Count return Symbols.Symbol_Index;
   --  Return the size of the array.

   procedure Set_Error;
--   procedure Fill_And_Sort;

   --  Debug
--   procedure JQ_Dump_Symbols;

private

   type Extra_Record;
   type Extra_Access is access all Extra_Record;

   type Cursor_Type;
   type Symbol_Cursor is access Cursor_Type;

end Extras;
