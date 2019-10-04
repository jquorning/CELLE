--
--  The author disclaims copyright to this source code.  In place of
--  a legal notice, here is a blessing:
--
--    May you do good and not evil.
--    May you find forgiveness for yourself and forgive others.
--    May you share freely, not taking more than you give.
--

package body Symbols is

   function "<" (Left  : in Symbol_Record;
                 Right : in Symbol_Record)
                return Boolean
   is
      function Value_Of (Item : in Symbol_Record) return Integer;

      function Value_Of (Item : in Symbol_Record) return Integer
      is
         Kind : constant Symbol_Kind := Item.Kind;
         Name : constant String      := To_String (Item.Name);
         Char : constant Character   := Name (Name'First);
      begin
         if Kind = Multi_Terminal then
            return 3;
         elsif Char > 'Z' then
            return 2;
         else
            return 1;
         end if;
      end Value_Of;

      I_Left  : constant Integer := Value_Of (Left);
      I_Right : constant Integer := Value_Of (Right);
   begin
      if I_Left = I_Right then
         return Left.Index - Right.Index > 0;
      else
         return I_Left - I_Right > 0;
      end if;
   end "<";



   procedure Symbol_Init is
   begin
      null;
   end Symbol_Init;



--     function Symbol_Array_Of return Symbol_Access_Array_Access;
--     --  Return an array of pointers to all data in the table.
--     --  The array is obtained from malloc.  Return NULL if memory allocation
--     --  problems, or if the array is empty.
   procedure Symbol_Allocate (Count : in Ada.Containers.Count_Type) is
   begin
      --  Symbol_Lists (Ada.Containers.Doubly_Linked_Lists) does not nave
      --  operation for setting length or capacity

      --  Symbol_Maps (Ada.Containers.Ordered_Maps does not nave
      --  operation for setting length or capacity

      null;
      --  Symbol_Lists.Set_Length (Extra.Symbol_Vector, Count);
      --  Symbol_Maps.
--        Extras.Symbol_Map := Symbols_Maps.Empty_Map;
--        for Index in
--          Symbol_Vectors.First (Extra.Symbol_Vector) ..
--          Symbol_Vectors.Last  (Extra.Symbol_Vector)
--        loop
--           Symbol_Maps.Replace_Element (Extras.Symbol_Map,
--        end loop;
   end Symbol_Allocate;


   --  2019-09-06 JQ
   --  Simply try to make a vector for symbols
   package Symbol_Bases is
      new Ada.Containers.Vectors (Index_Type   => Symbol_Index,
                                  Element_Type => Symbol_Access);
   Base : Symbol_Bases.Vector;


   procedure Count_Symbols_And_Terminals (Symbol_Count   : out Natural;
                                          Terminal_Count : out Natural)
   is
      Index : Symbol_Index;
   begin
      Symbol_Count   := Natural'First;
      Terminal_Count := Natural'First;

      --  Sequential index of symbols
      Index := 0;
      for Symbol of Base loop
         Symbol.all.Index := Index;
         Index := Index + 1;
      end loop;

      while Base (Index - 1).all.Kind = Multi_Terminal loop
         Index := Index - 1;
      end loop;

      pragma Assert (To_String (Base (Index - 1).Name) = "{default}");

      Symbol_Count := Natural (Index - 1);
      Index := 1;
      while
         To_String (Base (Index).all.Name) (1) in 'A' .. 'Z'
      loop
         Index := Index + 1;
      end loop;
      Terminal_Count := Natural (Index);
   end Count_Symbols_And_Terminals;


   function Create (Name : in String)
                   return Symbol_Access
   is
      Symbol : Symbol_Access := Find (Name);
   begin
      if Symbol = null then
         Symbol := new Symbol_Record;
         Symbol.Name        := To_Unbounded_String (Name);

         if Name (Name'First) in 'A' .. 'Z' then
            Symbol.Kind        := Terminal;
         else
            Symbol.Kind        := Non_Terminal;
         end if;

         Symbol.Rule        := null;
         Symbol.Fallback    := null;
         Symbol.Precedence  := -1;
         Symbol.Association := Unknown_Association;
         Symbol.First_Set   := Sets.Null_Set;
         Symbol.Lambda      := False;
         Symbol.Destructor  := Null_Unbounded_String;
         Symbol.Dest_Lineno := 0;
         Symbol.Data_Type   := Null_Unbounded_String;
         Symbol.Use_Count   := 0;
         Symbol_Bases.Append (Base, Symbol);
      end if;
      Symbol.Use_Count := Symbol.Use_Count + 1;
      return Symbol;
   end Create;


   function Find (Name : in String)
                 return Symbol_Access
   is
   begin
      for Symbol of Base loop
         if Symbol.all.Name = Name then
            return Symbol;
         end if;
      end loop;
      return null;
   end Find;


   function Symbol_Compare (Left  : in Symbol_Access;
                            Right : in Symbol_Access)
                           return Boolean;

   function Symbol_Compare (Left  : in Symbol_Access;
                            Right : in Symbol_Access)
                           return Boolean
   is
      L : Symbol_Record renames Left.all;
      R : Symbol_Record renames Right.all;

      L_I : constant Integer
        := (if L.Kind = Multi_Terminal then 3 else
             (if To_String (L.Name) (1) > 'Z' then 2 else 1));

      R_I : constant Integer
        := (if R.Kind = Multi_Terminal then 3 else
             (if To_String (R.Name) (1) > 'Z' then 2 else 1));
   begin
      if L_I = R_I then
         return L.Index < R.Index;
      else
         return L_I < R_I;
      end if;
   end Symbol_Compare;


   package Symbol_Sorting is
      new Symbol_Bases.Generic_Sorting (Symbol_Compare);


   procedure Sort is
      Index : Symbol_Index;
   begin
      --  Set index field in symbols in symbol table
      Index := 0;
      for Symbol of Base loop
         Symbol.all.Index := Index;
         Index := Index + 1;
      end loop;

      Symbol_Sorting.Sort (Base);
   end Sort;


   procedure Set_Lambda_False_And_Set_Firstset (First : in Natural;
                                                Last  : in Natural)
   is
   begin
      for Symbol of Base loop
         Symbol.Lambda := False;
      end loop;

      for I in First .. Last loop
         declare
            Symbol : constant Symbol_Access := Base.Element (Symbol_Index (I));
         begin
            Symbol.all.First_Set := Sets.Set_New;
            Base (Symbol_Index (I)) := Symbol;
         end;
      end loop;

   end Set_Lambda_False_And_Set_Firstset;


   function Last_Index return Symbol_Index is
   begin
      return Base.Last_Index;
   end Last_Index;


   function Element_At (Index : in Symbol_Index) return Symbol_Access
   is
   begin
      return Base.Element (Index);
   end Element_At;



end Symbols;

