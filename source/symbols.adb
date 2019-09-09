--
--  The author disclaims copyright to this source code.  In place of
--  a legal notice, here is a blessing:
--
--    May you do good and not evil.
--    May you find forgiveness for yourself and forgive others.
--    May you share freely, not taking more than you give.
--

with Ada.Text_IO;

package body Symbols is


--   subtype Cursor is Symbol_Lists.Cursor;


--     function "<" (Left, Right : in Symbol_Name) return Boolean;

--     function From_Key (Key : Key_Type) return Unbounded_String;


--     function "<" (Left, Right : in Symbol_Name) return Boolean
--     is
--        Length_Min   : constant Natural := Natural'Min (Length (Left),
--                                                        Length (Right));

--        String_Left  : constant String := To_String (Left);
--        String_Right : constant String := To_String (Right);
--        S_Left       : constant String :=
--          String_Left  (String_Left'First  .. String_Left'First  + Length_Min);
--        S_Right      : constant String :=
--          String_Right (String_Right'First .. String_Right'First + Length_Min);
--     begin
--        return (S_Left < S_Right);
--     end "<";


--     function From_Key (Key : Key_Type) return Unbounded_String
--     is
--     begin
--        return Null_Unbounded_String;
--     end From_Key;






   function To_Key (Item : in String) return Key_Type
   is
   begin
      return To_Unbounded_String (Item);
   end To_Key;


   function From_Key (Key : in Key_Type) return String
   is
   begin
      return To_String (Key);
   end From_Key;






--   procedure Sort is
--      new Ada.Containers.Generic_Array_Sort
--     (Index_Type   => Symbol_Index,
--      Element_Type => Symbol_Access,
--      Array_Type   => Symbol_Access_Array);

--      "<"          => Symbol_Compare);
--   procedure Do_Sort (Container : in out Symbol_Access_Array) is
--   begin
--      null;  --  Sort (Container);
--   end Do_Sort;


--     function "<" (Left, Right : in Symbol_Access)
--                  return Boolean
--     is
--        function Value_Of (Item : in Symbol_Access) return Integer;

--        function Value_Of (Item : in Symbol_Access) return Integer is
--           use Interfaces.C.Strings;
--           Kind : constant Symbol_Kind := Item.Kind;
--           Name : constant String      := Value (Item.Name);
--           Char : constant Character   := Name (Name'First);
--        begin
--           if Kind = MULTITERMINAL then return 3;
--           elsif Char > 'Z'        then return 2;
--           else                         return 1;
--           end if;
--        end Value_Of;

--        I_Left  : constant Integer := Value_Of (Left);
--        I_Right : constant Integer := Value_Of (Right);
--     begin
--        if I_Left = I_Right then
--           return (Left.Index - Right.Index) > 0;
--        else
--           return (I_Left - I_Right) > 0;
--        end if;
--     end "<";
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


--     procedure Symbol_Insert (Symbol : in Symbol_Record;
--                              Name   : in Symbol_Name)
--     is
--        Position : Symbol_Maps.Cursor;
--        Inserted : Boolean;
--     begin
--        Symbol_Maps.Insert
--          (Container => Extra.Symbol_Map,
--           Key       => Name, --  To_Unbounded_String (Name),
--           New_Item  => Symbol,
--           Position  => Position,
--           Inserted  => Inserted);
--     end Symbol_Insert;



--     function Symbol_Nth (Index : in Symbol_Index)
--                         return Symbol_Cursor
--     is
--        use Symbol_Maps;
--        Element : Symbol_Name;
--        Position : Symbol_Maps.Cursor := First (Extra.Symbol_Map);
--        Count    : Symbol_Index := 0;
--     begin
--        for Count in 0 .. Index loop
--           Position := Next (Position);
--        end loop;
--        return null;  --   To_Cursor (Position);
--  --      Element := Symbol_Maps.Element (Index); --  Extra.Symbol_Map);
--  --      return Element;
--     end Symbol_Nth;




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
      new Ada.Containers.Vectors (Index_Type   => Natural,
                                  Element_Type => Symbol_Access);
   Base : Symbol_Bases.Vector;


   procedure Count_Symbols_And_Terminals (Symbol_Count   : out Natural;
                                          Terminal_Count : out Natural)
   is
      Index : Natural;
   begin
      Symbol_Count   := Natural'First;
      Terminal_Count := Natural'First;

      --  Sequential index of symbols
      Index := 0;
      for Symbol of Base loop
         Symbol.all.Index := Symbol_Index (Index);
         Index := Index + 1;
      end loop;

      while Base (Index - 1).all.Kind = Multi_Terminal loop
         Index := Index - 1;
      end loop;

      pragma Assert (To_String (Base (Index - 1).Name) = "{default}");

      Symbol_Count := Index - 1;
      Index := 1;
      while
         To_String (Base (Index).all.Name) (1) in 'A' .. 'Z'
      loop
         Index := Index + 1;
      end loop;
      Terminal_Count := Index;
   end Count_Symbols_And_Terminals;


   function Create (Name : in String)
                   return Symbol_Access
   is
      Ptr : constant Symbol_Access := new Symbol_Record;
   begin
      Ptr.Name := To_Unbounded_String (Name);
      Symbol_Bases.Append (Base, Ptr);
      return Ptr;
   end Create;


   function Create_New (Name : in String)
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
         Symbol.Prec        := -1;
         Symbol.Assoc       := Unk;
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
   end Create_New;

--     is--     function Lime_Symbol_New
--       (Name : in Interfaces.C.Strings.chars_ptr)
--       return Symbol_Access
--     is
--        use Interfaces.C.Strings;
--        Cursor : Symbol_Cursor;
--        Symbol : Symbol_Access;
--     begin
--        Cursor := Symbol_New (Value (Name));
--        return Symbol;
--     end Lime_Symbol_New;


   function Find (Name : in String)
                 return Symbol_Access
   is
   begin
      for Symbol of Base loop
         if Symbol.all.Name = Name then
--            Ada.Text_IO.Put_Line ("Found Symbol Name: " & Name);
            return Symbol;
         end if;
      end loop;
      return null;
   end Find;

--     function Lime_Symbol_Find
--       (Name : in Interfaces.C.Strings.chars_ptr)
--       return Symbol_Access
--     is
--        use Interfaces.C.Strings;
--        Cursor : Symbol_Cursor;
--        Symbol : Symbol_Access;
--     begin
--        Cursor := Symbol_Find (Value (Name));
--        return Symbol;
--     end Lime_Symbol_Find;

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
            Symbol : constant Symbol_Access := Base.Element (I);
         begin
            Symbol.all.First_Set := Sets.Set_New;
            Base (I) := Symbol;
         end;
      end loop;

   end Set_Lambda_False_And_Set_Firstset;


   function Last_Index return Natural is
   begin
      return Base.Last_Index;
   end Last_Index;


   function Element_At (Index : in Natural) return Symbol_Access is
   begin
      return Base.Element (Index);
   end Element_At;


   --  Debug
   procedure JQ_Dump_Symbols is
      use Ada.Text_IO;
   begin
      for Symbol of Base loop
         Put ("SYM ");
         Put (To_String (Symbol.Name));
         Put (" INDEX");
         Put (Symbol_Index'Image (Symbol.Index));
         Put (" NSUB");
         Put (Symbol.Sub_Sym.Length'Img);
         Put (" KIND");
         Put (Symbol_Kind'Pos (Symbol.Kind)'Img);
         New_Line;
      end loop;
   end JQ_Dump_Symbols;


end Symbols;

