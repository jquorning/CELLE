--
--  The author disclaims copyright to this source code.  In place of
--  a legal notice, here is a blessing:
--
--    May you do good and not evil.
--    May you find forgiveness for yourself and forgive others.
--    May you share freely, not taking more than you give.
--

with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Ordered_Maps;

package body Symbols is

   package Symbol_Lists
   is new Ada.Containers.Doubly_Linked_Lists
     (Element_Type => Symbol_Record);

   subtype Cursor is Symbol_Lists.Cursor;

   type Cursor_Type is
      record
         C : Cursor;
      end record;

   function "<" (Left, Right : in Symbol_Name) return Boolean;

   function "<" (Left, Right : in Symbol_Name) return Boolean is
--      use Ada.Strings.Unbounded;
      Length_Min : constant Natural := Natural'Min (Length (Left),
                                                    Length (Right));

      String_Left  : constant String := To_String (Left);
      String_Right : constant String := To_String (Right);
      S_Left       : constant String :=
        String_Left  (String_Left'First  .. String_Left'First  + Length_Min);
      S_Right      : constant String :=
        String_Right (String_Right'First .. String_Right'First + Length_Min);
   begin
      return (S_Left < S_Right);
   end "<";

   function "=" (Left, Right : in Symbol_Cursor) return Boolean;

   package Symbol_Maps
   is new Ada.Containers.Ordered_Maps
     (Key_Type     => Symbol_Name,
      Element_Type => Symbol_Cursor);


   --  function To_Cursor (Position : in Symbol_Maps.Cursor) return Symbol_Cursor;
   --  function To_Cursor (Position : in Symbol_Cursor) return Symbol_Maps.Cursor;
   function To_Cursor (Position : in Symbol_Cursor) return Symbol_Lists.Cursor;
   function From_Cursor (Cursor : in Symbol_Cursor) return Symbol_Lists.Cursor;
   function From_Key (Key : Key_Type) return Unbounded_String;

   --  function To_Cursor (Position : in Symbol_Maps.Cursor) return Symbol_Cursor;
   --  function To_Cursor (Position : in Symbol_Cursor) return Symbol_Maps.Cursor;
   function To_Cursor (Position : in Symbol_Cursor) return Symbol_Lists.Cursor
   is
--      use Symbol_Lists;
   begin
      return Symbol_Lists.No_Element;
   end To_Cursor;

   function From_Cursor (Cursor : in Symbol_Cursor) return Symbol_Lists.Cursor
   is
--      use Symbol_Lists;
   begin
      return Symbol_Lists.No_Element;
   end From_Cursor;

   function From_Key (Key : Key_Type) return Unbounded_String
   is
--      use Ada.Strings.Unbounded;
   begin
      return Null_Unbounded_String;
   end From_Key;


   function "=" (Left, Right : in Symbol_Cursor) return Boolean
   is
      use Interfaces.C.Strings;
      Element_Left  : constant Symbol_Record := Symbol_Lists.Element (From_Cursor (Left));
      Element_Right : constant Symbol_Record := Symbol_Lists.Element (From_Cursor (Right));
      Name_Left     : constant String := To_String (Element_Left.Name);
      Name_Right    : constant String := To_String (Element_Right.Name);
   begin
      return (Name_Left = Name_Right);
   end "=";

   --
   --  Routines for handling symbols of the grammar
   --

   type Extra_Record is
      record
         Symbol_List : Symbol_Lists.List;  --  The Symbol_Record list
         Symbol_Map  : Symbol_Maps.Map;    --  Lookup from Symbol_Name
         Error       : Symbol_Cursor;
         Wild_Card   : Symbol_Cursor;
      end record;

   Extra : aliased Extra_Record;
   --  The one and only Extra instance.


   function Get_Extra return Extra_Access is
   begin
      return Extra'Access;
   end Get_Extra;

   function Element_At (Extra : in Extra_Access;
                        Index : in Symbol_Index)
                       return Symbol_Access
   is
   begin
      return null;  --  XXX
   end Element_At;


   function Get_Wildcard (Extra : in Extra_Access)
                         return Symbol_Access
   is
   begin
      return null;
   end Get_Wildcard;


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


   procedure Set_Error is
   begin
      Extra.Error := Symbol_Find ("error");
   end Set_Error;


   procedure Fill_And_Sort is
      Index : Symbol_Index := 0;
   begin
      for I of Extra.Symbol_List loop
         I.Index := Index; -- Lemon.Symbols.all (I).Index := I;
         Index := Index + 1;
      end loop;

      --  pragma Assert (Symbol_Lists.Length (Extra.Symbol_List) =
      --  Symbol_Maps.Length (Extra.Symbol_Map));
      declare
         use Ada.Containers;
         Length_List : constant Count_Type := Symbol_Lists.Length (Extra.Symbol_List);
         Length_Map  : constant Count_Type := Symbol_Maps. Length (Extra.Symbol_Map);
      begin
         if Length_List /= Length_Map then
            raise Program_Error;
         end if;
      end;

      --  Sort map/list.
      declare
         use Ada.Containers;
         --  function Cursor_To_Cursor (Cursor : Symbol_Lists.Cursor) return Symbol_Maps.Cursor;
         Length_List : constant Count_Type := Symbol_Lists.Length (Extra.Symbol_List);
         Indirect : array (1 .. Length_List) of Symbol_Cursor;
         Running  : Symbol_Lists.Cursor :=
           Symbol_Lists.First (Extra.Symbol_List);
      begin
         for Index in Indirect'Range loop
            --  Indirect (Index) :=
            --    Symbol_Lists.Element (Extra.Symbol_List,
            --                          Position => To_Cursor (To_Cursor (Running)));
            Running := Symbol_Lists.Next (Running);
         end loop;
      end;
   end Fill_And_Sort;


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


   procedure Do_Some_Things (Lemon_N_Symbol : in out Symbol_Index)
   is
   begin
      null;
   end Do_Some_Things;


   procedure Symbol_Append (Key      : in Key_Type;
                            New_Item : in Symbol_Record)
   is
      First    : constant Symbol_Lists.Cursor := Symbol_Lists.First (Extra.Symbol_List);
      --      Element  : Symbol_Record;
      Position : Symbol_Maps.Cursor;
   begin
      --  Insert payload record in list.
--      Symbol_Lists.Insert
--        (Container => Extra.Symbol_List,
--         Before    => First,
--         New_Item  => New_Item,
--         Position  => To_Cursor (Position));

      --  Insert key to payload relation in map.
--      Symbol_Maps.Insert
--        (Container => Extra.Symbol_Map,
--         Key       => Unbounded_String (Key),
--        New_Item  => To_Cursor (To_Cursor (Position)));
      null;
   end Symbol_Append;

   procedure Symbol_Append (Key : in String)
   is
      Dummy_Element : Symbol_Record;
   begin
      Symbol_Append
        (Key      => To_Key (Key),
         New_Item => Dummy_Element);
   end Symbol_Append;

   function Symbol_New (Name : in String)
                       return Symbol_Cursor
   is
      Element : Symbol_Record;
   begin
      Symbol_Append (Name);
      return null;
   end Symbol_New;


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

      function Value_Of (Item : in Symbol_Record) return Integer is
         use Interfaces.C.Strings;
         Kind : constant Symbol_Kind := Item.Kind;
         Name : constant String      := To_String (Item.Name);
         Char : constant Character   := Name (Name'First);
      begin
         if Kind = Multi_Terminal then return 3;
         elsif Char > 'Z'         then return 2;
         else                          return 1;
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

   function Symbol_Find
     (Key : in Key_Type)
     return Symbol_Cursor
   is
      Position : Symbol_Maps.Cursor;
   begin
      Position :=
        Symbol_Maps.Find (Extra.Symbol_Map,
                          Key => From_Key (Key));
      return null;  --  XXX
   end Symbol_Find;


   function Symbol_Find (Key : in String) return Symbol_Cursor  is
   begin
      return Symbol_Find (To_Key (Key));
   end Symbol_Find;


   function Symbol_Nth (Index : in Symbol_Index)
                       return Symbol_Cursor
   is
      use Symbol_Maps;
      Element : Symbol_Name;
      Position : Symbol_Maps.Cursor := First (Extra.Symbol_Map);
      Count    : Symbol_Index := 0;
   begin
      for Count in 0 .. Index loop
         Position := Next (Position);
      end loop;
      return null;  --   To_Cursor (Position);
--      Element := Symbol_Maps.Element (Index); --  Extra.Symbol_Map);
--      return Element;
   end Symbol_Nth;


   function Symbol_Count return Symbol_Index
   is
      Count : Ada.Containers.Count_Type;
   begin
      Count := Symbol_Maps.Length (Extra.Symbol_Map);
      return Symbol_Index (Count);
   end Symbol_Count;


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


   function Lime_Symbol_New
     (Name : in Interfaces.C.Strings.chars_ptr)
     return Symbol_Access
   is
      use Interfaces.C.Strings;
      Cursor : Symbol_Cursor;
      Symbol : Symbol_Access;
   begin
      Cursor := Symbol_New (Value (Name));
      return Symbol;
   end Lime_Symbol_New;


   function Lime_Symbol_Find
     (Name : in Interfaces.C.Strings.chars_ptr)
     return Symbol_Access
   is
      use Interfaces.C.Strings;
      Cursor : Symbol_Cursor;
      Symbol : Symbol_Access;
   begin
      Cursor := Symbol_Find (Value (Name));
      return Symbol;
   end Lime_Symbol_Find;



end Symbols;

