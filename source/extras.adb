--
--  The author disclaims copyright to this source code.  In place of
--  a legal notice, here is a blessing:
--
--    May you do good and not evil.
--    May you find forgiveness for yourself and forgive others.
--    May you share freely, not taking more than you give.
--

with Ada.Strings.Unbounded;
with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Ordered_Maps;

package body Extras is

   use type Symbols.Symbol_Record;
   use type Symbols.Symbol_Name;

   package Symbol_Lists is
      new Ada.Containers.Doubly_Linked_Lists
     (Element_Type => Symbols.Symbol_Record);

   package Symbol_Maps is
      new Ada.Containers.Ordered_Maps
     (Key_Type     => Symbols.Symbol_Name,
      Element_Type => Symbol_Cursor);

   function To_Cursor (Position : in Symbol_Cursor) return Symbol_Lists.Cursor;
   function From_Cursor (Cursor : in Symbol_Cursor) return Symbol_Lists.Cursor;
   function "=" (Left, Right : in Symbol_Cursor) return Boolean;
--   function Find (Name : in Symbols.Key_Type)
--                 return Symbol_Cursor;

   --
   --  Routines for handling symbols of the grammar
   --

   type Extra_Record is
      record
         Symbol_List : Symbol_Lists.List;  --  The Symbol_Record list
         Symbol_Map  : Symbol_Maps.Map;    --  Lookup from Symbol_Name
         Error       : Symbol_Cursor;
         Wildcard    : Symbol_Cursor;
      end record;

   type Cursor_Type is
      record
         C : Symbol_Lists.Cursor;
      end record;


   Extra_Base : aliased Extra_Record;
   --  The one and only Extra instance.


   function Get_Extra return Extra_Access is
   begin
      return Extra_Base'Access;
   end Get_Extra;


--     function Sorted_At (Extra : in Extra_Access;
--                         Index : in Symbols.Symbol_Index)
--                        return States.State_Access
--     is
--     begin
--        return null; --  XXX
--     end Sorted_At;

--     function Element_At (Extra : in Extra_Access;
--                          Index : in Symbols.Symbol_Index)
--                         return Symbols.Symbol_Access
--     is
--     begin
--        return null;  --  XXX
--     end Element_At;


   function Get_Wildcard (Extra : in Extra_Access)
                         return Symbols.Symbol_Access
   is
   begin
      return null;
   end Get_Wildcard;


   procedure Set_Wildcard (Extra    : in Extra_Access;
                           Wildcard : in Symbols.Symbol_Access)
   is
   begin
      null;
   end Set_Wildcard;


--     function Create (Name : in String)
--                     return Symbol_Cursor
--     is
--        use Symbols;

--        Element : Symbol_Record;
--     begin
--        Symbol_Append (Name);
--        return null;
--     end Create;


--     function Find (Name : in Symbols.Key_Type)
--                   return Symbol_Cursor
--     is
--        Position : Symbol_Maps.Cursor;
--     begin
--        Position :=
--          Symbol_Maps.Find (Extra_Base.Symbol_Map, Name);
--        --      Key => Symbols.From_Key (Key));
--        return null;  --  XXX
--     end Find;


--     function Find (Name : in String) return Symbol_Cursor  is
--        use Symbols;
--     begin
--        return Find (To_Key (Name));
--     end Find;


   function To_Cursor (Position : in Symbol_Cursor) return Symbol_Lists.Cursor
   is
   begin
      return Symbol_Lists.No_Element;
   end To_Cursor;


   function From_Cursor (Cursor : in Symbol_Cursor) return Symbol_Lists.Cursor
   is
   begin
      return Symbol_Lists.No_Element;
   end From_Cursor;


   function "=" (Left, Right : in Symbol_Cursor) return Boolean
   is
      use Ada.Strings.Unbounded;
      use Symbols;

      Element_Left  : constant Symbol_Record := Symbol_Lists.Element (From_Cursor (Left));
      Element_Right : constant Symbol_Record := Symbol_Lists.Element (From_Cursor (Right));
      Name_Left     : constant String        := To_String (Element_Left.Name);
      Name_Right    : constant String        := To_String (Element_Right.Name);
   begin
      return Name_Left = Name_Right;
   end "=";


   procedure Set_Error is
   begin
      null;
      --      Extra_Base.Error := Symbols.Find ("error");
--      Extra_Base.Error := Find ("error");
   end Set_Error;


--     procedure Fill_And_Sort
--     is
--        use Symbols;

--        Index : Symbol_Index := 0;
--     begin
--        for Symbol of Extra_Base.Symbol_List loop
--           Symbol.Index := Index; -- Lemon.Symbols.all (I).Index := I;
--           Index := Index + 1;
--        end loop;

--        --  pragma Assert (Symbol_Lists.Length (Extra.Symbol_List) =
--        --  Symbol_Maps.Length (Extra.Symbol_Map));
--        declare
--           use Ada.Containers;
--           Length_List : constant Count_Type := Symbol_Lists.Length (Extra_Base.Symbol_List);
--           Length_Map  : constant Count_Type := Symbol_Maps. Length (Extra_Base.Symbol_Map);
--        begin
--           if Length_List /= Length_Map then
--              raise Program_Error;
--           end if;
--        end;

--        --  Sort map/list.
--        declare
--           use Ada.Containers;
--     --  function Cursor_To_Cursor (Cursor : Symbol_Lists.Cursor) return Symbol_Maps.Cursor;
--           Length_List : constant Count_Type := Symbol_Lists.Length (Extra_Base.Symbol_List);
--           Indirect : array (1 .. Length_List) of Symbol_Cursor;
--           Running  : Symbol_Lists.Cursor :=
--             Symbol_Lists.First (Extra_Base.Symbol_List);
--        begin
--           for Index_2 in Indirect'Range loop
--              --  Indirect (Index) :=
--              --    Symbol_Lists.Element (Extra.Symbol_List,
--              --                          Position => To_Cursor (To_Cursor (Running)));
--              Running := Symbol_Lists.Next (Running);
--           end loop;
--        end;
--     end Fill_And_Sort;


--     procedure Symbol_Append (Key      : in Symbols.Key_Type;
--                              New_Item : in Symbols.Symbol_Record);

--     procedure Symbol_Append (Key      : in Symbols.Key_Type;
--                              New_Item : in Symbols.Symbol_Record)
--     is
--        First    : constant Symbol_Lists.Cursor := Symbol_Lists.First (Extra_Base.Symbol_List);
--        --      Element  : Symbol_Record;
--        Position : Symbol_Maps.Cursor;
--     begin
--        --  Insert payload record in list.
--  --      Symbol_Lists.Insert
--  --        (Container => Extra.Symbol_List,
--  --         Before    => First,
--  --         New_Item  => New_Item,
--  --         Position  => To_Cursor (Position));

--        --  Insert key to payload relation in map.
--  --      Symbol_Maps.Insert
--  --        (Container => Extra.Symbol_Map,
--  --         Key       => Unbounded_String (Key),
--  --        New_Item  => To_Cursor (To_Cursor (Position)));
--        null;
--     end Symbol_Append;


--     procedure Symbol_Append (Key : in String)
--     is
--        use Symbols;

--        Dummy_Element : Symbol_Record;
--     begin
--        Symbol_Append
--          (Key      => To_Key (Key),
--           New_Item => Dummy_Element);
--     end Symbol_Append;


   function Symbol_Count return Symbols.Symbol_Index
   is
      Count : Ada.Containers.Count_Type;
   begin
      Count := Symbol_Maps.Length (Extra_Base.Symbol_Map);
      return Symbols.Symbol_Index (Count);
   end Symbol_Count;


--     procedure JQ_Dump_Symbols is
--        use Ada.Text_IO;
--        use Ada.Strings.Unbounded;
--     begin
--        for Symbol of Get_Extra.Symbol_List loop
--           Put ("SYM ");
--           Put (To_String (Symbol.Name));
--           Put (ASCII.HT & "INDEX");
--           Put (Symbols.Symbol_Index'Image (Symbol.Index));
--           New_Line;
--        end loop;
--     end JQ_Dump_Symbols;


end Extras;
