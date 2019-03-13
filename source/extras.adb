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
--   use type Symbols.Symbol_Cursor;

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


   Extra : aliased Extra_Record;
   --  The one and only Extra instance.


   function Get_Extra return Extra_Access is
   begin
      return Extra'Access;
   end Get_Extra;


   function Sorted_At (Extra : in Extra_Access;
                       Index : in Symbols.Symbol_Index)
                      return States.State_Access
   is
   begin
      return null; --  XXX
   end Sorted_At;

   function Element_At (Extra : in Extra_Access;
                        Index : in Symbols.Symbol_Index)
                       return Symbols.Symbol_Access
   is
   begin
      return null;  --  XXX
   end Element_At;


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


   function Symbol_New (Name : in String)
                       return Symbol_Cursor
   is
      use Symbols;

      Element : Symbol_Record;
   begin
      Symbol_Append (Name);
      return null;
   end Symbol_New;

   function Symbol_Find (Key : in Symbols.Key_Type)
                        return Symbol_Cursor;

   function Symbol_Find (Key : in Symbols.Key_Type)
                        return Symbol_Cursor
   is
      Position : Symbol_Maps.Cursor;
   begin
      Position :=
        Symbol_Maps.Find (Extra.Symbol_Map,
                          Key => Key);
      --      Key => Symbols.From_Key (Key));
      return null;  --  XXX
   end Symbol_Find;


   function Symbol_Find (Key : in String) return Symbol_Cursor  is
      use Symbols;
   begin
      return Symbol_Find (To_Key (Key));
   end Symbol_Find;


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
      return (Name_Left = Name_Right);
   end "=";


   procedure Set_Error is
   begin
      Extra.Error := Symbol_Find ("error");
   end Set_Error;


   procedure Fill_And_Sort
   is
      use Symbols;

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


   procedure Symbol_Append (Key      : in Symbols.Key_Type;
                            New_Item : in Symbols.Symbol_Record);

   procedure Symbol_Append (Key      : in Symbols.Key_Type;
                            New_Item : in Symbols.Symbol_Record)
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
      use Symbols;

      Dummy_Element : Symbol_Record;
   begin
      Symbol_Append
        (Key      => To_Key (Key),
         New_Item => Dummy_Element);
   end Symbol_Append;


   function Symbol_Count return Symbols.Symbol_Index
   is
      Count : Ada.Containers.Count_Type;
   begin
      Count := Symbol_Maps.Length (Extra.Symbol_Map);
      return Symbols.Symbol_Index (Count);
   end Symbol_Count;


end Extras;
