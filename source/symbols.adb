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

with Interfaces.C.Strings;

package body Symbols is

--   function "<" (Left, Right : in Symbol_Access)
--                return Boolean;

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

   procedure Dummy is
   begin
      null;
   end Dummy;

   -----------------------------------------------------------------------------

--  struct symbol *Symbol_new(const char *x)
--  {
--    struct symbol *sp;

--    sp = Symbol_find(x);
--    if( sp==0 ){
--      sp = (struct symbol *)calloc(1, sizeof(struct symbol) );
--      MemoryCheck(sp);
--      sp->name = Strsafe(x);
--      sp->type = ISUPPER(*x) ? TERMINAL : NONTERMINAL;
--      sp->rule = 0;
--      sp->fallback = 0;
--      sp->prec = -1;
--      sp->assoc = UNK;
--      sp->firstset = 0;
--      sp->lambda = LEMON_FALSE;
--      sp->destructor = 0;
--      sp->destLineno = 0;
--      sp->datatype = 0;
--      sp->useCnt = 0;
--      Symbol_insert(sp,sp->name);
--    }
--    sp->useCnt++;
--    return sp;
--  }

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



   --  There is one instance of the following structure for each
   --  associative array of type "x2".
--     type S_X2_Node;
--     type S_X2_Node_Access   is access all S_X2_Node;
--     type S_X2_Node_Access_2 is access all S_X2_Node_Access;

--     type S_X2_Record is
--        record
--           Size : Integer;
--           --  The number of available slots.
--           --  Must be a power of 2 greater than or
--           --  equal to 1

--           Count : Integer;   --  Number of currently slots filled
--           tbl   : S_X2_Node_Access;   --  The data stored here
--           ht    : S_X2_Node_Access_2; --  Hash table for lookups
--        end record;
--     type S_X2_Access is access all S_X2_Record;


--     type Key_Type is new Natural;


--     --  There is one instance of this structure for every data element
--     --  in an associative array of type "x2".
--     type S_X2_Node is
--        record
--           --  typedef struct s_x2node {
--           Data : Symbol_Access;       --  The data
--           Key  : Key_Type;             --  const char *key; --  The key
--           Next : S_X2_Node_Access;    --  Next entry with the same hash
--           From : S_X2_Node_Access_2;  --  Previous link

--           --  } x2node;
--        end record;

   --  There is only one instance of the array, which is the following
--   X2A : S_X2_Access;   --  static struct s_x2 *x2a;

--  /* Allocate a new associative array */
--  void Symbol_init(void){
--    if( x2a ) return;
--    x2a = (struct s_x2*)malloc( sizeof(struct s_x2) );
--    if( x2a ){
--      x2a->size = 128;
--      x2a->count = 0;
--      x2a->tbl = (x2node*)calloc(128, sizeof(x2node) + sizeof(x2node*));
--      if( x2a->tbl==0 ){
--        free(x2a);
--        x2a = 0;
--      }else{
--        int i;
--        x2a->ht = (x2node**)&(x2a->tbl[128]);
--        for(i=0; i<128; i++) x2a->ht[i] = 0;
--      }
--    }
--  }
--  /* Insert a new record into the array.  Return TRUE if successful.
--  ** Prior data with the same key is NOT overwritten */
--  int Symbol_insert(struct symbol *data, const char *key)
--  {
--    x2node *np;
--    unsigned h;
--    unsigned ph;

--    if( x2a==0 ) return 0;
--    ph = strhash(key);
--    h = ph & (x2a->size-1);
--    np = x2a->ht[h];
--    while( np ){
--      if( strcmp(np->key,key)==0 ){
--        /* An existing entry with the same key is found. */
--        /* Fail because overwrite is not allows. */
--        return 0;
--      }
--      np = np->next;
--    }
--    if( x2a->count>=x2a->size ){
--      /* Need to make the hash table bigger */
--      int i,arrSize;
--      struct s_x2 array;
--      array.size = arrSize = x2a->size*2;
--      array.count = x2a->count;
--      array.tbl = (x2node*)calloc(arrSize, sizeof(x2node) + sizeof(x2node*));
--      if( array.tbl==0 ) return 0;  /* Fail due to malloc failure */
--      array.ht = (x2node**)&(array.tbl[arrSize]);
--      for(i=0; i<arrSize; i++) array.ht[i] = 0;
--      for(i=0; i<x2a->count; i++){
--        x2node *oldnp, *newnp;
--        oldnp = &(x2a->tbl[i]);
--        h = strhash(oldnp->key) & (arrSize-1);
--        newnp = &(array.tbl[i]);
--        if( array.ht[h] ) array.ht[h]->from = &(newnp->next);
--        newnp->next = array.ht[h];
--        newnp->key = oldnp->key;
--        newnp->data = oldnp->data;
--        newnp->from = &(array.ht[h]);
--        array.ht[h] = newnp;
--      }
--      free(x2a->tbl);
--      *x2a = array;
--    }
--    /* Insert the new data */
--    h = ph & (x2a->size-1);
--    np = &(x2a->tbl[x2a->count++]);
--    np->key = key;
--    np->data = data;
--    if( x2a->ht[h] ) x2a->ht[h]->from = &(np->next);
--    np->next = x2a->ht[h];
--    x2a->ht[h] = np;
--    np->from = &(x2a->ht[h]);
--    return 1;
--  }

--  /* Return a pointer to data assigned to the given key.  Return NULL
--  ** if no such key. */
--  struct symbol *Symbol_find(const char *key)
--  {
--    unsigned h;
--    x2node *np;

--    if( x2a==0 ) return 0;
--    h = strhash(key) & (x2a->size-1);
--    np = x2a->ht[h];
--    while( np ){
--      if( strcmp(np->key,key)==0 ) break;
--      np = np->next;
--    }
--    return np ? np->data : 0;
--  }

--  /* Return the n-th data.  Return NULL if n is out of range. */
--  struct symbol *Symbol_Nth(int n)
--  {
--    struct symbol *data;
--    if( x2a && n>0 && n<=x2a->count ){
--      data = x2a->tbl[n-1].data;
--    }else{
--      data = 0;
--    }
--    return data;
--  }

--  /* Return the size of the array */
--  int Symbol_count()
--  {
--    return x2a ? x2a->count : 0;
--  }

--  /* Return an array of pointers to all data in the table.
--  ** The array is obtained from malloc.  Return NULL if memory allocation
--  ** problems, or if the array is empty. */
--  struct symbol **Symbol_arrayof()
--  {
--    struct symbol **array;
--    int i,arrSize;
--    if( x2a==0 ) return 0;
--    arrSize = x2a->count;
--    array = (struct symbol **)calloc(arrSize, sizeof(struct symbol *));
--    if( array ){
--      for(i=0; i<arrSize; i++) array[i] = x2a->tbl[i].data;
--    }
--    return array;
--  }





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
      use Ada.Strings.Unbounded;
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


   function "=" (Left, Right : in Symbol_Cursor) return Boolean
   is
      use Interfaces.C.Strings;
      Element_Left  : Symbol_Record renames Symbol_Lists.Element (Cursor_Type (Left));
      Element_Right : constant Symbol_Record := Symbol_Lists.Element (Cursor_Type (Right).C);
      Name_Left     : constant String := Value (Element_Left.Name);
      Name_Right    : constant String := Value (Element_Right.Name);
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
      end record;

   Extra : aliased Extra_Record;
   --  The one and only Extra instance.


   function Get_Extra return Extra_Access is
   begin
      return Extra'Access;
   end Get_Extra;


   function To_Name (Item : in String)
                    return Symbol_Name
   is
      use Ada.Strings.Unbounded;
   begin
      return To_Unbounded_String (Item);
   end To_Name;


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

      --  pragma Assert (Symbol_Lists.Length (Extra.Symbol_List) = Symbol_Maps.Length (Extra.Symbol_Map));
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
         Length_List : constant Count_Type := Symbol_Lists.Length (Extra.Symbol_List);
         Indirect : array (1 .. Length_List) of Symbol_Cursor;
         Running  : Symbol_Cursor := First (Extra.Symbol_List);
      begin
         for Index in Indirect'Range loop
            Indirect (Index) := Element (Extra.Symbol_List,
                                         Position => Running);
            Succ (Running)
         end loop;
      end;
   end Fill_And_Sort;


   Procedure Symbol_Append (Key      : in Symbol_Name;
                            New_Item : in Symbol_Record)
   is
      First    : constant List_Cursor := Symbol_Lists.First (Extra.Symbol_List);
      --      Element  : Symbol_Record;
      Position : Symbol_Lists.Cursor;
   begin
      --  Insert payload record in list.
      Symbol_Lists.Insert
        (Container => Extra.Symbol_List,
         Before    => First,
         New_Item  => New_Item,
         Position  => Position);

      --  Insert key to payload relation in map.
      Symbol_Maps.Insert
        (Container => Extra.Symbol_Map,
         Key       => Key,
        New_Item   => Position);
   end Symbol_Append;

   procedure Symbol_Append (Key : in String)
   is
      Dummy_Element : Symbol_Record;
   begin
      Symbol_Append
        (Key      => To_Name (Key),
         New_Item => Dummy_Element);
   end Symbol_Append;

--   function Symbol_New (Name : in String)
--                        return Symbol_Access

--     procedure Symbol_New_Proc (Name : in Symbol_Name)
--     is
--        Element : Symbol_Record;
--     begin
--        Symbol_Append (Name, Element);
--     end Symbol_New_Proc;


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
     (Key : in Symbol_Name)
     return Cursor
   is
      Position : Symbol_Maps.Cursor;
   begin
      Position :=
        Symbol_Maps.Find (Symbol_Map,
                          Key => To_Unbounded_String (Name));
      return null;  --  XXX
   end Symbol_Find;


   function Symbol_Nth (Index : in Symbol_Index)
                       return Symbol_Access
   is
      use Symbol_Maps;
      Element : Element_Type;
   begin
      Element := Element (Symbol_Map);
      return Element;
   end Symbol_Nth;


   function Symbol_Count return Symbol_Index
   is
      Count : Count_Type;
   begin
      Count := Symbol_Maps.Lenght (Symbol_Map);
      return Count;
   end Symbol_Count;


--     function Symbol_Array_Of return Symbol_Access_Array_Access;
--     --  Return an array of pointers to all data in the table.
--     --  The array is obtained from malloc.  Return NULL if memory allocation
--     --  problems, or if the array is empty.
   procedure Symbol_Allocate (Count : in Count_Type) is
   begin
      Symbol_Vectors.Set_Length (Extra.Symbol_Vector, Count);
--        Extras.Symbol_Map := Symbols_Maps.Empty_Map;
--        for Index in
--          Symbol_Vectors.First (Extra.Symbol_Vector) ..
--          Symbol_Vectors.Last  (Extra.Symbol_Vector)
--        loop
--           Symbol_Maps.Replace_Element (Extras.Symbol_Map,
--        end loop;
   end Symbol_Allocate;





end Symbols;

