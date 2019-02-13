--
--  The author disclaims copyright to this source code.  In place of
--  a legal notice, here is a blessing:
--
--    May you do good and not evil.
--    May you find forgiveness for yourself and forgive others.
--    May you share freely, not taking more than you give.
--

with Ada.Containers;
with Ada.Strings.Unbounded;

with Rules;

package Symbols is

   use Rules;

   type Symbol_Kind is
     (Terminal,
      Non_Terminal,
      Multi_Terminal);
   pragma Convention (C, Symbol_Kind);

   type E_Assoc is
     (Left,
      Right,
      None,
      Unk);
   pragma Convention (C, E_Assoc);

   use Ada.Strings.Unbounded;
   type Key_Type is new Unbounded_String;

   type Symbol_Index is new Natural;
<<<<<<< HEAD
   type Symbol_Array is array (Symbol_Index range <>) of Symbol_Kind;

   type Symbol_Index_Array is
     array (Symbols.Symbol_Index range <>)
     of Symbol_Index;

   type Symbol_Access_Array is
     array (Symbols.Symbol_Index range <>)
     of Symbol_Access;

   type Symbol_Index_Array_Access  is access all Symbol_Index_Array;
   type Symbol_Access_Array_Access is access all Symbol_Access_Array;
   pragma Convention (C, Symbol_Access_Array_Access);
=======
>>>>>>> symbols

   type Symbol_Record is
      record
         Name      : Key_Type; -- Unbounded_String; --  Strings.chars_ptr;
         Index     : Symbol_Index;      --  Index number for this symbol
         Kind      : Symbol_Kind;       --  Symbols are all either TERMINALS or NTs
         The_Rule  : Rule_Access;       --  Linked list of rules of this (if an NT)
         Fallback  : Unbounded_String;
         --  Symbol_Access; --  fallback token in case this token doesn't parse
         Prec      : Integer;           --  Precedence if defined (-1 otherwise)
         Assoc     : E_Assoc;           --  Associativity if precedence is defined
         First_Set : Unbounded_String;
         --  Strings.chars_ptr; --  First-set for all rules of this symbol
         Lambda    : Boolean;           --  True if NT and can generate an empty string
         Use_Cnt   : Integer;           --  Number of times used

         Destructor  : Unbounded_String; -- Strings.chars_ptr;
         --  Code which executes whenever this symbol is
         --  popped from the stack during error processing

         Dest_Lineno : Integer;
         --  Line number for start of destructor.  Set to
         --  -1 for duplicate destructors.

         Data_Type   : Unbounded_String; -- Strings.chars_ptr;
         --  The data type of information held by this
         --  object. Only used if type==NONTERMINAL

         Dt_Num      : Integer;
         --  The data type number.  In the parser, the value
         --  stack is a union.  The .yy%d element of this
         --  union is the correct data type for this object

         B_Content   : Integer;
         --  True if this symbol ever carries content - if
         --  it is ever more than just syntax

         N_Subsym    : Integer;
         Sub_Sym     : Unbounded_String; --  System.Address;
      end record;
   pragma Convention (C_Pass_By_Copy, Symbol_Record);

   --  The following fields are used by MULTITERMINALs only
   --  Number of constituent symbols in the MULTI
   --  Array of constituent symbols
   --  Each production rule in the grammar is stored in the following
   --  structure.


   type Symbol_Cursor is private;
   type Extra_Access  is private;

   function Get_Extra return Extra_Access;

   subtype Symbol_Name is Ada.Strings.Unbounded.Unbounded_String;


   --
   --  Routines for handling symbols of the grammar
   --

   function To_Key (Item : in String) return Key_Type;
   --  Make symbol name from plain string.

   procedure Set_Error;
   procedure Fill_And_Sort;
--   procedure Do_Sort (Container : in out Symbol_Access_Array);
   procedure Do_Some_Things (Lemon_N_Symbol : in out Symbol_Index);

   procedure Symbol_Append (Key      : in Key_Type;
                            New_Item : in Symbol_Record);
   procedure Symbol_Append (Key      : in String);

   --  function Symbol_New (Name : in String) return Symbol_Lists.Cursor;
   --  function  Symbol_New (Name : in Symbol_Name) return Symbol_Access;
   --   procedure Symbol_New_Proc (Name : in Symbol_Name);
   --  Return a pointer to the (terminal or nonterminal) symbol "x".
   --  Create a new symbol if this is the first time "x" has been seen.

   function "<" (Left  : in Symbol_Record;
                 Right : in Symbol_Record)
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


   procedure Symbol_Init;
   --  Allocate a new associative array.

   --  int Symbol_insert(struct symbol *, const char *);
   --  procedure Symbol_Insert (Symbol : in Symbol_Record;
   --                            Name   : in Symbol_Name);
   --  Insert a new record into the array.  Return TRUE if successful.
   --  Prior data with the same key is NOT overwritten


   function Symbol_Find (Key : in Key_Type) return Symbol_Cursor; -- Symbol_Access;
   function Symbol_Find (Key : in String) return Symbol_Cursor;
   --  Return a pointer to data assigned to the given key.  Return NULL
   --  if no such key.

   function Symbol_Nth (Index : in Symbol_Index) return Symbol_Cursor; -- Symbol_Access;
   --  Return the n-th data.  Return NULL if n is out of range.

   function Symbol_Count return Symbol_Index;
<<<<<<< HEAD
   --  struct symbol **Symbol_arrayof(void);
   function Symbol_Array_Of return Symbol_Access_Array_Access;

private
   pragma Import (C, Symbol_New,      "Symbol_new");
   pragma Import (C, Symbol_New_Proc, "Symbol_new");
   pragma Import (C, Symbol_Init,     "Symbol_init");
   pragma Import (C, Symbol_Insert,   "Symbol_insert");
   pragma Import (C, Symbol_Find,     "Symbol_find");
   pragma Import (C, Symbol_Nth,      "Symbol_nth");
   pragma Import (C, Symbol_Count,    "Symbol_count");
   pragma Import (C, Symbol_Array_Of, "Symbol_arrayof");
=======
   --  Return the size of the array.

   --  function Symbol_Array_Of return Symbol_Access_Array_Access;

   procedure Symbol_Allocate (Count : in Ada.Containers.Count_Type);
   --  Return an array of pointers to all data in the table.
   --  The array is obtained from malloc.  Return NULL if memory allocation
   --  problems, or if the array is empty.

private

   type Extra_Record;
   type Extra_Access is access all Extra_Record;

   type Cursor_Type;
   type Symbol_Cursor is access Cursor_Type;
>>>>>>> symbols

end Symbols;

