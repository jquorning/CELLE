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
with Ada.Containers.Vectors;

with Interfaces.C.Strings;

limited with Rules;

package Symbols is

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

   type Symbol_Record;
   type Symbol_Access is access all Symbol_Record;

   package Symbol_Vectors is
      new Ada.Containers.Vectors
     (Positive,
      Symbol_Access);
   use Symbol_Vectors;

   use Ada.Strings.Unbounded;
   package Alias_Vectors is
      new Ada.Containers.Vectors
     (Positive,
      Unbounded_String);

   subtype S_Set is Unbounded_String;

   Null_Set : S_Set renames Null_Unbounded_String;

   function "=" (Left, Right : S_Set)
                return Boolean
     renames Ada.Strings.Unbounded."=";

   type Key_Type is new Unbounded_String;

   type Symbol_Index is new Natural;

   type Symbol_Record is
      record
         Name      : Key_Type;
         Index     : Symbol_Index;      --  Index number for this symbol
         Kind      : Symbol_Kind;       --  Symbols are all either TERMINALS or NTs
         Rule      : access Rules.Rule_Record; --  Linked list of rules of this (if an NT)
         Fallback  : access Symbol_Record;     --  fallback token in case this token doesn't parse
         Prec      : Integer;           --  Precedence if defined (-1 otherwise)
         Assoc     : E_Assoc;           --  Associativity if precedence is defined
         First_Set : Unbounded_String;  --  First-set for all rules of this symbol
         Lambda    : Boolean;           --  True if NT and can generate an empty string
         Use_Cnt   : Integer;           --  Number of times used

         Destructor  : aliased Unbounded_String;
         --  Code which executes whenever this symbol is
         --  popped from the stack during error processing

         Dest_Lineno : aliased Integer;
         --  Line number for start of destructor.  Set to
         --  -1 for duplicate destructors.

         Data_Type   : aliased Unbounded_String; -- Strings.chars_ptr;
         --  The data type of information held by this
         --  object. Only used if type==NONTERMINAL

         Dt_Num      : Integer;
         --  The data type number.  In the parser, the value
         --  stack is a union.  The .yy%d element of this
         --  union is the correct data type for this object

         Content : Boolean;
         --  True if this symbol ever carries content - if
         --  it is ever more than just syntax

         --  The following fields are used by MULTITERMINALs only
         N_Sub_Sym : Integer;  --  Number of constituent symbols in the MULTI
         Sub_Sym   : Vector;   --  Array of constituent symbols
      end record;


   type Symbol_Access_Array is
     array (Natural range <>) of Symbols.Symbol_Access;

   type Symbol_Cursor is private;
   type Extra_Access  is private;

   function Get_Extra return Extra_Access;

   function Element_At (Extra : in Extra_Access;
                        Index : in Symbol_Index)
                       return Symbol_Access;
   --  Get access to the symbol in Extra at position Index.

   function Get_Wildcard (Extra : in Extra_Access)
                         return Symbol_Access;

   procedure Set_Wildcard (Extra    : in Extra_Access;
                           Wildcard : in Symbol_Access);

   subtype Symbol_Name is Ada.Strings.Unbounded.Unbounded_String;

   --
   --  Routines for handling symbols of the grammar
   --

   function To_Key (Item : in String) return Key_Type;
   --  Make symbol name from plain string.

   function From_Key (Key : in Key_Type) return String;

   procedure Set_Error;
   procedure Fill_And_Sort;
--   procedure Do_Sort (Container : in out Symbol_Access_Array);
   procedure Do_Some_Things (Lemon_N_Symbol : in out Symbol_Index);


   procedure Symbol_Init;
   --  Allocate a new associative array.

   function Symbol_New (Name : in String) return Symbol_Cursor;
   --  Return a pointer to the (terminal or nonterminal) symbol "x".
   --  Create a new symbol if this is the first time "x" has been seen.

   function Symbol_Find (Key : in Key_Type) return Symbol_Cursor; -- Symbol_Access;
   function Symbol_Find (Key : in String) return Symbol_Cursor;
   --  Return a pointer to data assigned to the given key.  Return NULL
   --  if no such key.

   function Symbol_Nth (Index : in Symbol_Index)
                       return Symbol_Cursor; -- Symbol_Access;
   --  Return the n-th data.  Return NULL if n is out of range.

   function Symbol_Count return Symbol_Index;
   --  Return the size of the array.

   procedure Symbol_Append (Key      : in Key_Type;
                            New_Item : in Symbol_Record);
   procedure Symbol_Append (Key      : in String);

   procedure Symbol_Allocate (Count : in Ada.Containers.Count_Type);
   --  Return an array of pointers to all data in the table.
   --  The array is obtained from malloc.  Return NULL if memory allocation
   --  problems, or if the array is empty.

   function "<" (Left, Right  : in Symbol_Record)
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

   function Lime_Symbol_New
     (Name : in Interfaces.C.Strings.chars_ptr)
     return Symbol_Access;

   function Lime_Symbol_Find
     (Name : in Interfaces.C.Strings.chars_ptr)
     return Symbol_Access;

private

   type Extra_Record;
   type Extra_Access is access all Extra_Record;

   type Cursor_Type;
   type Symbol_Cursor is access Cursor_Type;

end Symbols;

