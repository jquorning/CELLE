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

with Sets;
limited with Rules;

package Symbols is

   type Symbol_Kind is
     (Terminal,
      Non_Terminal,
      Multi_Terminal);
   pragma Convention (C, Symbol_Kind);

   type E_Assoc is
     (Left_Assoc,
      Right_Assoc,
      None,
      Unk);
   pragma Convention (C, E_Assoc);

   type Symbol_Record;
   type Symbol_Access is access all Symbol_Record;

   package Symbol_Vectors is
      new Ada.Containers.Vectors (Index_Type   => Positive,
                                  Element_Type => Symbol_Access);
   use Symbol_Vectors;

   use Ada.Strings.Unbounded;
   package Alias_Vectors is
      new Ada.Containers.Vectors (Index_Type   => Positive,
                                  Element_Type => Unbounded_String);

   type Symbol_Index is new Natural;

   type Symbol_Record is
      record
         Name      : Unbounded_String := Null_Unbounded_String;

         Index     : Symbol_Index   := 0;
         --  Index number for this symbol

         Kind      : Symbol_Kind    := Terminal;
         --  Symbols are all either TERMINALS or NTs

         Rule      : access Rules.Rule_Record := null;
         --  Linked list of rules of this (if an NT)

         Fallback  : access Symbol_Record     := null;
         --  fallback token in case this token doesn't parse

         Prec      : Integer          := 0;
         --  Precedence if defined (-1 otherwise)

         Assoc     : E_Assoc          := Left_Assoc;
         --  Associativity if precedence is defined

         First_Set : Sets.Set_Type := Sets.Null_Set;
         --  Unbounded_String := Null_Unbounded_String;
         --  First-set for all rules of this symbol

         Lambda    : Boolean          := False;
         --  True if NT and can generate an empty string

         Use_Count : Natural          := 0;
         --  Number of times used

         Destructor  : aliased Unbounded_String := Null_Unbounded_String;
         --  Code which executes whenever this symbol is
         --  popped from the stack during error processing

         Dest_Lineno : aliased Integer := 0;
         --  Line number for start of destructor.  Set to
         --  -1 for duplicate destructors.

         Data_Type   : aliased Unbounded_String := Null_Unbounded_String;
         --  The data type of information held by this
         --  object. Only used if type==NONTERMINAL

         Dt_Num      : Integer := 0;
         --  The data type number.  In the parser, the value
         --  stack is a union.  The .yy%d element of this
         --  union is the correct data type for this object

         Content : Boolean := False;
         --  True if this symbol ever carries content - if
         --  it is ever more than just syntax

         --
         --  The following fields are used by MULTITERMINALs only
         --

         Sub_Sym : Vector := Empty_Vector;
         --  Array of constituent symbols

      end record;


   type Symbol_Access_Array is
     array (Natural range <>) of Symbols.Symbol_Access;


   subtype Symbol_Name is Ada.Strings.Unbounded.Unbounded_String;

   --
   --  Routines for handling symbols of the grammar
   --

   function Name_Of (Symbol : in Symbol_Access) return String
   is (To_String (Symbol.Name));

   procedure Count_Symbols_And_Terminals (Symbol_Count   : out Natural;
                                          Terminal_Count : out Natural);
   --  Count the number of symbols and terminals.

   procedure Symbol_Init;
   --  Allocate a new associative array.

   function Create (Name : in String) return Symbol_Access;
   --  Return a pointer to the (terminal or nonterminal) symbol "x".
   --  Create a new symbol if this is the first time "x" has been seen.

   --  function Symbol_Find (Key : in Key_Type) return Symbol_Access;

   function Find (Name : in String) return Symbol_Access;
   --  Return a pointer to data assigned to the given key.  Return NULL
   --  if no such key.

--   function Symbol_Nth (Index : in Symbol_Index)
--                       return Symbol_Cursor; -- Symbol_Access;
   --  Return the n-th data.  Return NULL if n is out of range.

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

   function Create_New (Name : in String)
                       return Symbol_Access;
--   function Lime_Symbol_New
--     (Name : in Interfaces.C.Strings.chars_ptr)
--     return Symbol_Access;

--   function Lime_Symbol_Find
--     (Name : in Interfaces.C.Strings.chars_ptr)
--     return Symbol_Access;

   function Last_Index return Natural;
   --  get symbol last index.

   function Element_At (Index : in Natural) return Symbol_Access;
   --  Get symbol at Index position.


   procedure Sort;
   --  Sort the symbol table


   procedure Set_Lambda_False_And_Set_Firstset (First : in Natural;
                                                Last  : in Natural);
   --  Helper procedure split out of Builds.Find_First_Sets.
   --  Set all symbol lambda to False.
   --  New_Set to symbol in range First to Last.


   --  Debug
   procedure JQ_Dump_Symbols;

end Symbols;

