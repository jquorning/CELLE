--
--  The author disclaims copyright to this source code.  In place of
--  a legal notice, here is a blessing:
--
--    May you do good and not evil.
--    May you find forgiveness for yourself and forgive others.
--    May you share freely, not taking more than you give.
--

with Interfaces.C;
with Interfaces.C.Strings;

with System;

with Rules;

package Symbols is

   use Interfaces.C;
   use Rules;


   type Symbol_Kind is
     (TERMINAL,
      NONTERMINAL,
      MULTITERMINAL);
   pragma Convention (C, Symbol_Kind);

   type E_Assoc is
     (LEFT,
      RIGHT,
      NONE,
      UNK);
   pragma Convention (C, E_Assoc);  -- lemon.h:58


   type Symbol_Record;
   type Symbol_Access is access all Symbol_Record;

   type Symbol_Index is new Natural;
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

   type Symbol_Record is
      record
         Name      : Strings.chars_ptr;
         Index     : Symbol_Index;      --  Index number for this symbol
         Kind      : Symbol_Kind;       --  Symbols are all either TERMINALS or NTs
         The_Rule  : Rule_Access;       --  Linked list of rules of this (if an NT)
         Fallback  : Symbol_Access;     --  fallback token in case this token doesn't parse
         Prec      : Integer;           --  Precedence if defined (-1 otherwise)
         Assoc     : E_Assoc;           --  Associativity if precedence is defined
         First_Set : Strings.chars_ptr; --  First-set for all rules of this symbol
         Lambda    : Boolean;           --  True if NT and can generate an empty string
         Use_Cnt   : Integer;           --  Number of times used

         Destructor  : Strings.chars_ptr;
         --  Code which executes whenever this symbol is
         --  popped from the stack during error processing

         Dest_Lineno : aliased int;
         --  Line number for start of destructor.  Set to
         --  -1 for duplicate destructors.

         Data_Type   : Strings.chars_ptr;
         --  The data type of information held by this
         --  object. Only used if type==NONTERMINAL

         Dt_Num      : aliased int;
         --  The data type number.  In the parser, the value
         --  stack is a union.  The .yy%d element of this
         --  union is the correct data type for this object

         B_Content   : aliased int;
         --  True if this symbol ever carries content - if
         --  it is ever more than just syntax

         N_Subsym    : aliased int;
         Subsym      : System.Address;
      end record;
   pragma Convention (C_Pass_By_Copy, Symbol_Record);

   --  The following fields are used by MULTITERMINALs only
   --  Number of constituent symbols in the MULTI
   --  Array of constituent symbols
   --  Each production rule in the grammar is stored in the following
   --  structure.


   procedure Do_Sort (Container : in out Symbol_Access_Array);

   --  Routines for handling symbols of the grammar

   function  Symbol_New (Name : in String) return Symbol_Access;
   procedure Symbol_New_Proc (Name : in String);
   --  int Symbolcmpp(const void *, const void *);
   procedure Symbol_Init;
   --  int Symbol_insert(struct symbol *, const char *);
   procedure Symbol_Insert (Symbol : in Symbol_Record;
                            Name   : in String);
   function Symbol_Find (Name : in String) return Symbol_Access;
   --  pragma Export (C, Symbol_Find, "symbols_symbol_find");

   function Symbol_Nth (Index : in Symbol_Index) return Symbol_Access;
   function Symbol_Count return Symbol_Index;
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

end Symbols;

