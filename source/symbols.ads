--
--  The author disclaims copyright to this source code.  In place of
--  a legal notice, here is a blessing:
--
--    May you do good and not evil.
--    May you find forgiveness for yourself and forgive others.
--    May you share freely, not taking more than you give.
--

with Ada.Containers.Generic_Array_Sort;

with Interfaces.C;
with Interfaces.C.Strings;

with System;

with Rules;

package Symbols is

   type Symbol_Type is
     (TERMINAL,
      NONTERMINAL,
      MULTITERMINAL);
   pragma Convention (C, symbol_type);  -- lemon.h:52

   type E_Assoc is
     (LEFT,
      RIGHT,
      NONE,
      UNK);
   pragma Convention (C, e_assoc);  -- lemon.h:58

   use Interfaces.C;

   --  Name of the symbol
   --  type Rule_Record;

   use Rules;

   type Symbol_Record;
   type Symbol_Access is access all Symbol_Record;

   type Symbol_Index is new Natural;
   type Symbol_Array is array (Symbol_Index range <>) of Symbol_Type;

   type Symbol_Index_Array is
     array (Symbols.Symbol_Index range <>)
     of Symbol_Index;  --  Symbols.Symbol_Index;

   type Symbol_Access_Array is
     array (Symbols.Symbol_Index range <>)
     of Symbol_Access;  --  Symbols.Symbol_Index;

   type Symbol_Index_Array_Access  is access all Symbol_Index_Array;
   type Symbol_Access_Array_Access is access all Symbol_Access_Array;


   type Symbol_Record is
      record
         Name        : Interfaces.C.Strings.chars_ptr;  -- lemon.h:67
         Index       : aliased Symbol_Index; --  Int;         -- lemon.h:68
         C_Type      : Symbol_Type;       -- lemon.h:69
         The_Rule    : access Rule_Record;  -- lemon.h:70
         Fallback    : access Symbol_Record;  -- lemon.h:71
         Prec        : aliased int;  -- lemon.h:72
         Assoc       : aliased e_assoc;  -- lemon.h:73
         First_Set   : Interfaces.C.Strings.chars_ptr;  -- lemon.h:74
         Lambda      : aliased Boolean;  -- lemon.h:75
         Use_Cnt     : aliased int;  -- lemon.h:76
         Destructor  : Interfaces.C.Strings.chars_ptr;  -- lemon.h:77
         Dest_Lineno : aliased int;  -- lemon.h:79
         Data_Type   : Interfaces.C.Strings.chars_ptr;  -- lemon.h:81
         Dt_Num      : aliased int;  -- lemon.h:83
         B_Content   : aliased int;  -- lemon.h:86
         N_Subsym    : aliased int;  -- lemon.h:89
         Subsym      : System.Address;  -- lemon.h:90
      end record;
   pragma Convention (C_Pass_By_Copy, Symbol_Record);  -- lemon.h:66

  -- Index number for this symbol
  -- Symbols are all either TERMINALS or NTs
  -- Linked list of rules of this (if an NT)
  -- fallback token in case this token doesn't parse
  -- Precedence if defined (-1 otherwise)
  -- Associativity if precedence is defined
  -- First-set for all rules of this symbol
  -- True if NT and can generate an empty string
  -- Number of times used
  -- Code which executes whenever this symbol is
  --                           ** popped from the stack during error processing

  -- Line number for start of destructor.  Set to
  --                           ** -1 for duplicate destructors.

  -- The data type of information held by this
  --                           ** object. Only used if type==NONTERMINAL

  -- The data type number.  In the parser, the value
  --                           ** stack is a union.  The .yy%d element of this
  --                           ** union is the correct data type for this object

  -- True if this symbol ever carries content - if
  --                           ** it is ever more than just syntax

   --  The following fields are used by MULTITERMINALs only
   --  Number of constituent symbols in the MULTI
   --  Array of constituent symbols
   --  Each production rule in the grammar is stored in the following
   --  structure.


   procedure Sort (Container : in out Symbol_Access_Array);

   --  Routines for handling symbols of the grammar

   function  Symbol_New (Name : in String) return Symbol_Access;
   procedure Symbol_New (Name : in String);
   --  int Symbolcmpp(const void *, const void *);
   procedure Symbol_Init;
   --  int Symbol_insert(struct symbol *, const char *);
   procedure Symbol_Insert (Symbol : in Symbol_Record;
                            Name   : in String);
   function Symbol_Find (Name : in String) return Symbol_Access;
   pragma Export (C, Symbol_Find, "symbols_symbol_find");

   function Symbol_Nth (Index : in Symbol_Index) return Symbol_Access;
   function Symbol_Count return Symbol_Index;
   --  struct symbol **Symbol_arrayof(void);

end Symbols;

