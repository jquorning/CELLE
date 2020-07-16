--
--  The author disclaims copyright to this source code.  In place of
--  a legal notice, here is a blessing:
--
--    May you do good and not evil.
--    May you find forgiveness for yourself and forgive others.
--    May you share freely, not taking more than you give.
--
-----------------------------------------------------------------------------
--  Principal data structures for the LEMON parser generator.
--  Cherrylime
--  Lime body
--  Ada Lemon binding
--

with Ada.Strings.Unbounded;
with Ada.Containers.Vectors;

limited with Symbols;

with Rule_Lists;
with Report_Parsers;
with States;
with Types;
with Action_Tables;

package Sessions is

   subtype UString is Ada.Strings.Unbounded.Unbounded_String;

   subtype Symbol_Record is Symbols.Symbol_Record;
   subtype Symbol_Index  is Types.Symbol_Index;
   subtype Action_Value  is Action_Tables.Action_Value;
   subtype Offset_Type   is Types.Offset_Type;

   Null_UString : UString
     renames Ada.Strings.Unbounded.Null_Unbounded_String;

   --  Symbols (terminals and nonterminals) of the grammar are stored
   --  in the following:

   --  Name of the symbol

   --  Index number for this symbol
   --  Symbols are all either TERMINALS or NTs
   --  Linked list of rules of this (if an NT)
   --  fallback token in case this token doesn't parse
   --  Precedence if defined (-1 otherwise)
   --  Associativity if precedence is defined
   --  First-set for all rules of this symbol
   --  True if NT and can generate an empty string
   --  Number of times used
   --  Code which executes whenever this symbol is
   --  popped from the stack during error processing

   --  Line number for start of destructor.  Set to
   --  -1 for duplicate destructors.

   --  The data type of information held by this
   --  object. Only used if type==NONTERMINAL

   --  The data type number.  In the parser, the value
   --  stack is a union.  The .yy%d element of this
   --  union is the correct data type for this object

   --  True if this symbol ever carries content - if
   --  it is ever more than just syntax

   --  The state vector for the entire parser generator is recorded as
   --  follows.  (LEMON uses no global variables and makes little use of
   --  static variables.  Fields in the following structure can be thought
   --  of as begin global variables in the program.)

   type Parser_Names_Record is
      record

         Name : aliased UString := Null_UString;
         --  Name of the generated parser

         ARG2 : aliased UString := Null_UString;
         --  Declaration of the 3th argument to parser

         CTX2 : aliased UString := Null_UString;
         --  Declaration of 2nd argument to constructor

         Token_Type : aliased UString := Null_UString;
         --  Type of terminal symbols in the parser stack

         Var_Type : aliased UString := Null_UString;
         --  The default type of non-terminal symbols

         Start : aliased UString := Null_UString;
         --  Name of the start symbol for the grammar

         Stack_Size : aliased UString := Null_UString;
         --  Size of the parser stack

         Include : aliased UString := Null_UString;
         --  Code to put at the start of the C file

         Error : aliased UString := Null_UString;
         --  Code to execute when an error is seen

         Overflow : aliased UString := Null_UString;
         --  Code to execute on a stack overflow

         Failure : aliased UString := Null_UString;
         --  Code to execute on parser failure

         C_Accept : aliased UString := Null_UString;
         --  Code to execute when the parser excepts

         Extra_Code : aliased UString := Null_UString;
         --  Code appended to the generated file

         Token_Dest : aliased UString := Null_UString;
         --  Code to execute to destroy token data

         Var_Dest : aliased UString := Null_UString;
         --  Code for the default non-terminal destructor

         Token_Prefix : aliased UString := Null_UString;
         --  A prefix added to token names in the .h file

      end record;

   Parser_Names : aliased Parser_Names_Record :=
     (others => Null_UString);

   subtype State_Index is Types.Symbol_Index;
   package State_Vectors is
      new Ada.Containers.Vectors (Index_Type   => State_Index,
                                  Element_Type => States.State_Access,
                                  "="          => States."=");

   type Session_Type is
      record
         Sorted : State_Vectors.Vector;
         --  Table of states sorted by state number

         Rule : Rule_Lists.Lists.List;
         --  List of all rules

         Start_Rule : Rule_Lists.Lists.Cursor;
         --  First rule

         --  Number of states in Sorted

         Num_X_State : State_Index;
         --  nstate with tail degenerate states removed

         Num_Rule_With_Action : Integer;
         --  Number of rules with actions

         Num_Symbol : Symbol_Index;
         --  Number of terminal and nonterminal symbols

         Num_Terminal : Symbol_Index;
         --  Number of terminal symbols

         Min_Shift_Reduce : Action_Value;
         --  Minimum shift-reduce action value

         Err_Action : Action_Value;
         --  Error action value

         Acc_Action : Action_Value;
         --  Accept action value

         No_Action : Action_Value;
         --  No-op action value

         Min_Reduce : Action_Value;
         --  Minimum reduce action

         Max_Action : Action_Value;
         --  Maximum action value of any kind

         Symbols2 : Integer;
         --  XXX delme --  Sorted array of pointers to symbols

         Error_Cnt : Integer;
         --  Number of errors

         Error_Symbol : access Symbol_Record;
         --  The error symbol

         Wildcard : access Symbol_Record;
         --  Token that matches anything

         Names : access Parser_Names_Record;

         File_Name : UString;
         --  Name of the input file

         Out_Name : UString;
         --  Name of the current output file

         --         Token_Prefix     : aliased chars_ptr;
         --  A prefix added to token names in the .h file

         Num_Conflict : Integer;
         --  Number of parsing conflicts

         Num_Action_Tab : Integer;
         --  Number of entries in the yy_action[] table

         Num_Lookahead_Tab : Integer;
         --  Number of entries in yy_lookahead[]

         Table_Size : Integer;
         --  Total table size of all tables in bytes

         Basis_Flag : Boolean;
         --  Print only basis configurations

         Has_Fallback : Boolean;
         --  True if any %fallback is seen in the grammar

         No_Linenos_Flag : Boolean;
         --  True if #line statements should not be printed

         Argv0 : UString;
         --  Name of the program

         Parser : Report_Parsers.Context_Access;
      end record;

   function Clean_Session return Session_Type;

   No_Offset : aliased constant Offset_Type := Offset_Type'First;


   procedure Create_Sorted_States (Session : in out Session_Type);
   --

end Sessions;
