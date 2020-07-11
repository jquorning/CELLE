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

with Rules;
with Symbols;
with Report_Parsers;
with States;
with Types;
with Action_Tables;

package Sessions is

   use Symbols;
   use Ada.Strings.Unbounded;

   subtype Action_Value is Action_Tables.Action_Value;

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

   E : Unbounded_String renames Null_Unbounded_String;

   type Parser_Names_Record is record
      Name       : aliased Unbounded_String := E;
      --  Name of the generated parser

      ARG2       : aliased Unbounded_String := E;
      --  Declaration of the 3th argument to parser

      CTX2       : aliased Unbounded_String := E;
      --  Declaration of 2nd argument to constructor

      Token_Type : aliased Unbounded_String := E;
      --  Type of terminal symbols in the parser stack

      Var_Type   : aliased Unbounded_String := E;
      --  The default type of non-terminal symbols

      Start      : aliased Unbounded_String := E;
      --  Name of the start symbol for the grammar

      Stack_Size : aliased Unbounded_String := E;
      --  Size of the parser stack

      Include    : aliased Unbounded_String := E;
      --  Code to put at the start of the C file

      Error      : aliased Unbounded_String := E;
      --  Code to execute when an error is seen

      Overflow   : aliased Unbounded_String := E;
      --  Code to execute on a stack overflow

      Failure    : aliased Unbounded_String := E;
      --  Code to execute on parser failure

      C_Accept   : aliased Unbounded_String := E;
      --  Code to execute when the parser excepts

      Extra_Code : aliased Unbounded_String := E;
      --  Code appended to the generated file

      Token_Dest : aliased Unbounded_String := E;
      --  Code to execute to destroy token data

      Var_Dest   : aliased Unbounded_String := E;
      --  Code for the default non-terminal destructor

      Token_Prefix : aliased Unbounded_String := E;
      --  A prefix added to token names in the .h file

   end record;

   Parser_Names : aliased Parser_Names_Record :=
     (others => Null_Unbounded_String);

   subtype State_Index is Types.Symbol_Index;
   package State_Vectors is
      new Ada.Containers.Vectors (Index_Type   => State_Index,
                                  Element_Type => States.State_Access,
                                  "="          => States."=");

   type Session_Type is
      record
         Sorted : State_Vectors.Vector;
         --  Table of states sorted by state number

         Rule : Rules.Rule_Lists.List;
         --  List of all rules
         Start_Rule : Rules.Rule_Lists.Cursor;
         --  First rule

         --  Number of states in Sorted

         Nx_State : State_Index;
         --  nstate with tail degenerate states removed

         N_Symbol : Types.Symbol_Index;
         --  Number of terminal and nonterminal symbols

         N_Terminal : Types.Symbol_Index;
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

         Error_Symbol : Symbol_Access;
         --  The error symbol

         Wildcard : Symbol_Access;
         --  Token that matches anything

         Names : access Parser_Names_Record;

         File_Name : Unbounded_String;
         --  Name of the input file

         Out_Name : Unbounded_String;
         --  Name of the current output file

--         Token_Prefix     : aliased chars_ptr;  --  A prefix added to token names in the .h file

         N_Conflict : Integer;
         --  Number of parsing conflicts

         N_Action_Tab : Integer;
         --  Number of entries in the yy_action[] table

         N_Lookahead_Tab : Integer;
         --  Number of entries in yy_lookahead[]

         Table_Size : Integer;
         --  Total table size of all tables in bytes

         Basis_Flag : Boolean;
         --  Print only basis configurations

         Has_Fallback : Boolean;
         --  True if any %fallback is seen in the grammar

         No_Linenos_Flag : Boolean;
         --  True if #line statements should not be printed

         Argv0 : Unbounded_String;
         --  Name of the program

         Parser : Report_Parsers.Context_Access;
      end record;

   Clean_Session : constant Session_Type :=
     Session_Type'(Sorted       => State_Vectors.Empty_Vector,
                   Rule         => Rules.Rule_Lists.Empty_List,
                   Start_Rule   => Rules.Rule_Lists.No_Element,
                   Nx_State     => 0,
                   N_Symbol     => 0,          N_Terminal   => 0,         Min_Shift_Reduce => 0,
                   Err_Action   => 0,          Acc_Action   => 0,         No_Action        => 0,
                   Min_Reduce   => 0,          Max_Action   => 0,         Symbols2         => 999,
                   Error_Cnt    => 0,          Error_Symbol => null,      Wildcard         => null,
                   Names        => Parser_Names'Access,
                   File_Name    => Null_Unbounded_String,
                   Out_Name     => Null_Unbounded_String,
                   N_Conflict   => 0,        N_Action_Tab => 0,         N_Lookahead_Tab  => 0,
                   Table_Size   => 0,        Basis_Flag   => False,     Has_Fallback     => False,
                   No_Linenos_Flag => False, Argv0        => Null_Unbounded_String,
                   Parser          => Report_Parsers.Get_Context);

   No_Offset : aliased constant Integer := Integer'First;


   procedure Create_Sorted_States (Session : in out Session_Type);
   --

end Sessions;
