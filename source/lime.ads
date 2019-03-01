--
--  The author disclaims copyright to this source code.  In place of
--  a legal notice, here is a blessing:
--
--    May you do good and not evil.
--    May you find forgiveness for yourself and forgive others.
--    May you share freely, not taking more than you give.
--
-----------------------------------------------------------------------------
--  Cherrylime
--  Lime body
--  Ada Lemon binding
--

with Ada.Text_IO;
with Ada.Strings.Unbounded;

with Interfaces.C;
with Interfaces.C.Strings;

with Rules;
with Symbols;
with Parsers;
with Actions;
with States;

package Lime is

   use Interfaces.C.Strings;
   Lemon_Program_Name  : aliased chars_ptr := New_String ("");
   Lemon_Input_File    : aliased chars_ptr := New_String ("");
   Lemon_User_Template : aliased chars_ptr := New_String ("");
   Lemon_Output_Dir    : aliased chars_ptr := New_String ("");

   --********* From the file "struct.h" ************************************
   --** Principal data structures for the LEMON parser generator.
   --

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
   --                           ** popped from the stack during error processing

   --  Line number for start of destructor.  Set to
   --                           ** -1 for duplicate destructors.

   --  The data type of information held by this
   --                           ** object. Only used if type==NONTERMINAL

   --  The data type number.  In the parser, the value
   --                           ** stack is a union.  The .yy%d element of this
   --                           ** union is the correct data type for this object

   --  True if this symbol ever carries content - if
   --                           ** it is ever more than just syntax

   use Interfaces.C;
   use Rules;
   use Symbols;

   function Sorted_At (Extra : in Symbols.Extra_Access;
                       Index : in Symbol_Index)
                      return States.State_Access;

   --  The state vector for the entire parser generator is recorded as
   --  follows.  (LEMON uses no global variables and makes little use of
   --  static variables.  Fields in the following structure can be thought
   --  of as begin global variables in the program.)

   use Ada.Strings.Unbounded;
   type Lemon_Record is
      record
         Sorted           : Rule_Access;        --  Table of states sorted by state number
         Rule             : Rule_Access;        --  List of all rules
         Start_Rule       : Rule_Access;        --  First rule
         N_State          : Integer;            --  Number of states
         Nx_State         : Integer;            --  nstate with tail degenerate states removed
         N_Rule           : Integer;            --  Number of rules
         N_Symbol         : Symbol_Index;       --  Number of terminal and nonterminal symbols
         N_Terminal       : Symbol_Index;       --  Number of terminal symbols
         Min_Shift_Reduce : Integer;            --  Minimum shift-reduce action value
         Err_Action       : Integer;            --  Error action value
         Acc_Action       : Integer;            --  Accept action value
         No_Action        : Integer;            --  No-op action value
         Min_Reduce       : Integer;            --  Minimum reduce action
         Max_Action       : Integer;            --  Maximum action value of any kind
         Symbols2         : Integer; -- XXX delme --  Sorted array of pointers to symbols
         Error_Cnt        : Integer;            --  Number of errors
         Err_Sym2         : Integer; --  Symbol_Access;      --  The error symbol
         Wildcard2        : Integer; --  Symbol_Access;      --  Token that matches anything
         Name             : aliased chars_ptr;  --  Name of the generated parser
         ARG2             : aliased chars_ptr;  --  Declaration of the 3th argument to parser
         CTX2             : aliased chars_ptr;  --  Declaration of 2nd argument to constructor
         Token_Type       : aliased chars_ptr;  --  Type of terminal symbols in the parser stack
         Var_Type         : aliased chars_ptr;  --  The default type of non-terminal symbols
         Start            : aliased chars_ptr;  --  Name of the start symbol for the grammar
         Stack_Size       : aliased chars_ptr;  --  Size of the parser stack
         Include          : aliased chars_ptr;  --  Code to put at the start of the C file
         Error            : aliased chars_ptr;  --  Code to execute when an error is seen
         Overflow         : aliased chars_ptr;  --  Code to execute on a stack overflow
         Failure          : aliased chars_ptr;  --  Code to execute on parser failure
         C_Accept         : aliased chars_ptr;  --  Code to execute when the parser excepts
         Extra_Code       : aliased chars_ptr;  --  Code appended to the generated file
         Token_Dest       : aliased chars_ptr;  --  Code to execute to destroy token data
         Var_Dest         : aliased chars_ptr;  --  Code for the default non-terminal destructor
         File_Name        : Unbounded_String;   --  Name of the input file
         Out_Name         : Unbounded_String;   --  Name of the current output file
         Token_Prefix     : aliased chars_ptr;  --  A prefix added to token names in the .h file
         N_Conflict       : Integer;            --  Number of parsing conflicts
         N_Action_Tab     : Integer;            --  Number of entries in the yy_action[] table
         N_Lookahead_Tab  : Integer;            --  Number of entries in yy_lookahead[]
         Table_Size       : Integer;            --  Total table size of all tables in bytes
         Basis_Flag       : Boolean;            --  Print only basis configurations
         Has_Fallback     : Boolean;            --  True if any %fallback is seen in the grammar
         No_Linenos_Flag  : Boolean;            --  True if #line statements should not be printed
         Argv0            : Strings.chars_ptr;  --  Name of the program

         Extra            : Symbols.Extra_Access;
         Parser           : Parsers.Context_Access;
      end record;

   Clean_Lemon : constant Lemon_Record :=
     (Sorted       => null,       Rule         => null,      Start_Rule       => null,
      N_State      => 0,          Nx_State     => 0,         N_Rule           => 0,
      N_Symbol     => 0,          N_Terminal   => 0,         Min_Shift_Reduce => 0,
      Err_Action   => 0,          Acc_Action   => 0,         No_Action        => 0,
      Min_Reduce   => 0,          Max_Action   => 0,         Symbols2         => 999,
      Error_Cnt    => 0,          Err_Sym2     => 999,
      Wildcard2    => 999,
      Name         => Null_Ptr, ARG2         => Null_Ptr,  CTX2             => Null_Ptr,
      Token_Type   => Null_Ptr, Var_Type     => Null_Ptr,  Start            => Null_Ptr,
      Stack_Size   => Null_Ptr, Include      => Null_Ptr,  Error            => Null_Ptr,
      Overflow     => Null_Ptr, Failure      => Null_Ptr,  C_Accept         => Null_Ptr,
      Extra_Code   => Null_Ptr, Token_Dest   => Null_Ptr,  Var_Dest         => Null_Ptr,
      File_Name    => Null_Unbounded_String,
      Out_Name     => Null_Unbounded_String,
      Token_Prefix => Null_Ptr,
      N_Conflict   => 0,        N_Action_Tab => 0,         N_Lookahead_Tab  => 0,
      Table_Size   => 0,        Basis_Flag   => False,     Has_Fallback     => False,
      No_Linenos_Flag => False, Argv0       => Null_Ptr,
      Extra           => Symbols.Get_Extra,
      Parser          => Parsers.Get_Context);

   ----------------------------------------------------------------------------
   --  #define NO_OFFSET (-2147483647)
   --  NO_OFFSET : aliased long;  -- lemon.h:247
   --  pragma Import (C, NO_OFFSET, "NO_OFFSET");
   No_Offset : aliased Integer := Integer'First;


   procedure Implementation_Open (File_Name : in String);
   --  Open a file for writing then implementaion (parse.adb/parse.c).
   --  File handler is located in the context structure.

   --  function Get_Token (Index : in Integer) return String;
   --  Get token for token symbol creation.

   procedure Open_If_Possible
     (File      : in out Ada.Text_IO.File_Type;
      File_Name : in     String;
      Success   :    out Boolean);
   --  Open File_Name to File. Success is True when success.

   procedure Template_Open
     (User_Template : in     String;
      Error_Count   : in out Integer;
      Success       :    out Integer);
   --  Thisk function finds the template file and opens it. File handle
   --  is located in the context structure.

   procedure Template_Transfer (Name : in String);
   --

   procedure Generate_Spec
     (Base_Name : in chars_ptr;
      Prefix    : in chars_ptr; --  Prefix of symbols in spec
      Module    : in chars_ptr; --  Prefix of symbols in spec
      First     : in Integer;   --  Index of first symbol
      Last      : in Integer);  --  Index of last symbol
   --  Create spec file with name File_Name including symols found by
   --  iterating from First to Last calling callback prepended with
   --  Suffix.

--   function Spec_Line_Callback
--     (Index : in Integer)
--     return chars_ptr;

   procedure Template_Print
     (Out_Name    : in String;
      No_Line_Nos : in Integer;
      Include     : in chars_ptr);

   procedure Write_Include
     (Include_Name : in String);

   procedure Generate_Tokens
     (Token_Prefix : in chars_ptr;
      First        : in Integer;
      Last         : in Integer);

--   function Get_Token_Callback
--     (Index : in Integer)
--     return chars_ptr;

   procedure Generate_The_Defines_1
     (YY_Code_Type   : in chars_ptr;
      Symbol_Count   : in Integer;
      YY_Action_Type : in chars_ptr;
      Is_Wildcard    : in Boolean;
      Wildcard_Index : in Symbol_Index);

   procedure Generate_The_Defines_2
     (Stack_Size : in chars_ptr);

   type Mystruct_Record is record
      Use_Count : Integer;
      Index     : Integer;
      DT_Num    : Integer;
   end record;
   pragma Convention (C, Mystruct_Record);

   type Struct_Access is access all Mystruct_Record;
   pragma Convention (C, Struct_Access);

   procedure Error_Fallback
     (Error_Sym    : in chars_ptr;
      Struct       : in Struct_Access;
      Has_Fallback : in Integer);
   --
   --
   type Render_Record is
      record
         Nx_State         : Integer;
         N_Rule           : Integer;
         N_Terminal       : Integer;
         Min_Shift_Reduce : Integer;
         Err_Action       : Integer;
         Acc_Action       : Integer;
         No_Action        : Integer;
         Min_Reduce       : Integer;
      end record;

   type Render_Access is access all Render_Record;
   pragma Convention (C, Render_Access);

   procedure Render_Constants
     (Render : in Render_Record);
   --
   --

   procedure Output_Action_Table
     (Action_Table : in Actions.A_Action_Table;
      N            : in Integer;
      No_Action    : in Integer);
   --
   --

   procedure Output_YY_Lookahead
     (Action_Table : in Actions.A_Action_Table;
      N            : in Integer;
      Nsymbol      : in Integer);
   --
   --

   procedure Output_YY_Shift_Offsets
     (Lemp          : in Lime.Lemon_Record;
      N             : in Integer;
      MnTknOfst     : in Integer;
      MxTknOfst     : in Integer;
      Min_Size_Type : in chars_ptr;
      Nactiontab    : in Integer;
      NO_OFFSET     : in Integer);

   --
   --
   --

   procedure Output_YY_Reduce_Offsets
     (Lemp          : in Lime.Lemon_Record;
      N             : in Integer;
      MnNtOfst      : in Integer;
      MxNtOfst      : in Integer;
      Min_Size_Type : in chars_ptr;
      NO_OFFSET     : in Integer);


   procedure Output_Default_Action_Table
     (Lemp         : in Lime.Lemon_Record;
      N            : in Integer;
      Error_Action : in Integer;
      Min_Reduce   : in Integer);

   procedure Template_Print_2
     (Line        : in chars_ptr;
      No_Line_Nos : in Integer;
      Out_Name    : in String);
   --  Print a string to the file and keep the linenumber up to date

   procedure Write_Arg_Defines
     (Name    : in String;
      Arg_Ctx : in String;
      Extend  : in Boolean;
      Arg     : in String;
      Arg_I   : in String);

   procedure Close_Out;
   --  Close out file

   procedure Close_In;
   --  Close in file

   procedure Write_Interface
     (Name      : in chars_ptr;
      Tokentype : in chars_ptr);
   --

   procedure Write_Interface_Begin;
   procedure Write_Interface_End;

   procedure Report_Header
     (Token_Prefix  : in chars_ptr;
      Base_Name     : in chars_ptr;
      Module_Name   : in chars_ptr;
      Terminal_Last : in Natural);
   --  Generate a header file for the Parser.

   procedure Generate_Reprint_Of_Grammar
     (Base_Name     : in chars_ptr;
      Token_Prefix  : in chars_ptr;
      Terminal_Last : in Natural);
   --  Generate a reprint of the grammar, if requested on the command line.

   --
   --  Other way round specs
   --
   procedure Reprint (Lemon : in out Lemon_Record);
   procedure Set_Size (Size : in Natural);
   procedure Find_Rule_Precedences (Lemon : in Lemon_Record);
   procedure Find_First_Sets (Lemon : in Lemon_Record);
   procedure Compute_LR_States (Lemon : in Lemon_Record);
   procedure Find_Links (Lemon : in Lemon_Record);
   procedure Find_Follow_Sets (Lemon : in Lemon_Record);
   procedure Find_Actions (Lemon : in Lemon_Record);
   procedure Compress_Tables (Lemon : in Lemon_Record);
   procedure Resort_States (Lemon : in Lemon_Record);
   procedure Report_Output (Lemon : in Lemon_Record);
   procedure Report_Table (Lemon : in Lemon_Record);

   procedure Strsafe_Init;
   procedure State_Init;

   function Rule_Sort (Rule : in Rules.Rule_Access)
                      return Rules.Rule_Access;

   procedure Make_Copy_Of_Ada_Option_Strings;

   procedure Lime_Partial_Database_Dump_Ada;
   procedure Lime_Partial_Database_Dump_C;

private

   --  Option strings has char* brother
   pragma Export (C, Lemon_Program_Name,  "lemon_program_name");
   pragma Export (C, Lemon_Input_File,    "lemon_input_file");
   pragma Export (C, Lemon_User_Template, "lemon_user_template");
   pragma Export (C, Lemon_Output_Dir,    "lemon_output_dir");

   pragma Export (C, Generate_Spec,       "lime_generate_spec");

   pragma Export (C, Generate_Tokens,  "lime_generate_tokens");

   pragma Import (C, Reprint,               "lemon_reprint");
   pragma Import (C, Set_Size,              "lemon_set_size");
   pragma Import (C, Find_Rule_Precedences, "lemon_find_rule_precedences");
   pragma Import (C, Find_First_Sets,       "lemon_find_first_sets");
   pragma Import (C, Compute_LR_States,     "lemon_compute_LR_states");
   pragma Import (C, Find_Links,            "lemon_find_links");
   pragma Import (C, Find_Follow_Sets,      "lemon_find_follow_sets");
   pragma Import (C, Find_Actions,          "lemon_find_actions");
   pragma Import (C, Compress_Tables,       "lemon_compress_tables");
   pragma Import (C, Resort_States,         "lemon_resort_states");
   pragma Import (C, Report_Output,         "lemon_report_output");
   pragma Import (C, Report_Table,          "lemon_report_table");

   pragma Import (C, Strsafe_Init, "Strsafe_init");
   pragma Import (C, State_Init,   "State_init");

   pragma Import (C, Rule_Sort, "lime_rule_sort");

   pragma Export (C, Lime_Partial_Database_Dump_Ada,
                  "lime_partial_database_dump_ada");
   pragma Import (C, Lime_Partial_Database_Dump_C,
                  "lime_partial_database_dump_c");
end Lime;
