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

with Interfaces.C;
with Interfaces.C.Strings;

with Rules;
with Symbols;
with Parsers;

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

   --  The following fields are used by MULTITERMINALs only
   --  Number of constituent symbols in the MULTI
   --  Array of constituent symbols
   --  Each production rule in the grammar is stored in the following
   --  structure.


   --  Alias for the LHS (NULL if none)
   --  True if left-hand side is the start symbol
   --  Line number for the rule
   --  Number of RHS symbols
   --  The RHS symbols
   --  An alias for each RHS symbol (NULL if none)
   --  Line number at which code begins
   --  The code executed when this rule is reduced
   --  Setup code before code[] above
   --  Breakdown code after code[] above
   --  True if this rule has no associated C code
   --  True if the code has been emitted already
   --  Precedence symbol for this rule
   --  An index number for this rule
   --  Rule number as used in the generated tables
   --  True if this rule is ever reduced
   --  Reduce actions occur after optimization
   --  Next rule with the same LHS
   --  Next rule in the global list
   --  A configuration is a production rule of the grammar together with
   --  a mark (dot) showing how much of that rule has been processed so far.
   --  Configurations also contain a follow-set which is a list of terminal
   --  symbols which are allowed to immediately follow the end of the rule.
   --  Every configuration is recorded as an instance of the following:

   type cfgstatus is
     (COMPLETE,
      INCOMPLETE);
   pragma Convention (C, cfgstatus);  -- lemon.h:125

   use Interfaces.C;
   use Rules;


   type Plink_Record;
   type Plink_Access is access all Plink_Record;

   type State_Record;
   type State_Access is access all State_Record;

   type Config_Record;
   type Config_Access is access all Config_Record;

   type Config_Record is
      record
         RP          : access Rule_Record;   --  The rule upon which the configuration is based
         DOT         : aliased Integer;      --  The parse point
         Follow_Set  : Strings.chars_ptr;    --  FWS, Follow-set for this configuration only
         FS_Forward  : Plink_Access;         --  fplp, forward propagation links
         FS_Backward : Plink_Access;         --  bplp; Follow-set backwards propagation links
         stp         : access State_Record;  --  Pointer to state which contains this
         status      : aliased cfgstatus;    --  used during followset and shift computations
         Next        : Config_Access;        --  Next configuration in the state
         Basis       : Config_Access;        --  bp, The next basis configuration
      end record;
   pragma Convention (C_Pass_By_Copy, Config_Record);


   type e_action is
     (SHIFT,
      c_ACCEPT,
      REDUCE,
      ERROR,
      SSCONFLICT,
      SRCONFLICT,
      RRCONFLICT,
      SH_RESOLVED,
      RD_RESOLVED,
      NOT_USED,
      SHIFTREDUCE);
   pragma Convention (C, e_action);  -- lemon.h:141

   --  A shift/shift conflict
   --  Was a reduce, but part of a conflict
   --  Was a reduce, but part of a conflict
   --  Was a shift.  Precedence resolved conflict
   --  Was reduce.  Precedence resolved conflict
   --  Deleted by compression
   --  Shift first, then reduce
   --  Every shift or reduce operation is stored as one of the following
   --  The look-ahead symbol
   --  The new state, if a shift

   use Symbols;

   type Action_Record;
   type Action_Access is access all Action_Record;

   type anon1015_x_union (discr : unsigned := 0) is record
      case discr is
         when 0      => stp : access State_Record;  -- lemon.h:161
         when others => Rp  : access Rule_Record;  -- lemon.h:162
      end case;
   end record;
   pragma Convention (C_Pass_By_Copy, anon1015_x_union);
   pragma Unchecked_Union (anon1015_x_union);

   type Action_Record is
      record
         SP      : access Symbol_Kind;
         c_type  : aliased e_action;
         X       : aliased anon1015_x_union;  --  The rule, if a reduce
         spOpt   : access Symbol_Kind;        --  SHIFTREDUCE optimization to this symbol
         Next    : access Action_Record;      --  Next action for this state
         collide : access Action_Record;      --  Next action with the same hash
      end record;
   pragma Convention (C_Pass_By_Copy, Action_Record);


   --  Each state of the generated parser's finite state machine
   --  is encoded as an instance of the following structure.

   type State_Record is
      record
         BP          : Config_Access;        --  The basis configurations for this state
         CFP         : Config_Access;        --  All configurations in this set
         State_Num   : aliased Integer;      --  Sequential number for this state
         AP          : access Action_Record; --  List of actions for this state
         nTknAct     : aliased Integer;      --  Number of actions on terminals and nonterminals
         nNtAct      : aliased Integer;      --  yy_action[] offset for terminals and nonterms
         iTknOfst    : aliased Integer;      --  Default action is to REDUCE by this rule
         iNtOfst     : aliased Integer;      --  The default REDUCE rule.
         iDfltReduce : aliased Integer;      --  True if this is an auto-reduce state
         pDfltReduce : access Rule_Record;
         autoReduce  : aliased int;
      end record;

   function Sorted_Element_At (Extra : in Extra_Access;
                               Index : in Symbol_Index)
                              return State_Access;

   --  A followset propagation link indicates that the contents of one
   --  configuration followset should be propagated to another whenever
   --  the first changes.   pragma Convention (C_Pass_By_Copy, State_Record);

   type Plink_Record is
      record
         cfp  : Config_Access; --  The configuration to which linked
         next : Plink_Access;  --  The next propagate link
      end record;
   pragma Convention (C_Pass_By_Copy, Plink_Record);  -- lemon.h:188


   --  The state vector for the entire parser generator is recorded as
   --  follows.  (LEMON uses no global variables and makes little use of
   --  static variables.  Fields in the following structure can be thought
   --  of as begin global variables in the program.)

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
         Err_Sym2         : Integer; --  Symbol_Access;  --  The error symbol
         Wildcard2        : Integer; --  Symbol_Access;  --  Token that matches anything
         Name             : Strings.chars_ptr;  --  Name of the generated parser
         ARG2             : Strings.chars_ptr;  --  Declaration of the 3th argument to parser
         CTX2             : Strings.chars_ptr;  --  Declaration of 2nd argument to constructor
         Token_Type       : Strings.chars_ptr;  --  Type of terminal symbols in the parser stack
         Var_Type         : Strings.chars_ptr;  --  The default type of non-terminal symbols
         Start            : Strings.chars_ptr;  --  Name of the start symbol for the grammar
         Stack_Size       : Strings.chars_ptr;  --  Size of the parser stack
         Include          : Strings.chars_ptr;  --  Code to put at the start of the C file
         Error            : Strings.chars_ptr;  --  Code to execute when an error is seen
         Overflow         : Strings.chars_ptr;  --  Code to execute on a stack overflow
         Failure          : Strings.chars_ptr;  --  Code to execute on parser failure
         C_Accept         : Strings.chars_ptr;  --  Code to execute when the parser excepts
         Extra_Code       : Strings.chars_ptr;  --  Code appended to the generated file
         Token_Dest       : Strings.chars_ptr;  --  Code to execute to destroy token data
         Var_Dest         : Strings.chars_ptr;  --  Code for the default non-terminal destructor
         File_Name        : Strings.chars_ptr;  --  Name of the input file

         Out_Name        : chars_ptr;          --  Name of the current output file
         Token_Prefix    : Strings.chars_ptr;  --  A prefix added to token names in the .h file
         N_Conflict      : Integer;            --  Number of parsing conflicts
         N_Action_Tab    : Integer;            --  Number of entries in the yy_action[] table
         N_Lookahead_Tab : Integer;            --  Number of entries in yy_lookahead[]
         Table_Size      : Integer;            --  Total table size of all tables in bytes
         Basis_Flag      : Boolean;            --  Print only basis configurations
         Has_Fallback    : Boolean;            --  True if any %fallback is seen in the grammar
         No_Linenos_Flag : Boolean;            --  True if #line statements should not be printed
         Argv0           : Strings.chars_ptr;  --  Name of the program

         Extra           : Symbols.Extra_Access;
         Parser          : Parsers.Context_Access;
      end record;
   --  pragma Convention (C_Pass_By_Copy, Lemon_Record);

   Clean_Lemon : constant Lemon_Record :=
     (Sorted       => null,     Rule         => null,      Start_Rule       => null,
      N_State      => 0,        Nx_State     => 0,         N_Rule           => 0,
      N_Symbol     => 0,        N_Terminal   => 0,         Min_Shift_Reduce => 0,
      Err_Action   => 0,        Acc_Action   => 0,         No_Action        => 0,
      Min_Reduce   => 0,        Max_Action   => 0,         Symbols2         => 999, --  null,
      Error_Cnt    => 0,        Err_Sym2     => 999, --  null,
      Wildcard2    => 999, -- null,
      Name         => Null_Ptr, ARG2         => Null_Ptr,  CTX2             => Null_Ptr,
      Token_Type   => Null_Ptr, Var_Type     => Null_Ptr,  Start            => Null_Ptr,
      Stack_Size   => Null_Ptr, Include      => Null_Ptr,  Error            => Null_Ptr,
      Overflow     => Null_Ptr, Failure      => Null_Ptr,  C_Accept         => Null_Ptr,
      Extra_Code   => Null_Ptr, Token_Dest   => Null_Ptr,  Var_Dest         => Null_Ptr,
      File_Name    => Null_Ptr, Out_Name     => Null_Ptr,  Token_Prefix     => Null_Ptr,
      N_Conflict   => 0,        N_Action_Tab => 0,         N_Lookahead_Tab  => 0,
      Table_Size   => 0,        Basis_Flag   => False,     Has_Fallback     => False,
      No_Linenos_Flag => False, Argv0       => Null_Ptr,
      Extra           => Symbols.Get_Extra,
      Parser          => Parsers.Get_Context);

   ----------------------------------------------------------------------------
   --#define NO_OFFSET (-2147483647)
   NO_OFFSET : aliased long;  -- lemon.h:247
   pragma Import (C, NO_OFFSET, "NO_OFFSET");

   type lime_render_record is
      record
         Nxstate : aliased int;  -- lemon.h:372
         nrule : aliased int;  -- lemon.h:373
         nterminal : aliased int;  -- lemon.h:374
         minShiftReduce : aliased int;  -- lemon.h:375
         errAction : aliased int;  -- lemon.h:376
         accAction : aliased int;  -- lemon.h:377
         noAction : aliased int;  -- lemon.h:378
         minReduce : aliased int;  -- lemon.h:379
      end record;
   pragma Convention (C_Pass_By_Copy, lime_render_record);  -- lemon.h:370


   procedure Implementation_Open
     (File_Name : in Interfaces.C.Strings.chars_ptr);
   --  Open the out file.

   function Get_Token (Index : in Integer) return String;
   --  Get token for token symbol creation.

   procedure Open_If_Possible
     (File      : in out Ada.Text_IO.File_Type;
      File_Name : in     String;
      Success   :    out Boolean);
   --  Open File_Name to File. Success is True when success.

   procedure Template_Open
     (User_Template : in     chars_ptr;
      Error_Count   : in out Integer;
      Success       :    out Integer);
   --  Uses Context for File_Template.

   procedure Template_Transfer
     (Name : in chars_ptr);
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

   function Spec_Line_Callback
     (Index : in Integer)
     return chars_ptr;

   procedure Template_Print
     (Out_Name    : in chars_ptr;
      No_Line_Nos : in Integer;
      Include     : in chars_ptr);

   procedure Write_Include
     (Include_Name : in chars_ptr);

   procedure Generate_Tokens
     (Tokenprefix : in chars_ptr;
      First       : in Integer;
      Last        : in Integer);

   function Get_Token_Callback
     (Index : in Integer)
     return chars_ptr;

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
   type Render_Record is record
      Nxstate        : Integer;
      nrule          : Integer;
      nterminal      : Integer;
      minShiftReduce : Integer;
      errAction      : Integer;
      accAction      : Integer;
      noAction       : Integer;
      minReduce      : Integer;
   end record;
   pragma Convention (C, Render_Record);

   type Render_Access is access all Render_Record;
   pragma Convention (C, Render_Access);

   procedure Render_Constants
     (Render : in Render_Access);
   --
   --

   function Get_Acttab_YY_Action
     (I : in Integer)
     return Integer;

   procedure Write_Action_Table
     (N         : in Integer;
      No_Action : in Integer);
   --
   --

   function Get_Acttab_YY_Lookahead
     (I : in Integer)
     return Integer;

   procedure Write_YY_Lookahead
     (N       : in Integer;
      Nsymbol : in Integer);
   --
   --

   function Get_Token_Offset
     (I : in Integer)
     return Integer;

   procedure Write_YY_Shift_Offsets
     (N             : in Integer;
      MnTknOfst     : in Integer;
      MxTknOfst     : in Integer;
      Min_Size_Type : in chars_ptr;
      Nactiontab    : in Integer;
      NO_OFFSET     : in Integer);

   --
   --
   --

   function Get_NT_Offset
     (I : in Integer)
     return Integer;

   procedure Write_YY_Reduce_Offsets
     (N             : in Integer;
      MnNtOfst      : in Integer;
      MxNtOfst      : in Integer;
      Min_Size_Type : in chars_ptr;
      NO_OFFSET     : in Integer);

   --
   --
   --

   function Get_Default_Reduce
     (I : in Integer)
     return Integer;

   procedure Write_Default_Action_Table
     (N            : in Integer;
      Error_Action : in Integer;
      Min_Reduce   : in Integer);

   procedure Template_Print_2
     (Line        : in chars_ptr;
      No_Line_Nos : in Integer;
      Out_Name    : in chars_ptr);
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

   procedure Parse (Lemon : in Lemon_Record);

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

   pragma Export (C, Template_Open,       "lime_template_open");
   pragma Export (C, Implementation_Open, "lime_implementation_open");
   pragma Export (C, Template_Transfer,   "lime_template_transfer");
   pragma Export (C, Generate_Spec,       "lime_generate_spec");

   pragma Export (C, Template_Print,   "lime_print");
   pragma Export (C, Write_Include,    "lime_write_include");
   pragma Export (C, Generate_Tokens,  "lime_generate_tokens");
   pragma Import (C, Spec_Line_Callback,
                  "lemon_generate_header_line_callback");
   pragma Import (C, Get_Token_Callback,
                  "lime_get_token_callback");
   pragma Export (C, Generate_The_Defines_1, "lime_generate_the_defines_1");
   pragma Export (C, Generate_The_Defines_2, "lime_generate_the_defines_2");
   pragma Export (C, Error_Fallback,         "lime_error_fallback");
   pragma Export (C, Render_Constants,       "lime_render_constants");
   pragma Export (C, Write_Action_Table,     "lime_write_action_table");
   pragma Import (C, Get_Acttab_YY_Action,   "lime_get_acttab_yy_action");
   pragma Export (C, Write_YY_Lookahead,     "lime_write_yy_lookahead");
   pragma Import (C, Get_Acttab_YY_Lookahead, "lime_get_acttab_yy_lookahead");
   pragma Export (C, Write_YY_Shift_Offsets,  "lime_write_yy_shift_offsets");
   pragma Import (C, Get_Token_Offset,        "lime_get_token_offset");
   pragma Export (C, Write_YY_Reduce_Offsets, "lime_write_yy_reduce_offsets");
   pragma Import (C, Get_NT_Offset,           "lime_get_nt_offset");
   pragma Export (C, Write_Default_Action_Table, "lime_write_default_action_table");
   pragma Import (C, Get_Default_Reduce,         "lime_get_default_reduce");

   pragma Export (C, Template_Print_2,    "lime_template_print");

   pragma Export (C, Write_Arg_Defines, "lime_write_arg_defines");
   pragma Export (C, Close_Out,         "lime_close_out");
   pragma Export (C, Close_In,          "lime_close_in");

   pragma Export (C, Write_Interface,   "lime_write_interface");
   pragma Export (C, Write_Interface_Begin, "lime_write_interface_begin");
   pragma Export (C, Write_Interface_End,   "lime_write_interface_end");
   pragma Export (C, Report_Header,         "lime_report_header");
   pragma Export (C, Generate_Reprint_Of_Grammar,
                  "lime_generate_reprint_of_grammar");

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

   pragma Import (C, Parse,     "lemon_parse");
   pragma Import (C, Rule_Sort, "lime_rule_sort");

   pragma Export (C, Lime_Partial_Database_Dump_Ada,
                  "lime_partial_database_dump_ada");
   pragma Import (C, Lime_Partial_Database_Dump_C,
                  "lime_partial_database_dump_c");
end Lime;
