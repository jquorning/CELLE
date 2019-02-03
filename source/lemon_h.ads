--
--  The author disclaims copyright to this source code.  In place of
--  a legal notice, here is a blessing:
--
--    May you do good and not evil.
--    May you find forgiveness for yourself and forgive others.
--    May you share freely, not taking more than you give.
--
--  pragma Ada_2005;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings;
--  with System;

with Symbols;
with Rules;

package Lemon_H is

  --**
  --**  lime.h
  --**
  --

   subtype boolean_t is int;  -- lemon.h:10

--   Lemon_Show_Conflict : aliased boolean_t;  -- lemon.h:11
--   pragma Import (C, lemon_show_conflict, "lemon_show_conflict");

--   Lemon_Show_Version : aliased boolean_t;  -- lemon.h:12
--   pragma Import (C, lemon_show_version, "lemon_show_version");

   Lemon_Basis_Flag : aliased boolean_t;  -- lemon.h:13
   pragma Import (C, lemon_basis_flag, "lemon_basis_flag");

--     Lemon_Compress : aliased boolean_t;  -- lemon.h:14
--     pragma Import (C, lemon_compress, "lemon_compress");

--    --extern boolean_t lemon_be_quiet;
   Lemon_Statistics : aliased boolean_t;  -- lemon.h:16
   pragma Import (C, lemon_statistics, "lemon_statistics");

   Lemon_No_Line_Nos : aliased boolean_t;  -- lemon.h:17
   pragma Import (C, lemon_no_line_nos, "lemon_no_line_nos");

--     Lemon_No_Resort : aliased boolean_t;  -- lemon.h:18
--     pragma Import (C, lemon_no_resort, "lemon_no_resort");

--     Lemon_Show_Help : aliased boolean_t;  -- lemon.h:19
--     pragma Import (C, lemon_show_help, "lemon_show_help");

--     subtype language_t is int;  -- lemon.h:21

--     LANGUAGE_DEFAULT : aliased language_t;  -- lemon.h:22
--     pragma Import (C, LANGUAGE_DEFAULT, "LANGUAGE_DEFAULT");

--     LANGUAGE_ADA : aliased language_t;  -- lemon.h:23
--     pragma Import (C, LANGUAGE_ADA, "LANGUAGE_ADA");

--     LANGUAGE_C : aliased language_t;  -- lemon.h:24
--     pragma Import (C, LANGUAGE_C, "LANGUAGE_C");

--     LANGUAGE_CPP : aliased language_t;  -- lemon.h:25
--     pragma Import (C, LANGUAGE_CPP, "LANGUAGE_CPP");

--     Lemon_Language : aliased language_t;  -- lemon.h:27
--     pragma Import (C, lemon_language, "lemon_language");

   Lemon_Program_Name : Interfaces.C.Strings.chars_ptr;  -- lemon.h:29
   pragma Import (C, lemon_program_name, "lemon_program_name");

   Lemon_Input_File : Interfaces.C.Strings.chars_ptr;  -- lemon.h:30
   pragma Import (C, lemon_input_file, "lemon_input_file");

--     Lemon_User_Template : Interfaces.C.Strings.chars_ptr;  -- lemon.h:31
--     pragma Import (C, lemon_user_template, "lemon_user_template");

--     Lemon_Output_Dir : Interfaces.C.Strings.chars_ptr;  -- lemon.h:32
--     pragma Import (C, lemon_output_dir, "lemon_output_dir");

  --** a few forward declarations...
  --

  --struct rule;
  --struct lemon;
  --struct action;
  --********* From the file "struct.h" ************************************
  --** Principal data structures for the LEMON parser generator.
  --

   type Boolean is
     (LEMON_FALSE,
      LEMON_TRUE);
   pragma Convention (C, Boolean);  -- lemon.h:47

  -- Symbols (terminals and nonterminals) of the grammar are stored
  --** in the following:

--     type symbol_type is
--       (TERMINAL,
--        NONTERMINAL,
--        MULTITERMINAL);
--     pragma Convention (C, symbol_type);  -- lemon.h:52

--     type e_assoc is
--       (LEFT,
--        RIGHT,
--        NONE,
--        UNK);
--     pragma Convention (C, e_assoc);  -- lemon.h:58

--    -- Name of the symbol
--     type rule;
--     type symbol is record
--        name : Interfaces.C.Strings.chars_ptr;  -- lemon.h:67
--        index : aliased int;  -- lemon.h:68
--        c_type : aliased symbol_type;  -- lemon.h:69
--        the_rule : access rule;  -- lemon.h:70
--        fallback : access symbol;  -- lemon.h:71
--        prec : aliased int;  -- lemon.h:72
--        assoc : aliased e_assoc;  -- lemon.h:73
--        firstset : Interfaces.C.Strings.chars_ptr;  -- lemon.h:74
--        lambda : aliased Boolean;  -- lemon.h:75
--        useCnt : aliased int;  -- lemon.h:76
--        destructor : Interfaces.C.Strings.chars_ptr;  -- lemon.h:77
--        destLineno : aliased int;  -- lemon.h:79
--        datatype : Interfaces.C.Strings.chars_ptr;  -- lemon.h:81
--        dtnum : aliased int;  -- lemon.h:83
--        bContent : aliased int;  -- lemon.h:86
--        nsubsym : aliased int;  -- lemon.h:89
--        subsym : System.Address;  -- lemon.h:90
--     end record;
--     pragma Convention (C_Pass_By_Copy, symbol);  -- lemon.h:66

--    -- Index number for this symbol
--    -- Symbols are all either TERMINALS or NTs
--    -- Linked list of rules of this (if an NT)
--    -- fallback token in case this token doesn't parse
--    -- Precedence if defined (-1 otherwise)
--    -- Associativity if precedence is defined
--    -- First-set for all rules of this symbol
--    -- True if NT and can generate an empty string
--    -- Number of times used
--    -- Code which executes whenever this symbol is
--    --                           ** popped from the stack during error processing

--    -- Line number for start of destructor.  Set to
--    --                           ** -1 for duplicate destructors.

--    -- The data type of information held by this
--    --                           ** object. Only used if type==NONTERMINAL

--    -- The data type number.  In the parser, the value
--    --                           ** stack is a union.  The .yy%d element of this
--    --                           ** union is the correct data type for this object

--    -- True if this symbol ever carries content - if
--    --                           ** it is ever more than just syntax

--    -- The following fields are used by MULTITERMINALs only
--    -- Number of constituent symbols in the MULTI
--    -- Array of constituent symbols
--    -- Each production rule in the grammar is stored in the following
--    --** structure.

   use Symbols;

--    -- Left-hand side of the rule
--     type rule is record
--        lhs : access Symbol_type;  -- lemon.h:97
--        lhsalias : Interfaces.C.Strings.chars_ptr;  -- lemon.h:98
--        lhsStart : aliased int;  -- lemon.h:99
--        ruleline : aliased int;  -- lemon.h:100
--        nrhs : aliased int;  -- lemon.h:101
--        rhs : System.Address;  -- lemon.h:102
--        rhsalias : System.Address;  -- lemon.h:103
--        line : aliased int;  -- lemon.h:104
--        code : Interfaces.C.Strings.chars_ptr;  -- lemon.h:105
--        codePrefix : Interfaces.C.Strings.chars_ptr;  -- lemon.h:106
--        codeSuffix : Interfaces.C.Strings.chars_ptr;  -- lemon.h:107
--        noCode : aliased int;  -- lemon.h:108
--        codeEmitted : aliased int;  -- lemon.h:109
--        precsym : access Symbol_type;  -- lemon.h:110
--        index : aliased int;  -- lemon.h:111
--        iRule : aliased int;  -- lemon.h:112
--        canReduce : aliased Boolean;  -- lemon.h:113
--        doesReduce : aliased Boolean;  -- lemon.h:114
--        nextlhs : access rule;  -- lemon.h:115
--        next : access rule;  -- lemon.h:116
--     end record;
--     pragma Convention (C_Pass_By_Copy, rule);  -- lemon.h:96

--    -- Alias for the LHS (NULL if none)
--    -- True if left-hand side is the start symbol
--    -- Line number for the rule
--    -- Number of RHS symbols
--    -- The RHS symbols
--    -- An alias for each RHS symbol (NULL if none)
--    -- Line number at which code begins
--    -- The code executed when this rule is reduced
--    -- Setup code before code[] above
--    -- Breakdown code after code[] above
--    -- True if this rule has no associated C code
--    -- True if the code has been emitted already
--    -- Precedence symbol for this rule
--    -- An index number for this rule
--    -- Rule number as used in the generated tables
--    -- True if this rule is ever reduced
--    -- Reduce actions occur after optimization
--    -- Next rule with the same LHS
--    -- Next rule in the global list
--    -- A configuration is a production rule of the grammar together with
--    --** a mark (dot) showing how much of that rule has been processed so far.
--    --** Configurations also contain a follow-set which is a list of terminal
--    --** symbols which are allowed to immediately follow the end of the rule.
--    --** Every configuration is recorded as an instance of the following:

   type cfgstatus is
     (COMPLETE,
      INCOMPLETE);
   pragma Convention (C, cfgstatus);  -- lemon.h:125

   use Rules;
  -- The rule upon which the configuration is based
   type plink;
   type state;
   type config is record
      rp : access Rule_Record;  -- lemon.h:130
      dot : aliased int;  -- lemon.h:131
      fws : Interfaces.C.Strings.chars_ptr;  -- lemon.h:132
      fplp : access plink;  -- lemon.h:133
      bplp : access plink;  -- lemon.h:134
      stp : access state;  -- lemon.h:135
      status : aliased cfgstatus;  -- lemon.h:136
      next : access config;  -- lemon.h:137
      bp : access config;  -- lemon.h:138
   end record;
   pragma Convention (C_Pass_By_Copy, config);  -- lemon.h:129

  -- The parse point
  -- Follow-set for this configuration only
  -- Follow-set forward propagation links
  -- Follow-set backwards propagation links
  -- Pointer to state which contains this
  -- used during followset and shift computations
  -- Next configuration in the state
  -- The next basis configuration
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

  -- A shift/shift conflict
  -- Was a reduce, but part of a conflict
  -- Was a reduce, but part of a conflict
  -- Was a shift.  Precedence resolved conflict
  -- Was reduce.  Precedence resolved conflict
  -- Deleted by compression
  -- Shift first, then reduce
  -- Every shift or reduce operation is stored as one of the following
  -- The look-ahead symbol
  -- The new state, if a shift
   type action;
   type anon1015_x_union (discr : unsigned := 0) is record
      case discr is
         when 0 =>
            stp : access state;  -- lemon.h:161
         when others =>
            rp : access Rule_Record;  -- lemon.h:162
      end case;
   end record;
   pragma Convention (C_Pass_By_Copy, anon1015_x_union);
   pragma Unchecked_Union (anon1015_x_union);type action is record
      sp : access Symbol_type;  -- lemon.h:158
      c_type : aliased e_action;  -- lemon.h:159
      x : aliased anon1015_x_union;  -- lemon.h:163
      spOpt : access SymboL_type;  -- lemon.h:164
      next : access action;  -- lemon.h:165
      collide : access action;  -- lemon.h:166
   end record;
   pragma Convention (C_Pass_By_Copy, action);  -- lemon.h:157

  -- The rule, if a reduce
  -- SHIFTREDUCE optimization to this symbol
  -- Next action for this state
  -- Next action with the same hash
  -- Each state of the generated parser's finite state machine
  --** is encoded as an instance of the following structure.

  -- The basis configurations for this state
   type state is record
      bp : access config;  -- lemon.h:173
      cfp : access config;  -- lemon.h:174
      statenum : aliased int;  -- lemon.h:175
      ap : access action;  -- lemon.h:176
      nTknAct : aliased int;  -- lemon.h:177
      nNtAct : aliased int;  -- lemon.h:177
      iTknOfst : aliased int;  -- lemon.h:178
      iNtOfst : aliased int;  -- lemon.h:178
      iDfltReduce : aliased int;  -- lemon.h:179
      pDfltReduce : access Rule_Record;  -- lemon.h:180
      autoReduce : aliased int;  -- lemon.h:181
   end record;
   pragma Convention (C_Pass_By_Copy, state);  -- lemon.h:172

  -- All configurations in this set
  -- Sequential number for this state
  -- List of actions for this state
  -- Number of actions on terminals and nonterminals
  -- yy_action[] offset for terminals and nonterms
  -- Default action is to REDUCE by this rule
  -- The default REDUCE rule.
  -- True if this is an auto-reduce state
  -- A followset propagation link indicates that the contents of one
  --** configuration followset should be propagated to another whenever
  --** the first changes.

  -- The configuration to which linked
   type plink is record
      cfp : access config;  -- lemon.h:189
      next : access plink;  -- lemon.h:190
   end record;
   pragma Convention (C_Pass_By_Copy, plink);  -- lemon.h:188

  -- The next propagate link
  -- The state vector for the entire parser generator is recorded as
  --** follows.  (LEMON uses no global variables and makes little use of
  --** static variables.  Fields in the following structure can be thought
  --** of as begin global variables in the program.)

   -- Table of states sorted by state number
   type Lemon_Record is
      record
         Sorted     : Rule_Access; --  System.Address;  -- lemon.h:199
         Rule       : Rule_Access;     -- lemon.h:200
         Start_Rule : Rule_Access;     -- lemon.h:201
         N_State    : aliased int;  -- lemon.h:202
         Nx_State   : aliased int;  -- lemon.h:203
         N_Rule     : aliased int;  -- lemon.h:204
         N_Symbol   : aliased Symbol_Index;  -- lemon.h:205
         N_Terminal : aliased Symbol_Index;  -- lemon.h:206
         Min_Shift_Reduce : aliased int;  -- lemon.h:207
         Err_Action   : aliased int;  -- lemon.h:208
         Acc_Action   : aliased int;  -- lemon.h:209
         No_Action    : aliased int;  -- lemon.h:210
         Min_Reduce   : aliased int;  -- lemon.h:211
         Max_Action   : aliased int;  -- lemon.h:212
         Symbols      : Symbol_Access_Array_Access;  -- System.Address;  -- lemon.h:213
         Error_Cnt    : aliased int;  -- lemon.h:214
         Err_Sym      : Symbol_Access;  -- lemon.h:215
         Wildcard     : Symbol_Access;  -- lemon.h:216
         Name         : Interfaces.C.Strings.chars_ptr;  -- lemon.h:217
         Arg          : Interfaces.C.Strings.chars_ptr;  -- lemon.h:218
         Ctx          : Interfaces.C.Strings.chars_ptr;  -- lemon.h:219
         Token_Type   : Interfaces.C.Strings.chars_ptr;  -- lemon.h:220
         Var_Type     : Interfaces.C.Strings.chars_ptr;  -- lemon.h:221
         Start        : Interfaces.C.Strings.chars_ptr;  -- lemon.h:222
         Stack_Size   : Interfaces.C.Strings.chars_ptr;  -- lemon.h:223
         Include      : Interfaces.C.Strings.chars_ptr;  -- lemon.h:224
         Error        : Interfaces.C.Strings.chars_ptr;  -- lemon.h:225
         Overflow     : Interfaces.C.Strings.chars_ptr;  -- lemon.h:226
         Failure      : Interfaces.C.Strings.chars_ptr;  -- lemon.h:227
         C_Accept     : Interfaces.C.Strings.chars_ptr;  -- lemon.h:228
         Extra_Code   : Interfaces.C.Strings.chars_ptr;  -- lemon.h:229
         Token_Dest   : Interfaces.C.Strings.chars_ptr;  -- lemon.h:230
         Var_Dest     : Interfaces.C.Strings.chars_ptr;  -- lemon.h:231
         File_Name    : Interfaces.C.Strings.chars_ptr;  -- lemon.h:232
         Token_Prefix : Interfaces.C.Strings.chars_ptr;  -- lemon.h:234
         N_Conflict       : aliased int;  -- lemon.h:235
         N_Action_Tab     : aliased int;  -- lemon.h:236
         N_Lookahead_Tab  : aliased int;  -- lemon.h:237
         Table_Size       : aliased int;  -- lemon.h:238
         Basis_Flag       : aliased int;  -- lemon.h:239
         Has_Fallback     : aliased int;  -- lemon.h:240
         No_Line_Nos_Flag : aliased int;  -- lemon.h:241
         Argv0            : Interfaces.C.Strings.chars_ptr;  -- lemon.h:242
      end record;
   pragma Convention (C_Pass_By_Copy, Lemon_Record);  -- lemon.h:198

   use Interfaces.C.Strings;

   Clean_Lemon : Constant Lemon_Record :=
     (
         Sorted     => null,
         Rule       => null,
         Start_Rule => null,
         N_State    => 0,
         Nx_State   => 0,
         N_Rule     => 0,
         N_Symbol   => 0,
         N_Terminal => 0,
         Min_Shift_Reduce => 0,
         Err_Action   => 0,
         Acc_Action   => 0,
         No_Action    => 0,
         Min_Reduce   => 0,
         Max_Action   => 0,
         Symbols      => null,
         Error_Cnt    => 0,
         Err_Sym      => null,
         Wildcard     => null,
         Name         => Null_Ptr,
         Arg          => Null_Ptr,
         Ctx          => Null_Ptr,
         Token_Type   => Null_Ptr,
         Var_Type     => Null_Ptr,
         Start        => Null_Ptr,
         Stack_Size   => Null_Ptr,
         Include      => Null_Ptr,
         Error        => Null_Ptr,
         Overflow     => Null_Ptr,
         Failure      => Null_Ptr,
         C_Accept     => Null_Ptr,
         Extra_Code   => Null_Ptr,
         Token_Dest   => Null_Ptr,
         Var_Dest     => Null_Ptr,
         File_Name    => Null_Ptr,
         Token_Prefix => Null_Ptr,
         N_Conflict       => 0,
         N_Action_Tab     => 0,
         N_Lookahead_Tab  => 0,
         Table_Size       => 0,
         Basis_Flag       => 0,
         Has_Fallback     => 0,
         No_Line_Nos_Flag => 0,
         Argv0            => Null_Ptr
);
  -- List of all rules
  -- First rule
  -- Number of states
  -- nstate with tail degenerate states removed
  -- Number of rules
  -- Number of terminal and nonterminal symbols
  -- Number of terminal symbols
  -- Minimum shift-reduce action value
  -- Error action value
  -- Accept action value
  -- No-op action value
  -- Minimum reduce action
  -- Maximum action value of any kind
  -- Sorted array of pointers to symbols
  -- Number of errors
  -- The error symbol
  -- Token that matches anything
  -- Name of the generated parser
  -- Declaration of the 3th argument to parser
  -- Declaration of 2nd argument to constructor
  -- Type of terminal symbols in the parser stack
  -- The default type of non-terminal symbols
  -- Name of the start symbol for the grammar
  -- Size of the parser stack
  -- Code to put at the start of the C file
  -- Code to execute when an error is seen
  -- Code to execute on a stack overflow
  -- Code to execute on parser failure
  -- Code to execute when the parser excepts
  -- Code appended to the generated file
  -- Code to execute to destroy token data
  -- Code for the default non-terminal destructor
  -- Name of the input file
  --  char *outname;           /* Name of the current output file
  -- A prefix added to token names in the .h file
  -- Number of parsing conflicts
  -- Number of entries in the yy_action[] table
  -- Number of entries in yy_lookahead[]
  -- Total table size of all tables in bytes
  -- Print only basis configurations
  -- True if any %fallback is seen in the grammar
  -- True if #line statements should not be printed
  -- Name of the program
  --********************************************************************
  --#define NO_OFFSET (-2147483647)
   NO_OFFSET : aliased long;  -- lemon.h:247
   pragma Import (C, NO_OFFSET, "NO_OFFSET");

  --********* From the file "report.h" ************************************
--     procedure lemon_reprint (arg1 : access Lemon_Record);  -- lemon.h:250
--     pragma Import (C, lemon_reprint, "lemon_reprint");

--     procedure lemon_report_output (arg1 : access Lemon_Record);  -- lemon.h:251
--     pragma Import (C, lemon_report_output, "lemon_report_output");

--     procedure lemon_report_table (arg1 : access Lemon_Record);  -- lemon.h:252
--     pragma Import (C, lemon_report_table, "lemon_report_table");

--     procedure lemon_report_header (arg1 : access Lemon_Record);  -- lemon.h:253
--     pragma Import (C, lemon_report_header, "lemon_report_header");

--     procedure lemon_compress_tables (arg1 : access Lemon_Record);  -- lemon.h:254
--     pragma Import (C, lemon_compress_tables, "lemon_compress_tables");

--     procedure lemon_resort_states (arg1 : access Lemon_Record);  -- lemon.h:255
--     pragma Import (C, lemon_resort_states, "lemon_resort_states");

--    --********* From the file "build.h" ***********************************
--     procedure lemon_find_rule_precedences (arg1 : access Lemon_Record);  -- lemon.h:258
--     pragma Import (C, lemon_find_rule_precedences, "lemon_find_rule_precedences");

--     procedure lemon_find_first_sets (arg1 : access Lemon_Record);  -- lemon.h:259
--     pragma Import (C, lemon_find_first_sets, "lemon_find_first_sets");

--  --   procedure lemon_find_states (arg1 : access Lemon_Record);  -- lemon.h:260
--  --   pragma Import (C, lemon_find_states, "lemon_find_states");

--     procedure lemon_find_links (arg1 : access Lemon_Record);  -- lemon.h:261
--     pragma Import (C, lemon_find_links, "lemon_find_links");

--     procedure lemon_find_follow_sets (arg1 : access Lemon_Record);  -- lemon.h:262
--     pragma Import (C, lemon_find_follow_sets, "lemon_find_follow_sets");

--     procedure lemon_find_actions (arg1 : access Lemon_Record);  -- lemon.h:263
--     pragma Import (C, lemon_find_actions, "lemon_find_actions");

--     function lime_get_user_template_name return Interfaces.C.Strings.chars_ptr;  -- lemon.h:267
--     pragma Import (C, lime_get_user_template_name, "lime_get_user_template_name");

--    --  lempar.c for C and cherry_parser.adb for Ada.
--     procedure lime_template_open
--       (arg1 : Interfaces.C.Strings.chars_ptr;
--        arg2 : access int;
--        arg3 : access int);  -- lemon.h:271
--     pragma Import (C, lime_template_open, "lime_template_open");

--    -- User provided template. "" when none.
--    -- Incremented on error
--    -- Success = 0 when no template file is
--    --  Thisk function finds the template file and opens it. File handle
--    --  is located in the context structure.
--     procedure lime_implementation_open (arg1 : Interfaces.C.Strings.chars_ptr);  -- lemon.h:280
--     pragma Import (C, lime_implementation_open, "lime_implementation_open");

--    --  Open a file for writing then implementaion (parse.adb/parse.c).
--    --  File handler is located in the context structure.
--     procedure lime_template_transfer (arg1 : Interfaces.C.Strings.chars_ptr);  -- lemon.h:286
--     pragma Import (C, lime_template_transfer, "lime_template_transfer");

--     procedure lime_print
--       (arg1 : Interfaces.C.Strings.chars_ptr;
--        arg2 : int;
--        arg3 : Interfaces.C.Strings.chars_ptr);  -- lemon.h:292
--     pragma Import (C, lime_print, "lime_print");

--     procedure lime_template_linedir (arg1 : access int; arg2 : Interfaces.C.Strings.chars_ptr);  -- lemon.h:301
--     pragma Import (C, lime_template_linedir, "lime_template_linedir");

--     procedure lime_write_include (arg1 : Interfaces.C.Strings.chars_ptr);  -- lemon.h:308
--     pragma Import (C, lime_write_include, "lime_write_include");

--    --(int          mh_flag,
--     procedure lime_generate_tokens
--       (arg1 : Interfaces.C.Strings.chars_ptr;
--        arg2 : int;
--        arg3 : int);  -- lemon.h:315
--     pragma Import (C, lime_generate_tokens, "lime_generate_tokens");

--    --  (int          mh_flag,
--     function lime_get_token_callback (arg1 : int) return Interfaces.C.Strings.chars_ptr;  -- lemon.h:323
--     pragma Import (C, lime_get_token_callback, "lime_get_token_callback");

--     procedure lime_generate_spec
--       (arg1 : Interfaces.C.Strings.chars_ptr;
--        arg2 : Interfaces.C.Strings.chars_ptr;
--        arg3 : Interfaces.C.Strings.chars_ptr;
--        arg4 : int;
--        arg5 : int);  -- lemon.h:329
--     pragma Import (C, lime_generate_spec, "lime_generate_spec");

--    --  Generate spec file (parse.h for parse.y).
--     procedure lime_generate_the_defines_1
--       (arg1 : Interfaces.C.Strings.chars_ptr;
--        arg2 : int;
--        arg3 : Interfaces.C.Strings.chars_ptr;
--        arg4 : int;
--        arg5 : int);  -- lemon.h:339
--     pragma Import (C, lime_generate_the_defines_1, "lime_generate_the_defines_1");

--     procedure lime_generate_the_defines_2 (arg1 : Interfaces.C.Strings.chars_ptr);  -- lemon.h:349
--     pragma Import (C, lime_generate_the_defines_2, "lime_generate_the_defines_2");

   type mystruct is record
      use_count : aliased int;  -- lemon.h:356
      index : aliased int;  -- lemon.h:357
      dt_num : aliased int;  -- lemon.h:358
   end record;
   pragma Convention (C_Pass_By_Copy, mystruct);  -- lemon.h:354

   procedure lime_error_fallback
     (arg1 : Interfaces.C.Strings.chars_ptr;
      arg2 : access mystruct;
      arg3 : int);  -- lemon.h:362
   pragma Import (C, lime_error_fallback, "lime_error_fallback");

   type lime_render_record is record
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

--     procedure lime_render_constants (arg1 : access lime_render_record);  -- lemon.h:383
--     pragma Import (C, lime_render_constants, "lime_render_constants");

--     procedure lime_write_action_table (arg1 : int; arg2 : int);  -- lemon.h:388
--     pragma Import (C, lime_write_action_table, "lime_write_action_table");

--     procedure lime_write_yy_lookahead (arg1 : int; arg2 : int);  -- lemon.h:395
--     pragma Import (C, lime_write_yy_lookahead, "lime_write_yy_lookahead");

--     procedure lime_write_yy_shift_offsets
--       (arg1 : int;
--        arg2 : int;
--        arg3 : int;
--        arg4 : Interfaces.C.Strings.chars_ptr;
--        arg5 : int;
--        arg6 : int);  -- lemon.h:402
--     pragma Import (C, lime_write_yy_shift_offsets, "lime_write_yy_shift_offsets");

--     procedure lime_write_yy_reduce_offsets
--       (arg1 : int;
--        arg2 : int;
--        arg3 : int;
--        arg4 : Interfaces.C.Strings.chars_ptr;
--        arg5 : int);  -- lemon.h:413
--     pragma Import (C, lime_write_yy_reduce_offsets, "lime_write_yy_reduce_offsets");

--     procedure lime_write_default_action_table
--       (arg1 : int;
--        arg2 : int;
--        arg3 : int);  -- lemon.h:423
--     pragma Import (C, lime_write_default_action_table, "lime_write_default_action_table");

--     procedure lime_put (arg1 : Interfaces.C.Strings.chars_ptr);  -- lemon.h:431
--     pragma Import (C, lime_put, "lime_put");

--     procedure lime_put_int (arg1 : int);  -- lemon.h:436
--     pragma Import (C, lime_put_int, "lime_put_int");

--     procedure lime_put_line (arg1 : Interfaces.C.Strings.chars_ptr);  -- lemon.h:441
--     pragma Import (C, lime_put_line, "lime_put_line");

--     procedure lime_write_line_directive (arg1 : int; arg2 : Interfaces.C.Strings.chars_ptr);  -- lemon.h:446
--     pragma Import (C, lime_write_line_directive, "lime_write_line_directive");

--     procedure lime_template_print
--       (arg1 : Interfaces.C.Strings.chars_ptr;
--        arg2 : int;
--        arg3 : Interfaces.C.Strings.chars_ptr);  -- lemon.h:453
--     pragma Import (C, lime_template_print, "lime_template_print");

--    -- int          line_number,
--     procedure lime_write_arg_defines
--       (arg1 : Interfaces.C.Strings.chars_ptr;
--        arg2 : Interfaces.C.Strings.chars_ptr;
--        arg3 : int;
--        arg4 : Interfaces.C.Strings.chars_ptr;
--        arg5 : Interfaces.C.Strings.chars_ptr);  -- lemon.h:462
--     pragma Import (C, lime_write_arg_defines, "lime_write_arg_defines");

--     procedure lime_close_out;  -- lemon.h:472
--     pragma Import (C, lime_close_out, "lime_close_out");

--     procedure lime_close_in;  -- lemon.h:477
--     pragma Import (C, lime_close_in, "lime_close_in");

--    --int
--    --lime_get_mh_flag (void);
--     procedure lime_write_interface (arg1 : Interfaces.C.Strings.chars_ptr; arg2 : Interfaces.C.Strings.chars_ptr);  -- lemon.h:487
--     pragma Import (C, lime_write_interface, "lime_write_interface");

--     procedure lime_write_interface_begin;  -- lemon.h:493
--     pragma Import (C, lime_write_interface_begin, "lime_write_interface_begin");

--     procedure lime_write_interface_end;  -- lemon.h:494
--     pragma Import (C, lime_write_interface_end, "lime_write_interface_end");

--     procedure lime_report_header
--       (arg1 : Interfaces.C.Strings.chars_ptr;
--        arg2 : Interfaces.C.Strings.chars_ptr;
--        arg3 : Interfaces.C.Strings.chars_ptr;
--        arg4 : int);  -- lemon.h:499
--     pragma Import (C, lime_report_header, "lime_report_header");

--   procedure lime_generate_reprint_of_grammar;  -- lemon.h:507
--   pragma Import (C, lime_generate_reprint_of_grammar, "lime_generate_reprint_of_grammar");

--   procedure lime_set_out_name (arg1 : Interfaces.C.Strings.chars_ptr);  -- lemon.h:512
--   pragma Import (C, lime_set_out_name, "lime_set_out_name");

--   function lime_get_out_name return Interfaces.C.Strings.chars_ptr;  -- lemon.h:518
--   pragma Import (C, lime_get_out_name, "lime_get_out_name");

end lemon_h;
