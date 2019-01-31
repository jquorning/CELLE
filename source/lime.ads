--
--  Cherrylime
--  Lime body
--  Ada Lemon binding
--

with Ada.Text_IO;

with Interfaces.C.Strings;

with GNAT.Strings;

package Lime is

   Option_Show_Conflict : aliased Boolean;
   Option_Show_Version  : aliased Boolean;
   Option_RP_Flag       : aliased Boolean;
   Option_Basis_Flag    : aliased Boolean;
   Option_Compress      : aliased Boolean;
   Option_Be_Quiet      : aliased Boolean;
   Option_Statistics    : aliased Boolean;
   Option_MH_Flag       : aliased Boolean;
   Option_No_Line_Nos   : aliased Boolean;
   Option_No_Resort     : aliased Boolean;
   Option_Show_Help     : aliased Boolean;

   use GNAT.Strings;
   Option_Program_Name  : aliased String_Access := new String'("");
   Option_Input_File    : aliased String_Access := new String'("parse.y");
   Option_User_Template : aliased String_Access := new String'("");
   Option_Output_Dir    : aliased String_Access := new String'(".");

   use Interfaces.C.Strings;
   Lemon_Program_Name  : aliased chars_ptr := New_String ("");
   Lemon_Input_File    : aliased chars_ptr := New_String ("");
   Lemon_User_Template : aliased chars_ptr := New_String ("");
   Lemon_Output_Dir    : aliased chars_ptr := New_String ("");

   type Language_Type is (Language_Ada, Language_C);

   Option_Language : Language_Type := Language_C;


   procedure Implementation_Open
     (File_Name : in Interfaces.C.Strings.chars_ptr);
--     renames Text_Out_IO.Open;

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

--   procedure Implementation_Open
--     (File_Name : in chars_ptr);
   --  Uses Context for File_implementation.

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
     --  (MH_Flag      : in Integer;
     (Include_Name : in chars_ptr);

   procedure Generate_Tokens
--     (MH_Flag     : in     Integer;
     (Tokenprefix : in     chars_ptr;
      First       : in     Integer;
      Last        : in     Integer);

   function Get_Token_Callback
     (Index : in Integer)
     return chars_ptr;

   procedure Generate_The_Defines_1
     (YY_Code_Type   : in     chars_ptr;
      Symbol_Count   : in     Integer;
      YY_Action_Type : in     chars_ptr;
      Wildcard       : in     Integer;
      Wildcard_Index : in     chars_ptr);

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
     (Error_Sym    : in     chars_ptr;
      Struct       : in     Struct_Access;
      Has_Fallback : in     Integer);
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
--      Line_Number : in Backend.Line_Number_Index;
      Out_Name    : in chars_ptr);
   --  Print a string to the file and keep the linenumber up to date

   procedure Write_Arg_Defines
     (Name    : in chars_ptr;
      Arg_Ctx : in chars_ptr;
      Extend  : in Integer;
      Arg     : in chars_ptr;
      Arg_I   : in chars_ptr);

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

   type Lemon_Type is private;
   type Lemon_Access is access Lemon_Type;

   procedure Generate_Reprint_Of_Grammar
     (Base_Name     : in chars_ptr;
      Token_Prefix  : in chars_ptr;
      Terminal_Last : in Natural);
   --  Generate a reprint of the grammar, if requested on the command line.

   --
   --  Other way round specs
   --
   procedure Reprint (Lemon : in Lemon_Type);
   procedure Set_Size (Size : in Natural);
   procedure Find_Rule_Precedences (Lemon : in Lemon_Type);
   procedure Find_First_Sets (Lemon : in Lemon_Type);
   procedure Compute_LR_States (Lemon : in Lemon_Type);
   procedure Find_Links (Lemon : in Lemon_Type);
   procedure Find_Follow_Sets (Lemon : in Lemon_Type);
   procedure Find_Actions (Lemon : in Lemon_Type);
   procedure Compress_Tables (Lemon : in Lemon_Type);
   procedure Resort_States (Lemon : in Lemon_Type);
   procedure Report_Output (Lemon : in Lemon_Type);
   procedure Report_Table (Lemon : in Lemon_Type);

private

   type Lemon_Record is null record;
   type Lemon_Type is access all Lemon_Record;
   for Lemon_Type'Storage_Size use 0;
   pragma Convention (C, Lemon_Type);

   --  Option integers
   pragma Export (C, Option_Show_Conflict, "lemon_show_conflict");
   pragma Export (C, Option_Show_Version,  "lemon_show_version");
--   pragma Export (C, Option_RP_Flag,       "lemon_rp_flag");
   pragma Export (C, Option_Basis_Flag,    "lemon_basis_flag");
   pragma Export (C, Option_Compress,      "lemon_compress");
   pragma Export (C, Option_Be_Quiet,      "lemon_be_quiet");
   pragma Export (C, Option_Statistics,    "lemon_statistics");
--   pragma Export (C, Option_MH_Flag,       "lemon_mh_flag");
   pragma Export (C, Option_No_Line_Nos,   "lemon_no_line_nos");
   pragma Export (C, Option_No_Resort,     "lemon_no_resort");

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
   --   pragma Export (C, Write_Fallback_Token,       "lime_write_fallback_token");

   pragma Export (C, Template_Print_2,    "lime_template_print");

   pragma Export (C, Write_Arg_Defines, "lime_write_arg_defines");
   pragma Export (C, Close_Out,         "lime_close_out");
   pragma Export (C, Close_In,          "lime_close_in");

   pragma Export (C, Write_Interface,   "lime_write_interface");
--   pragma Import (C, Get_MH_Flag, "lime_get_mh_flag");

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
end Lime;
