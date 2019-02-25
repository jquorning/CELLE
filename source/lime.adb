--
--  The author disclaims copyright to this source code.  In place of
--  a legal notice, here is a blessing:
--
--    May you do good and not evil.
--    May you find forgiveness for yourself and forgive others.
--    May you share freely, not taking more than you give.
--
--  Cherry
--  Lime body
--  Ada binding for Lemon
--

with Ada.Strings.Unbounded;
with Ada.Directories;
with Ada.IO_Exceptions;

with Generate_Ada;
with Generate_C;
with Setup;
with Backend;
with Text_Out;
with Auxiliary;
with Database;
with Options;

package body Lime is

   use Backend;

   procedure Implementation_Open
     (File_Name : in Interfaces.C.Strings.chars_ptr)
   is
      use Text_Out;
   begin
      Text_Out.Implementation_Open (File_Name);
   end Implementation_Open;

--   function Is_Alpha (C : Character) return Boolean;
   function Default_Template_Name return String;

   function Get_Token (Index : in Integer) return String
   is
      function Callback (Index : in Integer) return chars_ptr;
      pragma Import (C, Callback, "lemon_generate_header_line_callback");
   begin
      return Value (Callback (Index));
   end Get_Token;


   procedure Generate_Spec
     (Base_Name : in chars_ptr;
      Prefix    : in chars_ptr;
      Module    : in chars_ptr;
      First     : in Integer;
      Last      : in Integer)
   is
      Ada_Base_Name : constant String := Value (Base_Name);
      Ada_Prefix    : constant String := Value (Prefix);
      Ada_Module    : constant String := Value (Module);
   begin

      case Options.Language is

         when Options.Language_Ada =>
            Generate_Ada.Generate_Spec
              (Context   => Context,
               Base_Name => Ada_Base_Name,
               Module    => Ada_Module,
               Prefix    => Ada_Prefix,
               First     => First,
               Last      => Last);

         when Options.Language_C =>
            Generate_C.Generate_Spec
              (Context   => Context,
               File_Name => Ada_Base_Name,
               Module    => Ada_Module,
               Prefix    => Ada_Prefix,
               First     => First,
               Last      => Last);

      end case;
   end Generate_Spec;


   procedure Open_If_Possible
     (File      : in out Ada.Text_IO.File_Type;
      File_Name : in     String;
      Success   :    out Boolean)
   is
      use Ada.Directories;
      use Ada.Text_IO;
   begin
      Success := True;
      if not Exists (File_Name) then
         Put_Line (Standard_Output,
                   "Could not find the parser driver template file '" & File_Name & "'.");
         Success := False;
         return;
      end if;
      begin
         Open (File, In_File, File_Name);
      exception
         when others =>
            Put_Line (Standard_Output,
                        "Could not open the parser driver template file '" & File_Name & "'.");
            Success := False;
      end;
   end Open_If_Possible;

   function Default_Template_Name return String
   is
      use Options;
   begin
      case Language is
         when Language_Ada =>  return Setup.Default_Template_Ada;
         when Language_C   =>  return Setup.Default_Template_C;
      end case;
   end Default_Template_Name;

   procedure Template_Open
     (User_Template : in     chars_ptr;
      Error_Count   : in out Integer;
      Success       :    out Integer)
   is
      use Ada.Strings.Unbounded;
      Template     : Unbounded_String := Null_Unbounded_String;
      Open_Success : Boolean;
   begin
      Success := 1;

      if User_Template /= Null_Ptr or User_Template = New_String ("") then
         Template := To_Unbounded_String ("");
      end if;

      --  Try User_Template
      if Template /= "" then
         Open_If_Possible (Context.File_Template,
                           To_String (Template), Open_Success);
         if Open_Success then
            return;
         else
            Error_Count := Error_Count + 1;
         end if;
      end if;

      --  Try default template
      Open_If_Possible (File      => Context.File_Template,
                        File_Name => Default_Template_Name,
                        Success   => Open_Success);
      if Open_Success then
         return;
      else
         Error_Count := Error_Count + 1;
      end if;

      Success := 0;
   end Template_Open;


--   function Is_Alpha (C : Character) return Boolean is
--   begin
--      return (C in 'a' .. 'z') or (C in 'A' .. 'Z');
--   end Is_Alpha;


   procedure Template_Transfer
     (Name : in chars_ptr)
   is
      Parse : constant String := "Parse";
      Start : Natural;
   begin
      loop
         declare
            use Ada.Text_IO;
            Line : constant String := Get_Line (Context.File_Template);
            Index : Natural;
         begin
            exit when
              Line'Length >= 2 and then
              Line (Line'First .. Line'First + 1) = "%%";

            Start := Line'First;
            if False then  --  Name /= Null_Ptr then  XXX
               Index := Line'First;
               if
                 Line (Index) = 'P'              and then
                 Line (Index .. Index + Parse'Length - 1) = Parse and then
                 (Index = Line'First or else not Auxiliary.Is_Alpha (Line (Index - 1)))
               then
                  if Index > Start then
                     Put (Line (Start .. Index));
                  end if;
                  Text_Out.Put_CP (Name);
                  Index := Index + Parse'Length;
                  Start := Index;
               end if;
            end if;
            Put_Line (Line (Start .. Line'Last));
         end;
      end loop;

   exception

      when Ada.IO_Exceptions.End_Error =>
         Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error,
                               "EXCEPTION END ERROR");

   end Template_Transfer;

   procedure Template_Print
     (Out_Name    : in chars_ptr;
      No_Line_Nos : in Integer;
      Include     : in chars_ptr)
   is
   begin
      if Include = Null_Ptr then return; end if;

      declare
         Line : constant String := Value (Include);
      begin
         --  Transfer line incrementing line numbers on ASCII.LF
         Text_Out.Put_Line_CP (New_String (Line));
         --  for I in Line'Range loop
         --  Put (Context.File_Implementation, Line (I));
         --  if Line (I) = ASCII.LF then
         --     Line_Number := Line_Number + 1;
         --  end if;
         --  end loop;

         --  if Line (Line'Last) /= ASCII.LF then
         --     New_Line (Context.File_Implementation);
         --     Line_Number := Line_Number + 1;
         --  end if;

         --  Optionally add source line number comments
         Ada.Text_IO.Put ("WLD - ");
         if No_Line_Nos /= 0 then
            Ada.Text_IO.Put_Line ("1");
            --  Write_Line_Directive (Line_Number, Out_Name);
            Text_Out.Put_Line_Directive (Out_Name);
         end if;
      end;
   end Template_Print;


   procedure Write_Include
     (Include_Name : in chars_ptr)
   is
      use Text_Out;
   begin
      if Options.MH_Flag then
         Put ("#include <");
         Put_CP (Include_Name);
         Put_Line (">;");
      end if;
   end Write_Include;

   procedure Generate_Tokens
     (Tokenprefix : in chars_ptr;
      First       : in Integer;
      Last        : in Integer)
   is
      use Text_Out;
      Prefix : chars_ptr;
   begin
      if Options.MH_Flag then
         --  const char *prefix; */
         Put_Line ("#if INTERFACE");
--         Line_Number := Line_Number + 1;
         if Tokenprefix /= Null_Ptr then
            Prefix := Tokenprefix;
         else
            Prefix := New_String ("");
         end if;

         for I in First .. Last loop
--              Put_Line (Context.File_Implementation,
--                        "#define " &
--                          Value (Prefix)   &
--                          Value (Get_Token_Callback (I)) &
--                          " " & Integer'Image (I));
--              Line_Number := Line_Number + 1;
            Put ("#define ");
            Put_CP (Prefix);
            Put_CP (Get_Token_Callback (I));
            Put (" ");
            Put_Int (I);
            New_Line;
         end loop;
         Put_Line ("#endif");
      end if;
   end Generate_Tokens;

   procedure Generate_The_Defines_1
     (YY_Code_Type   : in     chars_ptr;
      Symbol_Count   : in     Integer;
      YY_Action_Type : in     chars_ptr;
      Wildcard       : in     Integer;
      Wildcard_Index : in     chars_ptr)
   is
      use Text_Out;
   begin
      Put ("#define YYCODETYPE ");
      Put_CP (YY_Code_Type);
      New_Line;

      Put ("#define YYNOCODE ");
      Put_Int (Symbol_Count);
      New_Line;

      Put ("#define YYACTIONTYPE ");
      Put_CP (YY_Action_Type);
      New_Line;

      if Wildcard = 0 then
         Put ("#define YYWILDCARD ");
         Put_CP (Wildcard_Index);
         Put_Line ("");
      end if;
   end Generate_The_Defines_1;

   procedure Generate_The_Defines_2
     (Stack_Size : in chars_ptr)
   is
      use Text_Out;
   begin
      Put_Line ("#ifndef YYSTACKDEPTH");
      if Stack_Size /= Null_Ptr then
         Put ("#define YYSTACKDEPTH ");
         Put_CP (Stack_Size);
         New_Line;
      else
         Put_Line ("#define YYSTACKDEPTH 100");
      end if;
      Put_Line ("#endif");
   end Generate_The_Defines_2;


   procedure Error_Fallback
     (Error_Sym    : in     chars_ptr;
      Struct       : in     Struct_Access;
      Has_Fallback : in     Integer)
   is
      use Text_Out;
   begin

      if Error_Sym /= Null_Ptr and Struct.Use_Count /= 0 then
         Put ("#define YYERRORSYMBOL ");
         Put_Int (Struct.Index);
         New_Line;

         Put ("#define YYERRSYMDT yy");
         Put_Int (Struct.DT_Num);
         New_Line;
      end if;

      if Has_Fallback /= 0 then
         Put ("#define YYFALLBACK 1");
      end if;

   end Error_Fallback;


   procedure Render_Constants
     (Render : in Render_Access)
   is
      procedure Put (Item  : in String;
                     Value : in Integer);

      procedure Put (Item  : in String;
                     Value : in Integer)
      is
         use Text_Out;
      begin
         Put (Item);
         Put_Int (Value);
         New_Line;
      end Put;

      I : Integer;
   begin
      Put ("#define YYNSTATE             ", Render.Nxstate);
      Put ("#define YYNRULE              ", Render.nrule);
      Put ("#define YYNTOKEN             ", Render.nterminal);
      Put ("#define YY_MAX_SHIFT         ", Render.Nxstate - 1);
      I := Render.minShiftReduce;
      Put ("#define YY_MIN_SHIFTREDUCE   ", I);
      I := I + Render.nrule;
      Put ("#define YY_MAX_SHIFTREDUCE   ", I - 1);
      Put ("#define YY_ERROR_ACTION      ", Render.errAction);
      Put ("#define YY_ACCEPT_ACTION     ", Render.accAction);
      Put ("#define YY_NO_ACTION         ", Render.noAction);
      Put ("#define YY_MIN_REDUCE        ", Render.minReduce);
      I := Render.minReduce + Render.nrule;
      Put ("#define YY_MAX_REDUCE        ", I - 1);
   end Render_Constants;


   procedure Write_Action_Table
     (N           : in     Integer;
      No_Action   : in     Integer)
   is
      use Text_Out;
      use Auxiliary;
      J : Integer;
      Action : Integer;
   begin
      Put_Line ("#define YY_ACTTAB_COUNT (" & Image (N) & ")");
      Put_Line ("static const YYACTIONTYPE yy_action[] = {");
      J := 0;
      for I in 0 .. N - 1 loop
         Action := Get_Acttab_YY_Action (I);
         if Action < 0 then
            Action := No_Action;
         end if;
         if J = 0 then
            Put (" /* " & Image (I) & " */ ");
         end if;
         Put (" " & Image (Action) & ",");
         if J = 9 or I = N - 1 then
            Put_Line ("");
            J := 0;
         else
            J := J + 1;
         end if;
      end loop;
      Put_Line ("};");

   end Write_Action_Table;


   procedure Write_YY_Lookahead
     (N           : in     Integer;
      Nsymbol     : in     Integer)
   is
      use Text_Out;
      use Auxiliary;
      LA : Integer;
      J  : Integer := 0;
   begin
      Put_Line ("static const YYCODETYPE yy_lookahead[] = {");
      for I in 0 .. N - 1 loop
         LA := Get_Acttab_YY_Lookahead (I);
         if LA < 0 then
            LA := Nsymbol;
         end if;
         if J = 0 then
            Put (" /* " & Image (I) & " */ ");
         end if;
         Put (" " & Image (LA) & ",");
         if J = 9 or I = N - 1 then
            Put_Line ("");
            J := 0;
         else
            J := J + 1;
         end if;
      end loop;
      Put_Line ("};");
   end Write_YY_Lookahead;

   procedure Write_YY_Shift_Offsets
     (N             : in Integer;
      MnTknOfst     : in Integer;
      MxTknOfst     : in Integer;
      Min_Size_Type : in chars_ptr;
      Nactiontab    : in Integer;
      NO_OFFSET     : in Integer)
   is
      use Text_Out;
      use Auxiliary;
      Ofst : Integer;
      J : Integer := 0;
   begin
      Put_Line ("#define YY_SHIFT_COUNT    (" & Image (N - 1) & ")");
      Put_Line ("#define YY_SHIFT_MIN      (" & Image (MnTknOfst) & ")");
      Put_Line ("#define YY_SHIFT_MAX      (" & Image (MxTknOfst) & ")");
      Put_Line ("static const " & Value (Min_Size_Type) & " yy_shift_ofst[] = {");
--  lemp->tablesize += n*sz;
      for I in 0 .. N - 1 loop
--  stp := lemp->sorted[i];
--  ofst := stp->iTknOfst;
         Ofst := Get_Token_Offset (I);
         if Ofst = NO_OFFSET then
            Ofst := Nactiontab;
         end if;
         if J = 0 then
            Put (" /* " & Image (I) & " */ ");
         end if;
         Put (" " & Image (Ofst) & ",");
         if J = 9 or I = N - 1 then
            Put_Line ("");
            J := 0;
         else
            J := J + 1;
         end if;
      end loop;
      Put_Line ("};");
   end Write_YY_Shift_Offsets;

   procedure Write_YY_Reduce_Offsets
     (N             : in Integer;
      MnNtOfst      : in Integer;
      MxNtOfst      : in Integer;
      Min_Size_Type : in chars_ptr;
      NO_OFFSET     : in Integer)
   is
      use Text_Out;
      use Auxiliary;
      J : Integer := 0;
      Ofst : Integer;
   begin
      Put_Line ("#define YY_REDUCE_COUNT (" & Image (N - 1) & ")");
      Put_Line ("#define YY_REDUCE_MIN   (" & Image (MnNtOfst) & ")");
      Put_Line ("#define YY_REDUCE_MAX   (" & Image (MxNtOfst) & ")");
      Put_Line ("static const " & Value (Min_Size_Type) & " yy_reduce_ofst[] = {");

--  lemp->tablesize += n*sz;
      for I in 0 .. N - 1 loop
         Ofst := Get_NT_Offset (I);
         if Ofst = NO_OFFSET then
            Ofst := MnNtOfst - 1;
         end if;
         if J = 0 then
            Put (" /* " & Image (I) & " */ ");
         end if;
         Put (" " & Image (Ofst) & ",");
         if J = 9 or I = N - 1 then
            Put_Line ("");
            J := 0;
         else
            J := J + 1;
         end if;
      end loop;
         Put_Line ("};");
   end Write_YY_Reduce_Offsets;


   procedure Write_Default_Action_Table
     (N            : in Integer;
      Error_Action : in Integer;
      Min_Reduce   : in Integer)
   is
      use Text_Out;
      use Auxiliary;
      J : Integer := 0;
      IDfltReduce : Integer;
   begin
      Put_Line ("static const YYACTIONTYPE yy_default[] = {");
      for I in 0 .. N - 1 loop
         IDfltReduce := Get_Default_Reduce (I);
--         stp := lemp->sorted[i];
         if J = 0 then
            Put (" /* " & Image (I) & " */ ");
         end if;
         if IDfltReduce < 0 then
            Put (" " & Image (Error_Action) & ",");
         else
            Put (" " & Image (IDfltReduce + Min_Reduce) & ",");
         end if;
         if J = 9 or I = N - 1 then
            Put_Line ("");
            J := 0;
         else
            J := J + 1;
         end if;
      end loop;
      Put_Line ("};");
   end Write_Default_Action_Table;

--     procedure Write_Fallback_Token
--       (Is_Fallback    : in Integer;
--        Name           : in chars_ptr;
--        Fallback_Index : in Integer;
--        Fallback_Name  : in chars_ptr)
--     is
--     begin

--     end Write_Fallback_Token;


   procedure Template_Print_2
     (Line        : in chars_ptr;
      No_Line_Nos : in Integer;
--      Line_Number : in Line_Number_Index;
      Out_Name    : in chars_ptr)
   is
--      pragma Unreferenced (Out_Name);
   begin
      if Line = Null_Ptr then
         Ada.Text_IO.Put_Line ("RETURN");
         return;
      end if;
      Text_Out.Put_Line_CP (Line);

      --  XXX mystisk kode
--      if( str[-1]!='\n' ){
--        putc('\n',out);
--        (*lineno)++;
--        }
      Ada.Text_IO.Put ("WLD - ");
      if No_Line_Nos /= 1 then
         Ada.Text_IO.Put_Line ("2");
         --  (*lineno)++; tplt_linedir(out,*lineno,lemp->outname);
         --  Write_Line_Directive (Line_Number, Out_Name);
         --  Write_Line_Directive (0, Out_Name);
         Text_Out.Put_Line_Directive (Out_Name);
      end if;

   end Template_Print_2;


   procedure Write_Arg_Defines
     (Name    : in chars_ptr;
      Arg_Ctx : in chars_ptr;
      Extend  : in Integer;
      Arg     : in chars_ptr;
      Arg_I   : in chars_ptr)
   is
      Ada_Name    : constant String := Value (Name);
      Ada_Arg_Ctx : constant String := Value (Arg_Ctx);
      Ada_Arg     : constant String := Value (Arg);
      Ada_Arg_I   : constant String := Value (Arg_I);

      procedure Write (Decl : in String);

      procedure Write (Decl : in String) is
         use Text_Out;
      begin
         Put_Line ("#define " & Ada_Name & Ada_Arg_Ctx & Decl & Ada_Arg & ";");
      end Write;

      use Text_Out;
   begin
      Write ("_SDECL ");
      Write ("_PDECL ,");
      Write ("_PARAM ,");
      if Extend = 0 then
         Put_Line ("#define " & Ada_Name & "_FETCH " &
                     Ada_Arg   & "=yypParser->" & Ada_Arg_I & ";");
         Put_Line ("#define " & Ada_Name & "_STORE " &
                     Ada_Arg_I & "=yypParser->" & Ada_Arg_I & ";");
      else
         Write ("_FETCH ");
         Write ("_STORE ");
      end if;
   end Write_Arg_Defines;


   procedure Close_Out is
   begin
      Text_Out.Close_Out; --   (File_Out);
   end Close_Out;

   procedure Close_In is
   begin
      Ada.Text_IO.Close (Context.File_Template);
   end Close_In;


   procedure Write_Interface
     (Name      : in chars_ptr;
      Tokentype : in chars_ptr)
   is
      use Text_Out;
   begin
      if Options.MH_Flag then
         Put_Line ("#if INTERFACE");
      end if;

      Put ("#define ");
      Put_CP (Name);
      Put ("TOKENTYPE ");
      Put_CP (Tokentype);
      New_Line;

      if Options.MH_Flag then
         Put_Line ("#endif");
      end if;
   end Write_Interface;


   procedure Write_Interface_Begin
   is
      use Text_Out;
   begin
      if Options.MH_Flag then
         Put_Line ("#if INTERFACE");
      end if;
   end Write_Interface_Begin;


   procedure Write_Interface_End
   is
      use Text_Out;
   begin
      if Options.MH_Flag then
         Put_Line ("#endif");
      end if;
   end Write_Interface_End;


   procedure Report_Header
     (Token_Prefix  : in chars_ptr;
      Base_Name     : in chars_ptr;
      Module_Name   : in chars_ptr;
      Terminal_Last : in Natural)
   is
      use Ada.Text_IO;
      Prefix : chars_ptr := Token_Prefix;
   begin

      if not Options.MH_Flag then
         return;
      end if;

      if Token_Prefix = Null_Ptr then
         Prefix := New_String ("");
      end if;

      --  Generate parse.h.ads
      Generate_Spec (Base_Name, Prefix, Module_Name,
                     First => 1,
                     Last  => Terminal_Last);
   end Report_Header;

   Lemon_Lemp : Lemon_Record;
   pragma Import (C, Lemon_Lemp, "lem");

   procedure Generate_Reprint_Of_Grammar
     (Base_Name     : in chars_ptr;
      Token_Prefix  : in chars_ptr;
      Terminal_Last : in Natural)
   is
      use Ada.Text_IO;
   begin
      if Options.RP_Flag then
         Reprint (Lemon_Lemp);
      else
         Put_Line ("### 2-1");
         --  Initialize the size for all follow and first sets
         Set_Size (Terminal_Last + 1);
         Put_Line ("### 2-2");
         --  Find the precedence for every production rule (that has one)
         Find_Rule_Precedences (Lemon_Lemp);
         Put_Line ("### 2-3");
         --  Compute the lambda-nonterminals and the first-sets for every
         --  nonterminal
         Find_First_Sets (Lemon_Lemp);
         Put_Line ("### 2-4");
         --  Compute all LR(0) states.  Also record follow-set propagation
         --  links so that the follow-set can be computed later
         Compute_LR_States (Lemon_Lemp);
         Put_Line ("### 2-5");
         --         Lemon_Lemp->nstate = 0;
--         FindStates (Lemon_lemp);
--         Lemon_Lemp->sorted = State_arrayof();

         --  Tie up loose ends on the propagation links
         Find_Links (Lemon_Lemp);
         Put_Line ("### 2-6");
         --  Compute the follow set of every reducible configuration
         Find_Follow_Sets (Lemon_Lemp);
         Put_Line ("### 2-7");
         --  Compute the action tables
         Find_Actions (Lemon_Lemp);
         Put_Line ("### 2-8");
         --  Compress the action tables
         if not Options.Compress then
            Compress_Tables (Lemon_Lemp);
         end if;
         Put_Line ("### 2-9");
         --  Reorder and renumber the states so that states with fewer choices
         --  occur at the end.  This is an optimization that helps make the
         --  generated parser tables smaller.
         if not Options.No_Resort then
            Resort_States (Lemon_Lemp);
         end if;
         Put_Line ("### 2-10");
         --   Generate a report of the parser generated.  (the "y.output" file)
         if not Options.Be_Quiet then
            Report_Output (Lemon_Lemp);
         end if;

         --  Generate the source code for the parser
         Report_Table (Lemon_Lemp);

         --  Produce a header file for use by the scanner.  (This step is
         --  omitted if the "-m" option is used because makeheaders will
         --  generate the file for us.)
         Report_Header
           (Token_Prefix,
            Base_Name, -- File_Makename (Lemon_Lemp, ""),
            New_String ("MODULE XXX"),
            Terminal_Last);
      end if;
   end Generate_Reprint_Of_Grammar;


   procedure Make_Copy_Of_Ada_Option_Strings
   is
   begin
      Lemon_Program_Name  := New_String (Options.Program_Name.all);
      Lemon_Input_File    := New_String (Options.Input_File.all);
      Lemon_User_Template := New_String (Options.User_Template.all);
      Lemon_Output_Dir    := New_String (Options.Output_Dir.all);
   end Make_Copy_Of_Ada_Option_Strings;


   procedure Lime_Partial_Database_Dump_Ada is
      use Database;
   begin
      Dump (Lime_Lemp);
   end Lime_Partial_Database_Dump_Ada;


   function Sorted_Element_At (Extra : in Extra_Access;
                               Index : in Symbol_Index)
                              return State_Access
   is
   begin
      return null; --  XXX
   end Sorted_Element_At;

   --  A followset propagation link indicates that the contents of one
   --  configuration followset should be propagated to another whenever

end Lime;
