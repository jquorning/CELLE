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

with Ada.Directories;
with Ada.IO_Exceptions;

with DK8543.Auxiliary;
with DK8543.Interfaces.C.Strings;

with Generate_Ada;
with Generate_C;
with Setup;
with Backend;
with Text_Out;
with Options;

package body Lime is

   use Backend;

   procedure Implementation_Open (File_Name : in String)
   is
      use Text_Out;
   begin
      Text_Out.Implementation_Open (File_Name);
   end Implementation_Open;

--   function Is_Alpha (C : Character) return Boolean;
   function Default_Template_Name return String;

--     function Get_Token (Index : in Integer) return String
--     is
--        function Callback (Index : in Integer) return chars_ptr;
--        --  pragma Import (C, Callback, "lem$$$$on_generate_header_line_callback");
--     begin
--        return Value (Callback (Index));
--     end Get_Token;


   procedure Generate_Spec
     (Lemp      : in Lime.Lemon_Record;
      Base_Name : in String;
      Prefix    : in String;
      Module    : in String;
      First     : in Integer;
      Last      : in Integer)
   is
--      Ada_Base_Name : constant String := Value (Base_Name);
--      Ada_Prefix    : constant String := Value (Prefix);
--      Ada_Module    : constant String := Value (Module);
   begin

      case Options.Language is

         when Options.Language_Ada =>
            Generate_Ada.Generate_Spec
              (Context   => Context,
               Base_Name => Base_Name,
               Module    => Module,
               Prefix    => Prefix,
               First     => First,
               Last      => Last);

         when Options.Language_C =>
            Generate_C.Generate_Spec
              (Lemp      => Lemp,
               Context   => Context,
               File_Name => Base_Name,
               Module    => Module,
               Prefix    => Prefix,
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
     (User_Template : in     String;
      Error_Count   : in out Integer;
      Success       :    out Integer)
   is
      Template     : String renames User_Template;
      Open_Success : Boolean;
   begin
      Success := 1;

      --  Try User_Template
      if Template /= "" then
         Open_If_Possible (Context.File_Template,
                           Template, Open_Success);
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
     (Name : in String)
   is
      Parse : constant String := "Parse";
      Start : Natural;
   begin
      loop
         declare
            use Ada.Text_IO;
            use DK8543.Interfaces.C.Strings;
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
                 (Index = Line'First or else not Is_Alpha (Line (Index - 1)))
               then
                  if Index > Start then
                     Put (Line (Start .. Index));
                  end if;
                  Text_Out.Put (Name);
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
     (Out_Name    : in String;
      No_Line_Nos : in Integer;
      Include     : in chars_ptr)
   is
   begin
      if Include = Null_Ptr then return; end if;

      declare
         Line : constant String := Value (Include);
      begin
         --  Transfer line incrementing line numbers on ASCII.LF
         --  Text_Out.Put_Line_CP (New_String (Line));
         Text_Out.Put_Line (Line);
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
     (Include_Name : in String)
   is
      use Text_Out;
   begin
      if Options.MH_Flag then
         Put ("#include <");
         Put (Include_Name);
         Put_Line (">;");
      end if;
   end Write_Include;


   procedure Generate_The_Defines_1
     (YY_Code_Type   : in String;
      Symbol_Count   : in Integer;
      YY_Action_Type : in String;
      Is_Wildcard    : in Boolean;
      Wildcard_Index : in Symbol_Index) --  Chars_Ptr)
   is
      use Text_Out;
   begin
      Put ("#define YYCODETYPE ");
      Put (YY_Code_Type);
      New_Line;

      Put ("#define YYNOCODE ");
      Put_Int (Symbol_Count);
      New_Line;

      Put ("#define YYACTIONTYPE ");
      Put (YY_Action_Type);
      New_Line;

      if Is_Wildcard then
         Put ("#define YYWILDCARD ");
         Put_Int (Integer (Wildcard_Index));
         Put_Line ("");
      end if;
   end Generate_The_Defines_1;


   procedure Generate_The_Defines_2
     (Stack_Size : in String)
   is
      use Text_Out;
   begin
      Put_Line ("#ifndef YYSTACKDEPTH");
      if Stack_Size /= "" then
         Put ("#define YYSTACKDEPTH ");
         Put (Stack_Size);
         New_Line;
      else
         Put_Line ("#define YYSTACKDEPTH 100");
      end if;
      Put_Line ("#endif");
   end Generate_The_Defines_2;


   procedure Error_Fallback
     (Error_Sym    : in String;
      Struct       : in Struct_Access;
      Has_Fallback : in Integer)
   is
      use Text_Out;
   begin

      if Error_Sym /= "" and Struct.Use_Count /= 0 then
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
     (Render : in Render_Record)
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
      Put ("#define YYNSTATE             ", Render.Nx_State);
      Put ("#define YYNRULE              ", Render.N_Rule);
      Put ("#define YYNTOKEN             ", Render.N_Terminal);
      Put ("#define YY_MAX_SHIFT         ", Render.Nx_State - 1);
      I := Render.Min_Shift_Reduce;
      Put ("#define YY_MIN_SHIFTREDUCE   ", I);
      I := I + Render.N_Rule;
      Put ("#define YY_MAX_SHIFTREDUCE   ", I - 1);
      Put ("#define YY_ERROR_ACTION      ", Render.Err_Action);
      Put ("#define YY_ACCEPT_ACTION     ", Render.Acc_Action);
      Put ("#define YY_NO_ACTION         ", Render.No_Action);
      Put ("#define YY_MIN_REDUCE        ", Render.Min_Reduce);
      I := Render.Min_Reduce + Render.N_Rule;
      Put ("#define YY_MAX_REDUCE        ", I - 1);
   end Render_Constants;

   --  lemon.c:4377
   procedure Output_Action_Table
     (Action_Table : in Actions.A_Action_Table;
      N            : in Integer;
      No_Action    : in Integer)
   is
      use Text_Out;
      use DK8543.Auxiliary;
      J : Integer;
      Action : Integer;
   begin
      Put_Line ("#define YY_ACTTAB_COUNT (" & Image (N) & ")");
      Put_Line ("static const YYACTIONTYPE yy_action[] = {");
      J := 0;
      for I in 0 .. N - 1 loop
         --  #define acttab_yyaction(X,N)  ((X)->aAction[N].action)
         --  return acttab_yyaction (lime_pActtab, i);
         --  struct acttab *lime_pActtab;
         --  Action := Get_Acttab_YY_Action (I);
         Action := Action_Table.Action (I).Action;
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

   end Output_Action_Table;


   procedure Output_YY_Lookahead
     (Action_Table : in Actions.A_Action_Table;
      N            : in Integer;
      Nsymbol      : in Integer)
   is
      use Text_Out;
      use DK8543.Auxiliary;
      LA : Integer;
      J  : Integer := 0;
   begin
      Put_Line ("static const YYCODETYPE yy_lookahead[] = {");
      for I in 0 .. N - 1 loop
         --  LA := Get_Acttab_YY_Lookahead (I);
         LA := Action_Table.Lookahead (I).Action;
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
   end Output_YY_Lookahead;


   --  lemon.c:4414
   procedure Output_YY_Shift_Offsets
     (Lemp          : in Lime.Lemon_Record;
      N             : in Integer;
      MnTknOfst     : in Integer;
      MxTknOfst     : in Integer;
      Min_Size_Type : in String;
      Nactiontab    : in Integer;
      NO_OFFSET     : in Integer)
   is
      use Text_Out;
      use DK8543.Auxiliary;
      Ofst : Integer;
      J    : Integer := 0;
   begin
      Put_Line ("#define YY_SHIFT_COUNT    (" & Image (N - 1) & ")");
      Put_Line ("#define YY_SHIFT_MIN      (" & Image (MnTknOfst) & ")");
      Put_Line ("#define YY_SHIFT_MAX      (" & Image (MxTknOfst) & ")");
      Put ("static const ");
      Put (Min_Size_Type);
      Put (" yy_shift_ofst[] = {");
      New_Line;
--  lemp->tablesize += n*sz;
      for I in 0 .. N - 1 loop
         declare
            STP : access States.State_Record;  --  States.State_Access;
         begin
            --  stp := lemp->sorted[i];
            STP := Sorted_At (Lemp.Extra,
                              Symbol_Index (I));
            --  ofst := stp->iTknOfst;
            --  Ofst := Get_Token_Offset (I);
            Ofst := STP.Token_Offset;
         end;
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
   end Output_YY_Shift_Offsets;


   --  lemon.c:4440
   procedure Output_YY_Reduce_Offsets
     (Lemp          : in Lime.Lemon_Record;
      N             : in Integer;
      MnNtOfst      : in Integer;
      MxNtOfst      : in Integer;
      Min_Size_Type : in String;
      NO_OFFSET     : in Integer)
   is
      use Text_Out;
      use DK8543.Auxiliary;
      J : Integer := 0;
      Ofst : Integer;
   begin
      Put_Line ("#define YY_REDUCE_COUNT (" & Image (N - 1) & ")");
      Put_Line ("#define YY_REDUCE_MIN   (" & Image (MnNtOfst) & ")");
      Put_Line ("#define YY_REDUCE_MAX   (" & Image (MxNtOfst) & ")");
      Put_Line ("static const " & Min_Size_Type & " yy_reduce_ofst[] = {");

--  lemp->tablesize += n*sz;
      for I in 0 .. N - 1 loop
         declare
            STP : access States.State_Record;
         begin
            STP := Sorted_At (Lemp.Extra, Symbol_Index (I));
            Ofst := STP.iNtOfst;
         end;
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
   end Output_YY_Reduce_Offsets;


   --  lemon.c:4465
   procedure Output_Default_Action_Table
     (Lemp         : in Lime.Lemon_Record;
      N            : in Integer;
      Error_Action : in Integer;
      Min_Reduce   : in Integer)
   is
      use Text_Out;
      use DK8543.Auxiliary;
      J : Integer := 0;
--      IDfltReduce : Integer;
   begin
      Put_Line ("static const YYACTIONTYPE yy_default[] = {");
      for I in 0 .. N - 1 loop
         declare
            STP : constant access States.State_Record := Sorted_At (Lemp.Extra, Symbol_Index (I));
         begin
--         IDfltReduce := Get_Default_Reduce (I);
--         stp := lemp->sorted[i];
            if J = 0 then
               Put (" /* " & Image (I) & " */ ");
            end if;
            if STP.iDfltReduce then
               Put (" " & Image (Error_Action) & ",");
            else
               Put (" " & Image (Boolean'Pos (STP.iDfltReduce) + Min_Reduce) & ",");
            end if;
         end;
         if J = 9 or I = N - 1 then
            Put_Line ("");
            J := 0;
         else
            J := J + 1;
         end if;
      end loop;
      Put_Line ("};");
   end Output_Default_Action_Table;


   procedure Template_Print_2
     (Line        : in String;
      No_Line_Nos : in Integer;
--      Line_Number : in Line_Number_Index;
      Out_Name    : in String)
   is
--      pragma Unreferenced (Out_Name);
   begin
      if Line = "" then
         Ada.Text_IO.Put_Line ("RETURN");
         return;
      end if;
      Text_Out.Put_Line (Line);

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
     (Name    : in String;
      Arg_Ctx : in String;
      Extend  : in Boolean;
      Arg     : in String;
      Arg_I   : in String)
   is
--      Ada_Name    : constant String := Name;
--      Ada_Arg_Ctx : constant String := Arg_Ctx;
--      Ada_Arg     : constant String := Arg;
--      Ada_Arg_I   : constant String := Arg_I;

      procedure Write (Decl : in String);

      procedure Write (Decl : in String) is
         use Text_Out;
      begin
         Put_Line ("#define " & Name & Arg_Ctx & Decl & Arg & ";");
      end Write;

      use Text_Out;
   begin
      Write ("_SDECL ");
      Write ("_PDECL ,");
      Write ("_PARAM ,");
      if Extend = False then
         Put_Line ("#define " & Name & "_FETCH " &
                     Arg   & "=yypParser->" & Arg_I & ";");
         Put_Line ("#define " & Name & "_STORE " &
                     Arg_I & "=yypParser->" & Arg_I & ";");
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
     (Name      : in String;
      Tokentype : in String)
   is
      use Text_Out;
   begin
      if Options.MH_Flag then
         Put_Line ("#if INTERFACE");
      end if;

      Put ("#define ");
      Put (Name);
      Put ("TOKENTYPE ");
      Put (Tokentype);
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
     (Lemp          : in Lime.Lemon_Record;
      Token_Prefix  : in String;
      Base_Name     : in String;
      Module_Name   : in String;
      Terminal_Last : in Natural)
   is
      use Ada.Text_IO;
      Prefix : constant String := Token_Prefix;
   begin

      if not Options.MH_Flag then
         return;
      end if;

--      if Token_Prefix = Null_Ptr then
--         Prefix := New_String ("");
--      end if;

      --  Generate parse.h.ads
      Generate_Spec (Lemp,
                     Base_Name, Prefix, Module_Name,
                     First => 1,
                     Last  => Terminal_Last);
   end Report_Header;

--   Lemon_Lemp : Lemon_Record;
--   pragma Import (C, Lemon_Lemp, "lem");



   procedure Make_Copy_Of_Ada_Option_Strings
   is
   begin
      Lemon_Program_Name  := New_String (Options.Program_Name.all);
      Lemon_Input_File    := New_String (Options.Input_File.all);
      Lemon_User_Template := New_String (Options.User_Template.all);
      Lemon_Output_Dir    := New_String (Options.Output_Dir.all);
   end Make_Copy_Of_Ada_Option_Strings;


--     procedure Lime_Partial_Database_Dump_Ada is
--        use Database;
--     begin
--        Dump (Lemon);
--     end Lime_Partial_Database_Dump_Ada;


   function Sorted_At (Extra : in Extra_Access;
                       Index : in Symbol_Index)
                      return States.State_Access
   is
   begin
      return null; --  XXX
   end Sorted_At;

   --  A followset propagation link indicates that the contents of one
   --  configuration followset should be propagated to another whenever

end Lime;
