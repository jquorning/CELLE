--
--  Cherry
--  Lime body
--  Ada binding for Lemon
--

with Ada.Strings.Unbounded;
with Ada.Strings.Fixed;
with Ada.Directories;
with Ada.IO_Exceptions;

with Generate_Ada;
with Generate_C;
with Setup;
with Auxiliary;

package body Lime is

   use Backend;

   procedure Put (Item : in String);
   procedure Put_Line (Item : in String);
   procedure New_Line;
   function Is_Alpha (C : Character) return Boolean;
   function Default_Template_Name return String;


--   function Get_MH_Flag
--     return Boolean is
--   begin
--      Ada.Text_IO.Put ("Option_MH_Flag => ");
--      Ada.Text_IO.Put_Line (Boolean'Image (Option_MH_Flag));
--      return Option_MH_Flag;
--   end Get_MH_Flag;


   procedure Put (Item : in String) is
   begin
      Ada.Text_IO.Put (Context.File_Implementation, Item);
   end Put;

   procedure Put_Line (Item : in String) is
   begin
      Ada.Text_IO.Put_Line (Context.File_Implementation, Item);
      Context.Line_Number := Context.Line_Number + 1;
   end Put_Line;

   procedure New_Line is
   begin
      Put_Line ("");
   end New_Line;


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

      case Option_Language is

         when Language_Ada =>
            Generate_Ada.Generate_Spec
              (Context   => Context,
               Base_Name => Ada_Base_Name,
               Module    => Ada_Module,
               Prefix    => Ada_Prefix,
               First     => First,
               Last      => Last);

         when Language_C =>
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

   function Default_Template_Name return String is
   begin
      case Option_Language is
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


--   function Make_Filename
--     (File_Name : in chars_ptr;
--      Suffix    : in String)
--     return String;
--   --  Create file name from File_Name and Suffix;

--   function Make_Filename
--     (File_Name : in chars_ptr;
--      Suffix    : in String)
--     return String is
--   begin
--      return Value (File_Name) & Suffix;
--   end Make_Filename;


   procedure Implementation_Open
     (File_Name : in chars_ptr)
   is
      use Ada.Text_IO;
   begin
      Auxiliary.Recreate (Context.File_Implementation, Out_File, Value (File_Name));
   end Implementation_Open;

   function Is_Alpha (C : Character) return Boolean is
   begin
      return (C in 'a' .. 'z') or (C in 'A' .. 'Z');
   end Is_Alpha;


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
                 (Index = Line'First or else not Is_Alpha (Line (Index - 1)))
               then
                  if Index > Start then
                     Put (Line (Start .. Index));
                  end if;
                  Put (Name);
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
--      use Ada.Text_IO;
   begin
      if Include = Null_Ptr then return; end if;

      declare
         Line : constant String := Value (Include);
      begin
         --  Transfer line incrementing line numbers on ASCII.LF
         Put_Line (New_String (Line));
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
            Write_Line_Directive (Context.Line_Number, Out_Name);
         end if;
      end;
   end Template_Print;


   procedure Write_Include
     --  (MH_Flag      : in Integer;
     (Include_Name : in chars_ptr)
   is
   begin
      if Option_MH_Flag then
         Put ("#include <");
         Put (Include_Name);
         Put_Line (">;");
      end if;
   end Write_Include;

   procedure Generate_Tokens
--     (MH_Flag     : in Integer;
     (Tokenprefix : in chars_ptr;
      First       : in Integer;
      Last        : in Integer)
   is
      Prefix : chars_ptr;
   begin
      if Option_MH_Flag then
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
            Put (Prefix);
            Put (Get_Token_Callback (I));
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

      if Wildcard = 0 then
         Put ("#define YYWILDCARD ");
         Put (Wildcard_Index);
         Put_Line ("");
      end if;
   end Generate_The_Defines_1;

   procedure Generate_The_Defines_2
     (Stack_Size : in chars_ptr)
   is
   begin
      Put_Line ("#ifndef YYSTACKDEPTH");
      if Stack_Size /= Null_Ptr then
         Put ("#define YYSTACKDEPTH ");
         Put (Stack_Size);
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
--      procedure Put (Item : in String);

--      procedure Put (Item : in String) is
--      begin
--         Ada.Text_IO.Put_Line (Context.File_Implementation, Item);
--         Line_Number := Line_Number + 1;
--      end Put;
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

   function Image (Value : in Integer) return String;

   function Image (Value : in Integer) return String is
      use Ada.Strings;
   begin
      return Fixed.Trim (Integer'Image (Value), Left);
   end Image;



   procedure Render_Constants
     (Render : in Render_Access)
   is
      procedure Put (Item  : in String;
                     Value : in Integer);

      procedure Put (Item  : in String;
                     Value : in Integer) is
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

   procedure Put (Item : in chars_ptr) is
   begin
      Put (Value (Item));
   end Put;

   procedure Put_Int (Item : in Integer) is
   begin
      Put (Image (Item));
   end Put_Int;

   procedure Put_Line (Item : in chars_ptr) is
   begin
      Put_Line (Value (Item));
   end Put_Line;

--     procedure Write_Fallback_Token
--       (Is_Fallback    : in Integer;
--        Name           : in chars_ptr;
--        Fallback_Index : in Integer;
--        Fallback_Name  : in chars_ptr)
--     is
--     begin

--     end Write_Fallback_Token;

   procedure Write_Line_Directive
     (Line_Number : in Line_Number_Index;
      File_Name   : in chars_ptr)
   is
      pragma Unreferenced (Line_Number);
   begin
      Ada.Text_IO.Put_Line ("Write_line_directive 616 ca");
      Put ("#line ");
      --  Put_Int (Line_Number_Index (Line_Number));
      Put_Int (Integer (Context.Line_Number));
      Put (" """);
--  while( *filename ){
--    if( *filename == '\\' ) putc('\\',out);
--    putc(*filename,out);
--    filename++;
--  }
      Put (File_Name);
      Put_Line ("""");
   end Write_Line_Directive;


   procedure Template_Print_2
     (Line        : in chars_ptr;
      No_Line_Nos : in Integer;
--      Line_Number : in Line_Number_Index;
      Out_Name    : in chars_ptr)
   is
   begin
      if Line = Null_Ptr then
         Ada.Text_IO.Put_Line ("RETURN");
         return;
      end if;
      Put_Line (Line);

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
         Write_Line_Directive (0, Out_Name);
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
      begin
         Put_Line ("#define " & Ada_Name & Ada_Arg_Ctx & Decl & Ada_Arg & ";");
      end Write;

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
      Ada.Text_IO.Close (Context.File_Implementation);
   end Close_Out;

   procedure Close_In is
   begin
      Ada.Text_IO.Close (Context.File_Template);
   end Close_In;


   procedure Write_Interface
     (Name      : in chars_ptr;
      Tokentype : in chars_ptr)
   is
   begin
      if Option_MH_Flag then
         Put_Line ("#if INTERFACE");
      end if;

      Put ("#define ");
      Put (Name);
      Put ("TOKENTYPE ");
      Put (Tokentype);
      New_Line;

      if Option_MH_Flag then
         Put_Line ("#endif");
      end if;
   end Write_Interface;


   procedure Write_Interface_Begin is
   begin
      if Option_MH_Flag then
         Put_Line ("#if INTERFACE");
      end if;
   end Write_Interface_Begin;


   procedure Write_Interface_End is
   begin
      if Option_MH_Flag then
         Put_Line ("#endif");
      end if;
   end Write_Interface_End;


end Lime;
