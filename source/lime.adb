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

with DK8543.Interfaces.C.Strings;

with Setup;
with Text_Out;
with Options;
with Backend;

package body Lime is

   function Default_Template_Name return String;


   procedure Implementation_Open (File_Name : in String) is
   begin
      Text_Out.Implementation_Open (File_Name);
   end Implementation_Open;


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
      use Backend;

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


   procedure Template_Transfer (Name : in String)
   is
      use Backend;

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
      Include     : in String)
   is
   begin
      if Include = "" then
         return;
      end if;

      --  Transfer line incrementing line numbers on ASCII.LF
      --  Text_Out.Put_Line_CP (New_String (Line));
      Text_Out.Put_Line (Include);
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
      Wildcard_Index : in Symbol_Index)
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


   procedure Close_Out is
   begin
      Text_Out.Close_Out;
   end Close_Out;


   procedure Close_In is
   begin
      Ada.Text_IO.Close (Backend.Context.File_Template);
   end Close_In;


   --  A followset propagation link indicates that the contents of one
   --  configuration followset should be propagated to another whenever

end Lime;
