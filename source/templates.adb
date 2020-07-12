--
--  The author disclaims copyright to this source code.  In place of
--  a legal notice, here is a blessing:
--
--    May you do good and not evil.
--    May you find forgiveness for yourself and forgive others.
--    May you share freely, not taking more than you give.
--

with Ada.Directories;
with Ada.IO_Exceptions;

with Setup;
with Backend;
with Auxiliary;
with Options;

package body Templates is

   procedure Open_If_Possible
     (File      : in out File_Type;
      File_Name :        String;
      Success   :    out Boolean);
   --  Open File_Name to File. Success is True when success.

   function Default_Template_Name return String;
   --

   -----------------------
   -- Template_Transfer --
   -----------------------

   procedure Template_Transfer (File : File_Type;
                                Name : String)
   is
      use Backend;

      Parse : constant String := "Parse";
      Start : Natural;
   begin
      loop
         declare
            use Ada.Text_IO;

            Line  : constant String := Get_Line (Context.File_Template);
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
                  Put (File, Name);
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

   -------------------
   -- Template_Open --
   -------------------

   procedure Template_Open
     (User_Template :        String;
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

   ------------------------
   -- Put_Line_Directive --
   ------------------------

   procedure Put_Line_Directive (File      : File_Type;
                                 Line      : Line_Number;
                                 File_Name : String)
   is
      use Ada.Text_IO;
   begin
      Put (File, "#line ");
      Put (File, Line_Number'Image (Line));
      Put (File, " """);
      Put (File, File_Name);
      Put (File, """");
      New_Line (File);
   end Put_Line_Directive;

   --------------------
   -- Template_Print --
   --------------------

   procedure Template_Print
     (File            :        File_Type;
      Line            : in out Line_Number;
      Out_Name        :        String;
      No_Line_Numbers :        Boolean;
      Include         :        String)
   is
      use Ada.Text_IO;
      use type Line_Number;
   begin
      if Include = "" then
         return;
      end if;

      --  Transfer line incrementing line numbers on ASCII.LF
      --  Text_Out.Put_Line_CP (New_String (Line));
      Put_Line (File, Include); Line := Line + 1;
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
      if No_Line_Numbers then
         Ada.Text_IO.Put_Line ("1");
         --  Write_Line_Directive (Line_Number, Out_Name);
         Put_Line_Directive (File, Line, Out_Name);
      end if;
   end Template_Print;

   ----------------------
   -- Open_If_Possible --
   ----------------------

   procedure Open_If_Possible
     (File      : in out File_Type;
      File_Name :        String;
      Success   :    out Boolean)
   is
      use Ada.Directories;
      use Ada.Text_IO;

      Stdout : File_Type renames Standard_Output;
   begin
      Success := True;
      if not Exists (File_Name) then
         Put (Stdout, "Could not find the parser driver template file '");
         Put (Stdout, File_Name);
         Put (Stdout, "'.");
         New_Line (Stdout);
         Success := False;
         return;
      end if;
      begin
         Open (File, In_File, File_Name);
      exception
         when others =>
            Put (Stdout, "Could not open the parser driver template file '");
            Put (Stdout, File_Name);
            Put (Stdout, "'.");
            New_Line (Stdout);
            Success := False;
      end;
   end Open_If_Possible;

   ---------------------------
   -- Default_Template_Name --
   ---------------------------

   function Default_Template_Name return String
   is
      use Options;
   begin
      case Language is
         when Language_Ada =>  return Setup.Default_Template_Ada;
         when Language_C   =>  return Setup.Default_Template_C;
      end case;
   end Default_Template_Name;

end Templates;
