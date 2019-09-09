--
--  The author disclaims copyright to this source code.  In place of
--  a legal notice, here is a blessing:
--
--    May you do good and not evil.
--    May you find forgiveness for yourself and forgive others.
--    May you share freely, not taking more than you give.
--

with Ada.Text_IO;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;
with Ada.Directories;

with Setup;
with Auxiliary;
with Symbols;
with Extras;

package body Generate_C is

   procedure Open_Template
     (Context       : in out Context_Type;
      User_Template : in     String;
      File_Name     : in     String;
      Error_Count   : in out Integer)
   is
      use Ada.Text_IO;
      use Ada.Strings.Unbounded;

      Default_Template : String renames Setup.Default_Template_C;
      Template_Name    : Unbounded_String;
   begin
      --  First, see if user specified a template filename on the command line.
      if User_Template /= "" then

         --  Eksisterer den angivede template fil
         if not Ada.Directories.Exists (User_Template) then
            Ada.Text_IO.Put_Line
              (Ada.Text_IO.Standard_Error,
               "Can not find the parser driver template file '" & User_Template & "'.");
            Error_Count   := Error_Count + 1;
            Template_Name := Null_Unbounded_String;
            return;
         end if;

         --  Can User_Template open.
         begin
            Open (Context.File_Template, In_File, User_Template);

         exception
            when others =>
               --  No it could not open User_Template.
               Put_Line
                 (Standard_Error,
                  "Can not open the template file '" & User_Template & "'.");
               Error_Count := Error_Count + 1;
               Template_Name := Null_Unbounded_String;
               return;
         end;
         return;         --  User template open with success.
      end if;

      --  No user template.
      declare
         use Ada.Strings.Fixed;
         Point : constant Natural := Index (File_Name, ".");
         Buf   : Unbounded_String;
      begin
         if Point = 0 then
            Buf := To_Unbounded_String (File_Name & ".lt");
         else
            Buf := To_Unbounded_String (File_Name & ".lt");  --  XXX
         end if;

         if Ada.Directories.Exists (To_String (Buf)) then
            Template_Name := Buf;
         elsif Ada.Directories.Exists (Default_Template) then
            Template_Name := To_Unbounded_String (Default_Template);
         --  else
         --  null;  --  Template_Name := Pathsearch (Lemp.Argv0, Templatename, 0);
         end if;
      end;

      if Template_Name = Null_Unbounded_String then
         Put_Line
           (Standard_Error,
            "Can not find then parser driver template file '" & To_String (Template_Name) & "'.");
         Error_Count := Error_Count + 1;
         return;
      end if;

      begin
         Open (Context.File_Template,
               In_File, To_String (Template_Name));

      exception
         when others =>
            Put_Line
              (Standard_Error,
               "Can not open then template file '" & To_String (Template_Name) & ".");
            Error_Count := Error_Count + 1;
            return;
      end;

   end Open_Template;

   Header_Extension : constant String := ".h";

   procedure Generate_Spec
     (Lemp      : in     Lime.Lemon_Record;
      Context   : in out Context_Type;
      File_Name : in     String;
      Module    : in     String;
      Prefix    : in     String;
      First     : in     Integer;
      Last      : in     Integer)
   is
      pragma Unreferenced (Context, Module);

      package Integer_IO is
         new Ada.Text_IO.Integer_IO (Num => Integer);

      use Ada.Text_IO, Auxiliary;
      use Symbols, Extras;
      File : File_Type;
   begin

      Recreate
        (File, Out_File,
         File_Name & Header_Extension);

      for I in First .. Last - 1 loop
         declare
            Symbol : constant String :=
              Prefix & From_Key (Element_At (Lemp.Extra,
                                             Symbol_Index (I)).Name);
         begin
            Put (File, "#define ");
            Put (File, Symbol);
            Set_Col (File, 40);
            Integer_IO.Put (File, I, Width => 6);
            New_Line (File);
         end;
      end loop;

      Close (File);
   end Generate_Spec;


end Generate_C;
