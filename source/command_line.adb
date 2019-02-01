--
--  The author disclaims copyright to this source code.  In place of
--  a legal notice, here is a blessing:
--
--    May you do good and not evil.
--    May you find forgiveness for yourself and forgive others.
--    May you share freely, not taking more than you give.
--

with Ada.Text_IO;
with Ada.Characters.Handling;
with Ada.Command_Line;

with Interfaces.C.Strings;

with GNAT.Command_Line;
with GNAT.Strings;

with Lime;
with Setup;

package body Command_Line is

   use Interfaces.C.Strings;
   use Lime;

   procedure Getopt_Callback
     (Switch  : String;  Param : String;  Section : String);
   --  To be installed and called by Getopt on command line switch encounter.

   procedure Define_Macro (Z : in chars_ptr);
   pragma Import (C, Define_Macro,   "handle_D_option");
   --  Handlers to call on switch encounter
   --  Handlers are locates in lemon.c


   Long_Switch_Output_Dir    : constant String := "--output-dir";
   Long_Switch_Show_Help     : constant String := "--help";
   Long_Switch_Template_File : constant String := "--template";
   Long_Switch_Language      : constant String := "--language";
   Long_Switch_Show_Version  : constant String := "--version";

   Switch_Define_Macro_Long  : constant String := "--define-macro";
   Switch_Define_Macro_Short : constant String := "-D";

   Option_Placeholder_Dummy : aliased GNAT.Strings.String_Access;

   Option_Language_String : aliased GNAT.Strings.String_Access :=
     new String'("C");
   --  C is the default language like in Lemon.

   use GNAT.Command_Line;
   Config     : Command_Line_Configuration;
   File_Count : Natural := Natural'First;

   procedure Getopt_Callback (Switch, Param, Section : String) is
      pragma Unreferenced (Section);

   begin
      if
        Switch = Switch_Define_Macro_Short or
        Switch = Switch_Define_Macro_Long
      then
         Define_Macro (New_String (Param));
      else
         --  Then it must be the file to process (.y).
         --  Found switch not accounted for. Assume input file name.
         Option_Input_File := new String'(Switch);
         File_Count        := Natural'Succ (File_Count);
      end if;
   end Getopt_Callback;


   procedure Process_Command_Line (Result : out Process_Result) is
   begin
      Option_Program_Name := new String'(Ada.Command_Line.Command_Name);

      Define_Switch (Config, Option_Basis_Flag'Access, "-b",
                     Help => "Print only the basis in report.");
      Define_Switch (Config, Option_Compress'Access, "-c",
                     Help => "Do not compress the action table.");
      Define_Switch (Config, Option_Output_Dir'Access, "-d=",
                     Help        => "Output directory. Default '.'",
                     Long_Switch => Long_Switch_Output_Dir & '=',
                     Argument => "DIR");
      Define_Switch (Config, "-D=",  --  Handled by GetOpt_Callback
                     Help        => "Define an %ifdef macro.",
                     Long_Switch => Switch_Define_Macro_Long & '=',
                     Argument => "MACRO");
      Define_Switch (Config, Option_Placeholder_Dummy'Access, "-f=",
                     Help     => "Placeholder for -f compiler options. Ignored.",
                     Argument => "IGNORE");
      Define_Switch (Config, Option_RP_Flag'Access, "-g",
                     Help => "Print grammar without actions.");
      Define_Switch (Config, Option_Show_Help'Access, "-h",
                     Help => "Show this help text.",
                     Long_Switch => Long_Switch_Show_Help);
      Define_Switch (Config, Option_Placeholder_Dummy'Access, "-I=",
                     Help     => "Placeholder for '-I' compiler options. Ignored.",
                     Argument => "IGNORE");
      Define_Switch (Config, Option_Language_String'Access, "-L=",
                     Help        => "Language (Ada, C). Default is 'C'.",
                     Long_Switch => Long_Switch_Language & '=',
                     Argument => "LANG");
      Define_Switch (Config, Option_MH_Flag'Access, "-m",
                     Help => "Output a makeheaders compatible file.");
      Define_Switch (Config, Option_No_Line_Nos'Access, "-l",
                     Help => "Do not print #line statements.");
      Define_Switch (Config, Option_Placeholder_Dummy'Access, "-O=",
                     Help => "Placeholder for '-O' compiler options. Ignored.",
                     Argument => "IGNORE");
      Define_Switch (Config, Option_Show_Conflict'Access, "-p",
                     Help => "Show conflicts resolved by precedence rules.");
      Define_Switch (Config, Option_Be_Quiet'Access, "-q",
                     Help => "Quiet. Do not print the report file.");
      Define_Switch (Config, Option_No_Resort'Access, "-r",
                     Help => "Do not sort or renumber states");
      Define_Switch (Config, Option_Statistics'Access, "-s",
                     Help => "Print parser stats to standard output.");
      Define_Switch (Config, Option_Show_Version'Access, "-x",
                     Help => "Show program name and version.");
      Define_Switch (Config, Option_User_Template'Access, "-T=",
                     Help => "Specify a template file.",
                     Long_Switch => Long_Switch_Template_File & '=',
                     Argument    => "FILE");
      Define_Switch (Config, Option_Show_Version'Access, "-v",
                     Help        => "Show program name and version.",
                     Long_Switch => Long_Switch_Show_Version);
      Define_Switch (Config, Option_Placeholder_Dummy'Access, "-W=",
                     Help     => "Placeholder for '-W' compiler options. Ignored.",
                     Argument => "IGNORE");
      Define_Switch (Config, "*", Help => "File to Parse (Typically parse.y).");

      --  Do the whole parsing business
      Getopt (Config, Getopt_Callback'Access);

      --  Set language
      declare
         use Ada.Text_IO;
         use Ada.Characters.Handling;
         LANG : constant String := To_Upper (Option_Language_String.all);
      begin
         if LANG = "ADA" then
            Option_Language := Language_Ada;
         elsif LANG = "C" then
            Option_Language := Language_C;
         else
            Put_Line (Standard_Error,
                      "Error: Unknown language");
         end if;
      end;

      if
        File_Count = 1       and
        not Option_Show_Help and
        not Option_Show_Version
      then
         Result := Success;

      elsif Option_Show_Help then
         Result := Bailout;

      elsif Option_Show_Version then
         declare
            use Ada.Text_IO, Setup;
            Version : constant String :=
              Program_Name & " (" &
              Program_Version & ")";
         begin
            Put_Line (Version);
         end;
         Result := Bailout;

      elsif File_Count /= 1 then
         declare
            use Ada.Text_IO;
         begin
            Put_Line
              (Standard_Error,
               "Error: Exactly one source file must be givven.");
         end;
         Result := Failure;
      end if;

      --  Make lemon copy of Ada option strings.
      Lemon_Program_Name  := New_String (Option_Program_Name.all);
      Lemon_Input_File    := New_String (Option_Input_File.all);
      Lemon_User_Template := New_String (Option_User_Template.all);
      Lemon_Output_Dir    := New_String (Option_Output_Dir.all);

   exception

      when Invalid_Switch =>
         Ada.Text_IO.Put_Line ("INVALID_SWITCH");

      when Invalid_Parameter =>
         Ada.Text_IO.Put_Line ("INVALID_PARAMETER");
         Result := Failure;

      when Exit_From_Command_Line =>
         Ada.Text_IO.Put_Line ("EXIt_FROM_COMMAND_LINE");
         Result := Bailout;

   end Process_Command_Line;


   procedure Main is
      use Ada.Command_Line;
      Status : Process_Result;
   begin
      Process_Command_Line (Status);

      case Status is
         when Success  =>  Set_Exit_Status (Ada.Command_Line.Success);
         when Failure  =>  Set_Exit_Status (Ada.Command_Line.Failure);  return;
         when Bailout  =>  Set_Exit_Status (Ada.Command_Line.Success);  return;
      end case;

      Lemon_Entry_Function;
   end Main;


end Command_Line;
