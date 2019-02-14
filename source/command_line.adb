--
--  The author disclaims copyright to this source code.  In place of
--  a legal notice, here is a blessing:
--
--    May you do good and not evil.
--    May you find forgiveness for yourself and forgive others.
--    May you share freely, not taking more than you give.
--

with Ada.Text_IO;
with Ada.Command_Line;

with Interfaces.C.Strings;

with GNAT.Command_Line;

with Lime;
with Options;

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
         Options.Input_File := new String'(Switch);
         File_Count        := Natural'Succ (File_Count);
      end if;
   end Getopt_Callback;


   procedure Parse (Success : out Boolean) is
   begin
      Options.Program_Name := new String'(Ada.Command_Line.Command_Name);

      Define_Switch (Config, Options.Basis_Flag'Access, "-b",
                     Help => "Print only the basis in report.");
      Define_Switch (Config, Options.Compress'Access, "-c",
                     Help => "Do not compress the action table.");
      Define_Switch (Config, Options.Output_Dir'Access, "-d=",
                     Help        => "Output directory. Default '.'",
                     Long_Switch => Long_Switch_Output_Dir & '=',
                     Argument => "DIR");
      Define_Switch (Config, "-D=",  --  Handled by GetOpt_Callback
                     Help        => "Define an %ifdef macro.",
                     Long_Switch => Switch_Define_Macro_Long & '=',
                     Argument => "MACRO");
      Define_Switch (Config, Options.Placeholder_Dummy'Access, "-f=",
                     Help     => "Placeholder for -f compiler options. Ignored.",
                     Argument => "IGNORE");
      Define_Switch (Config, Options.RP_Flag'Access, "-g",
                     Help => "Print grammar without actions.");
      Define_Switch (Config, Options.Show_Help'Access, "-h",
                     Help => "Show this help text.",
                     Long_Switch => Long_Switch_Show_Help);
      Define_Switch (Config, Options.Placeholder_Dummy'Access, "-I=",
                     Help     => "Placeholder for '-I' compiler options. Ignored.",
                     Argument => "IGNORE");
      Define_Switch (Config, Options.Language_String'Access, "-L=",
                     Help        => "Language (Ada, C). Default is 'C'.",
                     Long_Switch => Long_Switch_Language & '=',
                     Argument => "LANG");
      Define_Switch (Config, Options.MH_Flag'Access, "-m",
                     Help => "Output a makeheaders compatible file.");
      Define_Switch (Config, Options.No_Line_Nos'Access, "-l",
                     Help => "Do not print #line statements.");
      Define_Switch (Config, Options.Placeholder_Dummy'Access, "-O=",
                     Help => "Placeholder for '-O' compiler options. Ignored.",
                     Argument => "IGNORE");
      Define_Switch (Config, Options.Show_Conflict'Access, "-p",
                     Help => "Show conflicts resolved by precedence rules.");
      Define_Switch (Config, Options.Be_Quiet'Access, "-q",
                     Help => "Quiet. Do not print the report file.");
      Define_Switch (Config, Options.No_Resort'Access, "-r",
                     Help => "Do not sort or renumber states");
      Define_Switch (Config, Options.Statistics'Access, "-s",
                     Help => "Print parser stats to standard output.");
      Define_Switch (Config, Options.Show_Version'Access, "-x",
                     Help => "Show program name and version.");
      Define_Switch (Config, Options.User_Template'Access, "-T=",
                     Help => "Specify a template file.",
                     Long_Switch => Long_Switch_Template_File & '=',
                     Argument    => "FILE");
      Define_Switch (Config, Options.Show_Version'Access, "-v",
                     Help        => "Show program name and version.",
                     Long_Switch => Long_Switch_Show_Version);
      Define_Switch (Config, Options.Placeholder_Dummy'Access, "-W=",
                     Help     => "Placeholder for '-W' compiler options. Ignored.",
                     Argument => "IGNORE");
      Define_Switch (Config, "*", Help => "File to Parse (Typically parse.y).");

      --  Do the whole parsing business.
      --  Actually just the -D= option and file name.
      Getopt (Config, Getopt_Callback'Access);

      if
        Options.Show_Help    or
        Options.Show_Version
      then
         Success := True;
      elsif File_Count = 1 then
         Success := True;
      else
         raise Exit_From_Command_Line
           with "Error: Exactly one source file must be givven.";
      end if;

   exception

      when Invalid_Switch =>
         Ada.Text_IO.Put_Line ("INVALID_SWITCH");
         Success := False;

      when Invalid_Parameter =>
         Ada.Text_IO.Put_Line ("INVALID_PARAMETER");
         Success := False;

      when Exit_From_Command_Line =>
         Ada.Text_IO.Put_Line ("EXIt_FROM_COMMAND_LINE");
         Success := False;

   end Parse;


end Command_Line;
