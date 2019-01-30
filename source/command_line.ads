--
--
--

package Command_Line is

   procedure Main;
   --  Program entry procedure.

   type Process_Result is
     (Success,  --  Command line is processed with success.
      Failure,  --  Failure in command line.
      Bailout   --  Ok, but stop program (--help / --version)
     );

   procedure Process_Command_Line
     (Result : out Process_Result);
   --  Process command line setting variables.

   procedure Lemon_Entry_Function;
   pragma Import (C, Lemon_Entry_Function, "lemon_main");
   --  Lemon entry function.

private



end Command_Line;
