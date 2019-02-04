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

with Command_Line;
with Database;
with Lime;
with Rules;
with Symbols;
with Auxiliary;

procedure Cherry_Program is

--   procedure Cherry_Main
--     (Lemon  : in out Lime.Lemon_Record;
--      Status :    out Ada.Command_Line.Exit_Status);

   use Interfaces.C;



   use Ada.Command_Line;
   use Command_Line;
   Status      : Process_Result;
   --  Main_Status : Exit_Status;
begin
   Process_Command_Line (Status);

   case Status is

      when Command_Line.Success  =>
         Set_Exit_Status (Ada.Command_Line.Success);

      when Command_Line.Failure  =>
         Set_Exit_Status (Ada.Command_Line.Failure);
         return;

      when Command_Line.Bailout  =>
         Set_Exit_Status (Ada.Command_Line.Success);
         return;

   end case;

--   procedure Cherry_Main
--     (Lemon  : in out Lime.Lemon_Record;


   --       is
   declare
      use Interfaces.C.Strings;
      use Ada.Text_IO;
      use Lime;
      use Symbols;
      use Rules;

      Lemon  : Lime.Lemon_Record renames Database.Lime_Lemp;
      --  Status : Ada.Command_Line.Exit_Status;

      --  Success : Ada.Command_Line.Exit_Status renames Ada.Command_Line.Success;
      Failure : Ada.Command_Line.Exit_Status renames Ada.Command_Line.Failure;

      I  : Symbol_Index;
      RP : Rules.Rule_Access;
   begin
      Lemon := Lime.Clean_Lemon;
      Lemon.Error_Cnt := 0;

      --  Initialize the machine
      Lime.Strsafe_Init;
      Symbols.Symbol_Init;
      Lime.State_Init;
      Lemon.Argv0            := New_String (Lime.Option_Program_Name.all);
      Lemon.File_Name        := New_String (Lime.Option_Input_File.all);
      Lemon.Basis_Flag       := Boolean'Pos (Lime.Option_Basis_Flag);
      Lemon.No_Line_Nos_Flag := Boolean'Pos (Lime.Option_No_Line_Nos);
      Symbols.Symbol_New_Proc ("$");

      --  Dump Ada mirror of lemon structure
      Put_Line ("Dumping Ada");
      Database.Dump (Lemon);

      --  Dump C mirror of lemon structure
      Put_Line ("Dumping C");
      Lime_Partial_Database_Dump_C;

      --  Parse the input file
      Lime.Parse (Lemon);

      if Lemon.Error_Cnt /= 0 then
         --  Status := Failure;
         Ada.Command_Line.Set_Exit_Status (Failure);
         return;
      end if;

      if Lemon.N_Rule = 0 then
         Put_Line (Standard_Error, "Empty grammar.");
         --  Status := Failure;
         Ada.Command_Line.Set_Exit_Status (Failure);
         return;
      end if;

      Lemon.Err_Sym := Symbols.Symbol_Find ("error");

      --  Count and index the symbols of the grammar
      Symbols.Symbol_New_Proc ("{default}");
      Lemon.N_Symbol := Symbols.Symbol_Count;
      Lemon.Symbols := new Symbol_Access_Array (0 .. Lemon.N_Symbol - 1);

      for I in 0 .. Lemon.N_Symbol - 1 loop
         Lemon.Symbols.all (I).Index := I;
      end loop;

      Symbols.Do_Sort (Container => Lemon.Symbols.all);

      for Idx in 0 .. Lemon.N_Symbol - 1 loop
         Lemon.Symbols.all (Idx).all.Index := Idx;
         I := Idx;  --  C for loop hack dehacked
      end loop;
      I := I + 1;   --  C for loop hack dehacked

      while Lemon.Symbols.all (I - 1).all.Kind = Symbols.MULTITERMINAL loop
         I := I - 1;
      end loop;

      pragma Assert (Lemon.Symbols.all (I - 1).Name = New_String ("{default}"));
      Lemon.N_Symbol := I - 1;

      I := 1;
      loop
         declare
            Text  : constant String    := Value (Lemon.Symbols.all (I).Name);
            First : constant Character := Text (Text'First);
         begin
            exit when Auxiliary.Is_Upper (First);
            I := I + 1;
         end;
      end loop;

      Lemon.N_Terminal := I;

      --  Assign sequential rule numbers.  Start with 0.  Put rules that have no
      --  reduce action C-code associated with them last, so that the switch()
      --  statement that selects reduction actions will have a smaller jump table.

      I := 0;
      RP := Lemon.Rule;
      loop
         exit when RP /= null;
         if RP.code /= Null_Ptr then
            RP.iRule := int (I);
            I := I + 1;
         else
            RP.iRule := -1;
         end if;
         RP := RP.next;
      end loop;

      I := 0;
      RP := Lemon.Rule;
      loop
         exit when RP = null;
         RP := RP.next;
      end loop;

      RP := Lemon.Rule;
      loop
         exit when RP = null;
         if RP.iRule < 0 then
            RP.iRule := int (I);
            I := I + 1;
         end if;
         RP := RP.next;
      end loop;

      Lemon.Start_Rule := Lemon.Rule;
      Lemon.Rule       := Rule_Sort (Lemon.Rule);

      --  Generate a reprint of the grammar, if requested on the command line
      declare
         --  procedure Generate_Reprint_Of_Grammar
         Base_Name     : constant chars_ptr  := New_String ("XXX"); --  in chars_ptr;
         Token_Prefix  : constant chars_ptr  := New_String ("XXX"); --  in chars_ptr;
         Terminal_Last : constant Natural := 999; --  in Natural);

         --  Generate a reprint of the grammar, if requested on the command line.
      begin

         if Option_RP_Flag then
            Reprint (Lemon);
         else
            Put_Line ("### 2-1");
            --  Initialize the size for all follow and first sets
            Set_Size (Terminal_Last + 1);
            Put_Line ("### 2-2");

            --  Find the precedence for every production rule (that has one)
            Find_Rule_Precedences (Lemon);
            Put_Line ("### 2-3");

            --  Compute the lambda-nonterminals and the first-sets for every
            --  nonterminal
            Find_First_Sets (Lemon);
            Put_Line ("### 2-4");

            --  Compute all LR(0) states.  Also record follow-set propagation
            --  links so that the follow-set can be computed later
            Compute_LR_States (Lemon);
            Put_Line ("### 2-5");
            --         Lemon_Lemp->nstate = 0;
            --         FindStates (Lemon_lemp);
            --         Lemon_Lemp->sorted = State_arrayof();

            --  Tie up loose ends on the propagation links
            Find_Links (Lemon);
            Put_Line ("### 2-6");

            --  Compute the follow set of every reducible configuration
            Find_Follow_Sets (Lemon);
            Put_Line ("### 2-7");

            --  Compute the action tables
            Find_Actions (Lemon);
            Put_Line ("### 2-8");

            --  Compress the action tables
            if not Option_Compress then
               Compress_Tables (Lemon);
            end if;
            Put_Line ("### 2-9");

            --  Reorder and renumber the states so that states with fewer choices
            --  occur at the end.  This is an optimization that helps make the
            --  generated parser tables smaller.
            if not Option_No_Resort then
               Resort_States (Lemon);
            end if;
            Put_Line ("### 2-10");

            --   Generate a report of the parser generated.  (the "y.output" file)
            if not Option_Be_Quiet then
               Report_Output (Lemon);
            end if;

            --  Generate the source code for the parser
            Report_Table (Lemon);

            --  Produce a header file for use by the scanner.  (This step is
            --  omitted if the "-m" option is used because makeheaders will
            --  generate the file for us.)
            Report_Header
              (Token_Prefix,
               Base_Name,
               New_String ("MODULE XXX"),
               Terminal_Last);

         end if;
      end;

      if Option_Statistics then
         declare

            procedure Stats_Line (Text : in String; Value : in Integer);
            procedure Stats_Line (Text : in String; Value : in Integer) is
               Line : String (1 .. 35) := (others => '.');
            begin
               Line (Line'First .. Text'Last) := Text;
               Line (Text'Last + 1) := ' ';
               Put (Line);
               Put (Integer'Image (Value));
               New_Line;
            end Stats_Line;

         begin
            Put_Line ("Parser statistics:");
            Stats_Line ("terminal symbols", Integer (Lemon.N_Terminal));
            Stats_Line ("non-terminal symbols", Integer (Lemon.N_Symbol - Lemon.N_Terminal));
            Stats_Line ("total symbols", Integer (Lemon.N_Symbol));
            Stats_Line ("rules", Lemon.N_Rule);
            Stats_Line ("states", Lemon.Nx_State);
            Stats_Line ("conflicts", Lemon.N_Conflict);
            Stats_Line ("action table entries", Lemon.N_Action_Tab);
            Stats_Line ("lookahead table entries", Lemon.N_Lookahead_Tab);
            Stats_Line ("total table size (bytes)", Lemon.Table_Size);
         end;
      end if;

      if Lemon.N_Conflict > 0 then
         Put_Line
           (Standard_Error,
            Integer'Image (Lemon.N_Conflict) & " parsing conflicts.");
      end if;

      if
        Lemon.Error_Cnt  > 0 or
        Lemon.N_Conflict > 0
      then
         Ada.Command_Line.Set_Exit_Status (Failure);
         return;
      end if;
   end;
   Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Success);

end Cherry_Program;