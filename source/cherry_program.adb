--
--  The author disclaims copyright to this source code.  In place of
--  a legal notice, here is a blessing:
--
--    May you do good and not evil.
--    May you find forgiveness for yourself and forgive others.
--    May you share freely, not taking more than you give.
--

with Ada.Text_IO;
with Ada.Strings.Unbounded;
with Ada.Command_Line;
with Ada.Containers;

with Setup;
with Options;
with Command_Line;
with Lime;
with Cherry;
with Rules;
with Symbols;
with Parsers;
with Exceptions;
with Reports;
with Extras;

procedure Cherry_Program is

   procedure Put_Help;
   procedure Put_Version;

   procedure Put_Help is
   begin
      Cherry.Dummy;  -- XXX
      Reports.Dummy;
   end Put_Help;

   procedure Put_Version
   is
      use Ada.Text_IO, Setup;
      Version : constant String := Get_Program_Name & " (" & Get_Program_Version & ")";
      Build   : constant String := "Build (" & Get_Build_ISO8601_UTC & ")";
   begin
      Put_Line (Version);
      Put_Line (Build);
      New_Line;
      Put_Line ("The author disclaims copyright to this source code.  In place of");
      Put_Line ("a legal notice, here is a blessing:");
      New_Line;
      Put_Line ("   May you do good and not evil.");
      Put_Line ("   May you find forgiveness for yourself and forgive others.");
      Put_Line ("   May you share freely, not taking more than you give.");
      New_Line;
   end Put_Version;

   use Ada.Command_Line;

   Parse_Success : Boolean;
begin
   Command_Line.Parse (Parse_Success);

   if not Parse_Success then
      Set_Exit_Status (Ada.Command_Line.Failure);
      return;
   end if;

   if Options.Show_Version then
      Put_Version;
      return;
   end if;

   if Options.Show_Help then
      Put_Help;
      return;
   end if;

   Options.Set_Language;

   declare
      use Ada.Strings.Unbounded;
      use Ada.Text_IO;
      use Lime;
      use Symbols;
      use Rules;

      --  Lemon : Lime.Lemon_Record renames Database.Lemon;
      Lemon : aliased Lime.Lemon_Record;
      --  Status : Ada.Command_Line.Exit_Status;

      --  Success : Ada.Command_Line.Exit_Status renames Ada.Command_Line.Success;
      Failure : Ada.Command_Line.Exit_Status renames Ada.Command_Line.Failure;

--      I  : Symbol_Index;
--      RP : Rules.Rule_Access;
   begin
      Lemon := Lime.Clean_Lemon;
      Lemon.Error_Cnt := 0;

      --  Initialize the machine
      Lime.Strsafe_Init;
      Symbols.Symbol_Init;
      Lime.State_Init;

      Lemon.Argv0           := To_Unbounded_String (Options.Program_Name.all);
      Lemon.File_Name       := To_Unbounded_String (Options.Input_File.all);
      Lemon.Basis_Flag      := Options.Basis_Flag;
      Lemon.No_Linenos_Flag := Options.No_Line_Nos;

      --  Extras.Symbol_New_Proc (Extras.To_Name ("$"));
      Extras.Symbol_Append (Key => "$");

      --  Parse the input file
      Parsers.Parse (Lemon);

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

      --  Lemon.Err_Sym := Extras.Symbol_Find (Extras.To_Name ("error"));
      --  Extra.Error := Extras.Symbol_Find ("error");
      Extras.Set_Error;

      --  Count and index the symbols of the grammar
      --  Extras.Symbol_New_Proc (Extras.To_Name ("{default}"));
      Extras.Symbol_Append (Key => "{default}");
      Lemon.N_Symbol := Extras.Symbol_Count;

      --  Lemon.Symbols := new Symbol_Access_Array (0 .. Lemon.N_Symbol - 1);
      Symbols.Symbol_Allocate (Ada.Containers.Count_Type (Lemon.N_Symbol));

      Extras.Fill_And_Sort;

      Symbols.Do_Some_Things (Lemon.N_Symbol);  --  The following

      --  XXX Section XXX

--        for Idx in 0 .. Lemon.N_Symbol - 1 loop
--           Lemon.Symbols.all (Idx).all.Index := Idx;
--           I := Idx;  --  C for loop hack dehacked
--        end loop;
--        I := I + 1;   --  C for loop hack dehacked

--        while Lemon.Symbols.all (I - 1).all.Kind = Symbols.Multi_Terminal loop
--           I := I - 1;
--        end loop;

      --  XXX Section End XXX

--        pragma Assert (Lemon.Symbols.all (I - 1).Name = New_String ("{default}"));
--        Lemon.N_Symbol := I - 1;

--        I := 1;
--        loop
--           declare
--              Text  : constant String    := Value (Lemon.Symbols.all (I).Name);
--              First : constant Character := Text (Text'First);
--           begin
--              exit when Auxiliary.Is_Upper (First);
--              I := I + 1;
--           end;
--        end loop;

--        Lemon.N_Terminal := I;

      --  Assign sequential rule numbers.  Start with 0.  Put rules that have no
      --  reduce action C-code associated with them last, so that the switch()
      --  statement that selects reduction actions will have a smaller jump table.

      Rules.Assing_Sequential_Rule_Numbers
        (Lemon.Rule,
         Lemon.Start_Rule);

--        I := 0;
--        RP := Lemon.Rule;
--        loop
--           exit when RP /= null;
--           if RP.code /= Null_Ptr then
--              RP.iRule := int (I);
--              I := I + 1;
--           else
--              RP.iRule := -1;
--           end if;
--           RP := RP.next;
--        end loop;

--        I := 0;
--        RP := Lemon.Rule;
--        loop
--           exit when RP = null;
--           RP := RP.next;
--        end loop;

--        RP := Lemon.Rule;
--        loop
--           exit when RP = null;
--           if RP.iRule < 0 then
--              RP.iRule := int (I);
--              I := I + 1;
--           end if;
--           RP := RP.next;
--        end loop;

      Lemon.Start_Rule := Lemon.Rule;
      Lemon.Rule       := Rule_Sort (Lemon.Rule);

      --  Generate a reprint of the grammar, if requested on the command line
      if Options.RP_Flag then
         Reports.Reprint (Lemon);
      else
         Reports.Reprint_Of_Grammar
           (Lemon,
            Base_Name     => "XXX",
            Token_Prefix  => "YYY",
            Terminal_Last => 999);
      end if;

      if Options.Statistics then
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

exception

   when OCC : Command_Line.Parameter_Error =>
      Exceptions.Put_Message (OCC);


end Cherry_Program;
