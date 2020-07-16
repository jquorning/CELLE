--
--  The author disclaims copyright to this source code.  In place of
--  a legal notice, here is a blessing:
--
--    May you do good and not evil.
--    May you find forgiveness for yourself and forgive others.
--    May you share freely, not taking more than you give.
--

with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;
with Ada.Unchecked_Deallocation;
with Ada.Containers;

with Rules;
with Symbols;
with Report_Parsers;
with Actions;
with Action_Lists;
with Action_Tables;
with Action_Algorithms;
with Configs;
with States;
with Options;
with Generate_Ada;
with Generate_C;
with Backend;
with Symbol_Sets;
with Types;
with Templates;

with Auxiliary;

package body Reports is

   use type Types.Line_Number;

   subtype Symbol_Index  is Types.Symbol_Index;
   subtype Line_Number   is Types.Line_Number;
   subtype Offset_Type   is Types.Offset_Type;
   subtype Symbol_Access is Symbols.Symbol_Access;
   subtype Rule_Access   is Rules.Rule_Access;
   subtype Dot_Type      is Rules.Dot_Type;
   subtype Action_Record is Actions.Action_Record;
   subtype Action_Table  is Action_Tables.Table_Type;
   subtype Action_Value  is Action_Tables.Action_Value;
   subtype Config_Access is Configs.Config_Access;
   subtype State_Access  is States.State_Access;
   subtype State_Number  is States.State_Number;

   procedure Rule_Print (File   : File_Type;
                         Rule   : Rule_Access);
   procedure Rule_Print (File   : File_Type;
                         Rule   : Rule_Access;
                         Cursor : Dot_Type);
   --  Print the text of a rule. This procedure i overloaded !!!

   procedure Print_Action
     (Action :     Action_Record;
      File   :     File_Type;
      Indent :     Natural;
      Emit   : out Boolean);
   --  Print an Action to File with Indent indention. Return Falese if
   --  nothing was actually printed.

   procedure Config_Print (File   : File_Type;
                           Config : Config_Access);
   --  Print the rule for a configuration.

   function Minimum_Size_Type (LWS     :     Offset_Type;
                               UPR     :     Offset_Type;
                               PnBytes : out Integer)
                              return String;
   --  Return the name of a C datatype able to represent values between
   --  lwr and upr, inclusive.  If pnByte!=NULL then also write the sizeof
   --  for that type (1, 2, or 4) into *pnByte.

   function File_Makename (Session   : Session_Type;
                           Extension : String) return String;


   procedure Write_Rule_Text (File : File_Type;
                              Rule : Rule_Access);
   --  Write text on "out" that describes the rule "rp".

   procedure Emit_Destructor_Code
     (File    :        File_Type;
      Line    : in out Line_Number;
      Symbol  :        Symbol_Access;
      Session :        Session_Type);
   --  The following routine emits code for the destructor for the
   --  symbol sp

   procedure Translate_Code
     (Session :     Session_Type;
      Rule    :     Rule_Access;
      Length  : out Natural);
     --  Write and transform the rp->code string so that symbols are expanded.
     --  Populate the rp->codePrefix and rp->codeSuffix strings, as appropriate.
     --
     --  Return 1 if the expanded code requires that "yylhsminor" local variable
     --  to be defined.

   procedure Emit_Code
     (File    :        File_Type;
      Rule    :        Rule_Access;
      Session :        Session_Type;
      Line    : in out Line_Number);
   --  Generate code which executes when the rule "rp" is reduced.  Write
   --  the code to "out".  Make sure lineno stays up-to-date.

   procedure Print_Stack_Union
     (File        :        File_Type;
      Line        : in out Line_Number;
      Session     :        Session_Type;
      Make_Header :        Boolean);
   --  Print the definition of the union used for the parser's data stack.
   --  This union contains fields for every possible data type for tokens
   --  and nonterminals.  In the process of computing and printing this
   --  union, also set the ".dtnum" field of every terminal and nonterminal
   --  symbol.

   procedure Generate_Tokens
     (Session      : in Session_Type;
      Token_Prefix : in String;
      First        : in Integer;
      Last         : in Integer);
   --

   type Render_Record is
      record
         Nx_State         : Integer;
         N_Rule           : Integer;
         N_Terminal       : Integer;
         Min_Shift_Reduce : Action_Value;
         Err_Action       : Action_Value;
         Acc_Action       : Action_Value;
         No_Action        : Action_Value;
         Min_Reduce       : Action_Value;
         Num_Rule_With_Action : Natural;
      end record;

   procedure Render_Constants
     (File   :        File_Type;
      Render :        Render_Record;
      Line   : in out Line_Number);
   --
   --

   procedure Output_Action_Table
     (File      :        File_Type;
      Table     :        Action_Table;
      N         :        Integer;
      No_Action :        Action_Value;
      Line      : in out Line_Number);
   --
   --

   --  procedure Output_YY_Lookahead
   --    (File    :        File_Type;
   --     Table   :        Action_Table;
   --     N       :        Integer;
   --     Nsymbol :        Integer;
   --     Line    : in out Line_Number);
   --
   --

   procedure Output_YY_Shift_Offsets
     (File          :        File_Type;
      Session       :        Session_Type;
      N             :        Integer;
      MnTknOfst     :        Offset_Type;
      MxTknOfst     :        Offset_Type;
      Min_Size_Type :        String;
      Nactiontab    :        Integer;
      No_Offset     :        Offset_Type;
      Line          : in out Line_Number);

   --
   --
   --

   procedure Output_YY_Reduce_Offsets
     (File          :        File_Type;
      Session       :        Session_Type;
      N             :        Integer;
      MnNtOfst      :        Offset_Type;
      MxNtOfst      :        Offset_Type;
      Min_Size_Type :        String;
      No_Offset     :        Offset_Type;
      Line          : in out Line_Number);

   procedure Output_Default_Action_Table
     (File         :        File_Type;
      Session      :        Session_Type;
      N            :        Integer;
      Error_Action :        Action_Value;
      Min_Reduce   :        Action_Value;
      Line         : in out Line_Number);

--     procedure Template_Print_2
--       (Line        : in String;
--        No_Line_Nos : in Integer;
--        Out_Name    : in String);
   --  Print a string to the file and keep the linenumber up to date

   procedure Write_Arg_Defines
     (File    :        File_Type;
      Name    :        String;
      Arg_Ctx :        String;
      Extend  :        Boolean;
      Arg     :        String;
      Arg_I   :        String;
      Line    : in out Line_Number);

   procedure Write_Interface
     (File      :        File_Type;
      Name      :        String;
      Tokentype :        String;
      Line      : in out Line_Number);
   pragma Unreferenced (Write_Interface);
   --

   procedure Write_Interface_Begin
     (File        :        File_Type;
      Line        : in out Line_Number;
      Make_Header :        Boolean);

   procedure Write_Interface_End
     (File        :        File_Type;
      Line        : in out Line_Number;
      Make_Header :        Boolean);

   --  Each state contains a set of token transaction and a set of
   --  nonterminal transactions.  Each of these sets makes an instance
   --  of the following structure.  An array of these structures is used
   --  to order the creation of entries in the yy_action[] table.
   type AX_Record is
     record
        STP        : access States.State_Record; --  A pointer to a state
        Is_Token   : Boolean;
        --  True to use tokens.  False for non-terminals

        Num_Action : Action_Value;               --  Number of actions
        Order      : Integer;                    --  Original order of action sets
     end record;


   procedure Generate_Spec
     (Session   : Session_Type;
      Base_Name : String;
      Prefix    : String;    --  Prefix of symbols in spec
      Module    : String;    --  Prefix of symbols in spec
      First     : Integer;   --  Index of first symbol
      Last      : Integer);  --  Index of last symbol
   --  Create spec file with name File_Name including symols found by
   --  iterating from First to Last calling callback prepended with
   --  Suffix.


--   procedure Implementation_Open (File_Name : in String);
   --  Open a file for writing then implementaion (parse.adb/parse.c).
   --  File handler is located in the context structure.


   procedure Write_Include
     (File         :        File_Type;
      Line         : in out Line_Number;
      Include_Name :        String);
   --

   procedure Generate_The_Defines_1
     (File           :        File_Type;
      Line           : in out Line_Number;
      YY_Code_Type   :        String;
      Symbol_Count   :        Symbol_Index;
      YY_Action_Type :        String;
      Is_Wildcard    :        Boolean;
      Wildcard_Index :        Symbol_Index);
   --

   procedure Generate_The_Defines_2
     (File       :        File_Type;
      Line       : in out Line_Number;
      Stack_Size :        String);
   --

   type Mystruct_Record is record
      Use_Count : Integer;
      Index     : Symbol_Index;
      DT_Num    : Integer;
   end record;

   procedure Error_Fallback
     (File         :        File_Type;
      Line         : in out Line_Number;
      Error_Sym    :        String;
      Struct       :        Mystruct_Record;
      Has_Fallback :        Boolean);
   --
   --

   procedure Emit_SQL (File    : File_Type;
                       Session : Session_Type);
   --

   type AX_Set_Record is
      record
         Token        : AX_Record;
         Non_Terminal : AX_Record;
      end record;

   type AX_Set_Array is array (Symbol_Index range <>) of AX_Set_Record;
   type A_AX_Set_Array is access all AX_Set_Array;

   procedure Put (File : File_Type;
                  Item : String)
     renames Ada.Text_IO.Put;

   procedure Put_Line (File : File_Type;
                       Item : String := "")
     renames Ada.Text_IO.Put_Line;

   generic
      Line : in out Line_Number;
   procedure Increment;

   procedure Increment is
   begin
      Line := Line + 1;
   end Increment;

   -----------------------------------------------------------------------------

--   function Axset_Compare (A, B : AX_Set_Record) return Integer;
   --  Compare to axset structures for sorting purposes

--    static int axset_compare(const void *a, const void *b){
--    struct axset *p1 = (struct axset*)a;
--    struct axset *p2 = (struct axset*)b;
--    int c;
--    c = p2->nAction - p1->nAction;
--    if( c==0 ){
--      c = p1->iOrder - p2->iOrder;
--    }
--    assert( c!=0 || p1==p2 );
--    return c;
--  }

   -------------
   -- Reprint --
   -------------

   procedure Reprint (File    : File_Type;
                      Session : Session_Type)
   is
      use Ada.Strings.Unbounded;
      use Symbols;
      use type Rules.Rule_Access;
      use type Rules.Rule_Symbol_Access;
      use Types;

      package Symbol_Index_IO is
         new Ada.Text_IO.Integer_IO (Num => Types.Symbol_Index);

      Symbol : Symbol_Access;
      J      : Symbol_Index;
      Max_Len, Len, N_Columns, Skip : Integer;
      Column : Natural;
   begin
      Put (File, "// Reprint of input file """);
      Put (File, To_String (Unbounded_String'(Session.File_Name)));
      Put_Line (File, """.");
      Put_Line (File, "// Symbols:");
      Max_Len := 10;

      --  Determine Max_Len
      for I in 0 .. Symbols.Last_Index loop
         Symbol := Symbols.Element_At (Index => I);
         Len := Length (Symbol.Name);
         if Len > Max_Len then
            Max_Len := Len;
         end if;
      end loop;

      --  Determine N_Columns
      N_Columns := 76 / (Max_Len + 5);
      if N_Columns < 1 then
         N_Columns := 1;
      end if;

      --  Print symbol list
      Skip := (Integer (Session.Num_Symbol) + N_Columns - 1) / N_Columns;
      for I in 0 .. Skip - 1 loop
         Put (File, "//");
         J := Types.Symbol_Index (I);
         Column := 0;
         while J < Session.Num_Symbol loop
            Symbol := Symbols.Element_At (J);
            pragma Assert (Symbol.Index = J);

            Put (File, " ");
            Symbol_Index_IO.Put (J, Width => 3);
            Put (File, " ");
            declare
               Name  : constant String := Symbols.Name_Of (Symbol);
               Field : String (1 .. Max_Len) := (others => ' ');
            begin
               Field (Name'Range) := Name;
               Put (File, Field);
            end;
            J := J + Symbol_Index (Skip);
            Column := Column + 1;
         end loop;
         Put_Line (File, "");
      end loop;

      --  Print rules
      for Rule of Session.Rule loop
         Rule_Print (File, Rule);
         Put (File, ".");
         if Rule.Prec_Symbol /= null then
            Put (File, " [" & Name_Of (Symbol_Access (Rule.Prec_Symbol)) & "]");
         end if;
         --  /* if( rp->code ) printf("\n    %s",rp->code); */
         Put_Line (File, "");
      end loop;
   end Reprint;

   -------------------
   -- Report_Output --
   -------------------

   procedure Report_Output (Session : Session_Type)
   is
      use Ada.Text_IO;
      use Symbols;
      use Rules;
      use Configs;
      use type Types.Symbol_Index;

      subtype State_Number is States.State_Number;

      File : File_Type;

      Action_Result : Boolean;

      N      : Integer;
      State  : States.State_Access;
      Config : Config_Access;
   begin
--  fp = file_open(lemp,".out","wb");
      Ada.Text_IO.Open (File, Ada.Text_IO.Out_File, "XXX.out");

      for I in 0 .. Session.Nx_State - 1 loop
         State := Session.Sorted (I);
         Put (File, "State ");
         Put (File, State_Number'Image (State.Number));
         Put (File, ":");
         New_Line (File);

         if Session.Basis_Flag then
            Config := State.Basis;
         else
            Config := State.Config;
         end if;

         while Config /= null loop
            if Config.Dot = Dot_Type (Config.Rule.RHS.Length) then
               Put (File, "    (");
               Put (File, Config.Rule.Number'Image);
               Put (File, ") ");
            else
               Put (File, "          ");
            end if;
            Config_Print (File, Config);
            New_Line (File);

--      SetPrint(fp,cfp->fws,lemp);
--      PlinkPrint(fp,cfp->fplp,"To  ");
--      PlinkPrint(fp,cfp->bplp,"From");

            if Session.Basis_Flag then
               Config := Config.Basis;
            else
               Config := Config.Next;
            end if;
         end loop;
         New_Line (File);

         for Action of State.Action loop
            Print_Action (Action, File, 30, Action_Result);
            if Action_Result then
               New_Line (File);
            end if;
         end loop;
         New_Line (File);
      end loop;

      Put_Line (File, "----------------------------------------------------");
      Put_Line (File, "Symbols:");
      Put_Line (File, "The first-set of non-terminals is shown after the name.");
      New_Line (File);

      for I in 0 .. Symbols.Last_Index - 1 loop
         declare
            use type Symbol_Sets.Set_Type;

            Symbol : Symbol_Access;
         begin
            Symbol := Element_At (I);
            Put (File, "  " & I'Image & ": " & Name_Of (Symbol));
            if Symbol.Kind = Non_Terminal then
               Put (File, ":");
               if Symbol.Lambda then
                  Put (File, " <lambda>");
               end if;
               for J in 0 .. Session.Num_Terminal - 1 loop
                  if
                    Symbol.First_Set /= Symbol_Sets.Null_Set and then
                    Symbol_Sets.Set_Find (Symbol.First_Set, J)
                  then
                     Put (File, " ");
                     Put (File, Name_Of (Element_At (J)));
                  end if;
               end loop;
            end if;
            if Symbol.Precedence >= 0 then
               Put (File, " (precedence=");
               Put (File, Symbol.Precedence'Image);
               Put (File, ")");
            end if;
         end;
         New_Line (File);
      end loop;

      Put_Line (File, "----------------------------------------------------");
      Put_Line (File, "Syntax-only Symbols:");
      Put_Line (File, "The following symbols never carry semantic content.");
      New_Line (File);

      N := 0;
      for I in 0 .. Symbols.Last_Index loop
         declare
            W      : Integer;
            Symbol : constant Symbol_Access := Element_At (I);
         begin
            if not Symbol.Content then
               W := Ada.Strings.Unbounded.Length (Symbol.Name);
               if N > 0 and N + W > 75 then
                  New_Line (File);
                  N := 0;
               end if;
               if N > 0 then
                  Put (File, " ");
                  N := N + 1;
               end if;
               Put (File, Name_Of (Symbol));
               N := N + W;
            end if;
         end;
      end loop;
      if N > 0 then
         New_Line (File);
      end if;

      Put_Line (File, "----------------------------------------------------");
      Put_Line (File, "Rules:");

      for Rule of Session.Rule loop
         Put (File, Rule.Number'Image); -- XXX "%4d: ", rp->iRule);
         Put (File, ": ");
         Rule_Print (File, Rule);
         Put (File, ".");
         if Rule.Prec_Symbol /= null then
            Put (File, " [");
            Put (File, Name_Of (Symbol_Access (Rule.Prec_Symbol)));
            Put (File, " precedence=");
            Put (File, Rule.Prec_Symbol.Precedence'Image);
            Put (File, "]");
         end if;
         New_Line (File);
      end loop;

      Close (File);
   end Report_Output;

   --------------
   -- Emit_SQL --
   --------------

   procedure Emit_SQL (File    : File_Type;
                       Session : Session_Type)
   is
      subtype Rule_Number is Rules.Rule_Number;
      use type Rule_Number;
      --  use type Ada.Containers.Count_Type;
      use type Symbol_Index;
      use type Symbols.Symbol_Kind;

      I : Rule_Number;
      J : Symbol_Index;
   begin
      Put_Line (File, "BEGIN;");
      Put_Line (File, "CREATE TABLE symbol(");
      Put_Line (File, "  id INTEGER PRIMARY KEY,");
      Put_Line (File, "  name TEXT NOT NULL,");
      Put_Line (File, "  isTerminal BOOLEAN NOT NULL,");
      Put_Line (File, "  fallback INTEGER REFERENCES symbol"
                  & " DEFERRABLE INITIALLY DEFERRED");
      Put_Line (File, ");");

      for I in 0 .. Session.Num_Symbol - 1 loop
         Put (File,  "INSERT INTO symbol(id,name,isTerminal,fallback) ");
         Put (File, " VALUES(");
         Put (File, I'Image);  Put (File, ",'");
         Put (File, Symbols.Name_Of (Symbols.Element_At (I))); Put (File, "',");
         Put (File, (if I < Session.Num_Terminal then "TRUE" else "FALSE"));
         Put (File, ",");

         if Symbols.Element_At (I).Fallback /= null then
            Put (File, Symbols.Element_At (I).Fallback.Index'Image);
            Put_Line (File, ");");
         else
            Put_Line (File, "NULL);");
         end if;
      end loop;

      Put_Line (File, "CREATE TABLE rule(");
      Put_Line (File, "  ruleid INTEGER PRIMARY KEY,");
      Put_Line (File, "  lhs INTEGER REFERENCES symbol(id),");
      Put_Line (File, "  txt TEXT");
      Put_Line (File, ");");
      Put_Line (File, "CREATE TABLE rulerhs(");
      Put_Line (File, "  ruleid INTEGER REFERENCES rule(ruleid),");
      Put_Line (File, "  pos INTEGER,");
      Put_Line (File, "  sym INTEGER REFERENCES symbol(id)");
      Put_Line (File, ");");

      I := 0;
      for Rule of Session.Rule loop
         pragma Assert (I = Rule.Number);

         Put (File, "INSERT INTO rule(ruleid,lhs,txt) VALUES(");
         Put (File, Rule.Number'Image);     Put (File, ",");
         Put (File, Rule.LHS.Index'Image);  Put (File, ",'");

         Write_Rule_Text (File, Rule);
         Put_Line (File, "');");

         J := 0;
         for Symbol of Rule.RHS loop
            if Symbol.Kind /= Symbols.Multi_Terminal then
               Put (File, "INSERT INTO rulerhs(ruleid,pos,sym)VALUES(");
               Put (File, I'Image);      Put (File, ",");
               Put (File, J'Image);      Put (File, ",");
               Put (File, Symbol.Index'Image); Put (File, ");");
               Put_Line (File, "");
            else
               for Sub_Symbol of Symbol.Sub_Symbol loop
                  Put (File, "INSERT INTO rulerhs(ruleid,pos,sym)VALUES(");
                  Put (File, I'Image);      Put (File, ",");
                  Put (File, J'Image);      Put (File, ",");
                  Put (File, Sub_Symbol.Index'Image); Put (File, ");");
                  Put_Line (File, "");
               end loop;
            end if;
            J := J + 1;
         end loop;

         I := I + 1;
      end loop;
      Put_Line (File, "COMMIT;");
   end Emit_SQL;

   ------------------
   -- Report_Table --
   ------------------

   procedure Report_Table
     (Session            : in out Session_Type;
      Make_Headers       :        Boolean;
      Generate_SQL       :        Boolean;
      User_Template_Name :        String)
   is
      use Ada.Strings.Unbounded;
      use Sessions;
      use type Action_Tables.Action_Value;

      Column_Count : constant := 8;

      type Column_Number is mod Column_Count;

      procedure Close (File : in out File_Type) renames Ada.Text_IO.Close;

      Session_Name : constant String := To_String (Session.Names.Name);

      Out_File : File_Type;
      SQL_File : File_Type;
      File     : File_Type renames Out_File;

      --    char line[LINESIZE];
      Lineno : Types.Line_Number := 0;
      State : States.State_Access;
--    struct action *ap;
--      Rule  : Rules.Rule_Access;

      Act_Tab : Action_Tables.Table_Access;
--    int i, j, n, sz;
      I  : Integer;
      N  : Integer;
      SZ : Integer;
      Size_Of_Action_Type : Integer;
      Size_Of_Code_Type   : Integer;
--    const char *name;
      Mn_Tkn_Ofst, Mx_Tkn_Ofst : Offset_Type;
      Mn_Nt_Ofst,  Mx_Nt_Ofst  : Offset_Type;

      AX : A_AX_Set_Array;

      Template_Open_Success : Integer;
      Error_Count           : Natural := 0;

      procedure Increment_Line is new Increment (Lineno);
   begin
      Session.Min_Shift_Reduce := Action_Value (Session.Sorted.Length);
--      Session.Err_Action       := Session.Min_Shift_Reduce + Session.N_Rule;
      Session.Err_Action       := Session.Min_Shift_Reduce + Action_Value (Session.Rule.Length);
      Session.Acc_Action       := Session.Err_Action + 1;
      Session.No_Action        := Session.Acc_Action + 1;
      Session.Min_Reduce       := Session.No_Action + 1;
--      Session.Max_Action       := Session.Min_Reduce + Session.N_Rule;
      Session.Max_Action       := Session.Min_Reduce + Action_Value (Session.Rule.Length);

      Templates.Open (User_Template_Name, Error_Count, Template_Open_Success);
      Auxiliary.Recreate (File, Ada.Text_IO.Out_File,
                          File_Name => File_Makename (Session, ".c"));

      if Generate_SQL then
         declare
            use Ada.Text_IO;
         begin
            Open (SQL_File, Ada.Text_IO.Out_File, "XXX" & ".sql");

         exception
            when others =>
               --               Close (Input_File);
               Close (Out_File);
               return;
         end;

         Emit_SQL (SQL_File, Session);

      end if;
  --  if( sqlFlag==0 ){
  --    sql = 0;
  --  }else{
  --    sql = file_open(lemp, ".sql", "wb");
  --    if( sql==0 ){
  --      fclose(in);
  --      fclose(out);
  --      return;
  --    }
  --  }
  --  lineno = 1;

      Templates.Transfer (File, Session_Name, Lineno);

      --  Generate the include code, if any
      --  Sessions_Print (Session.Outname, Session.No_Linenos_Flag, Session.Include);
      Templates.Print (File, Ada.Strings.Unbounded.To_String (Session.Out_Name),
                       Session.No_Linenos_Flag,
                       To_String (Session.Names.Include), Lineno);
      --  lime_print (lime_get_ouüt_name (), lemp->nolinenosflag, lemap->include);
      --  lime_write_include (lime_get_mh_flag(), file_makename(lemp, ".h"));
      Write_Include (File, Lineno, File_Makename (Session, ".h"));

      Templates.Transfer (File, Session_Name, Lineno);

      --  Generate #defines for all tokens
--  XXX    Sessions_Session_Copy := Session;
      --  lime_generate_tokens (lime_get_mh_flag(), lemp->tokenprefix, 1, lemp->nterminal);
      Generate_Tokens (Session, To_String (Session.Names.Token_Prefix),
                       1, Integer (Session.Num_Terminal));

      Templates.Transfer (File, Session_Name, Lineno);

      --  Generate the defines
      declare
         use Symbols;

         Code     : constant String :=
           Minimum_Size_Type (0,
                              Offset_Type (Symbols.Last_Index),
                              Size_Of_Code_Type);

         Action   : constant String :=
           Minimum_Size_Type (0,
                              Offset_Type (Session.Max_Action),
                              Size_Of_Action_Type);

         Wildcard    : constant Symbol_Access := Session.Wildcard;
         Is_Wildcard : constant Boolean       := Wildcard /= null;
      begin
         if Is_Wildcard then
            Generate_The_Defines_1
              (File, Lineno, Code,
               Symbols.Last_Index,
               Action,
               Is_Wildcard    => True,
               Wildcard_Index => Wildcard.Index);
         else
            Generate_The_Defines_1
            (File, Lineno, Code,
             Symbols.Last_Index,
             Action,
             Is_Wildcard    => False,
             Wildcard_Index => 0);
         end if;
      end;

      --  print_stack_union (lemp, lime_get_mh_flag());
      Print_Stack_Union (File, Lineno, Session, Make_Headers);
      Generate_The_Defines_2 (File, Lineno, To_String (Session.Names.Stack_Size));

      Write_Interface_Begin (File, Lineno, Make_Headers);

      declare

         function Get_Name return String;

         function Get_Name return String is
         begin
            if Session.Names.Name = "" then
               return "Parse";
            else
               return To_String (Session.Names.Name);
            end if;
         end Get_Name;


         Name : constant String := Get_Name;

         use Report_Parsers;
         ARG   : constant String := Get_ARG (Get_Context);
         CTX   : constant String := Get_CTX (Get_Context);
         ARG_I : Natural;
         CTX_I : Natural;
      begin
         Trim_Right_Symbol (ARG, ARG_I);
         if ARG = "" then
            Write_Arg_Defines (File, Name, "ARG", False, "", "", Lineno);
         else
            Write_Arg_Defines (File, Name, "ARG", True, ARG,
                               ARG (ARG_I .. ARG'Last), Lineno);
         end if;

         Trim_Right_Symbol (CTX, CTX_I);
         if CTX = "" then
            Write_Arg_Defines (File, Name, "CTX", False, "", "", Lineno);
         else
            Write_Arg_Defines (File, Name, "CTX", True,
                               CTX, CTX (CTX_I .. CTX'Last), Lineno);
         end if;
      end;

      Write_Interface_End (File, Lineno, Make_Headers);

      Error_Fallback (File, Lineno, "",
                      Struct => (Use_Count => Session.Error_Symbol.Use_Count,
                                 Index     => Session.Error_Symbol.Index,
                                 DT_Num    => Session.Error_Symbol.DT_Num),
                      Has_Fallback => Session.Has_Fallback);

      --  Compute the action table, but do not output it yet.  The action
      --  table must be computed before generating the YYNSTATE macro because
      --  we need to know how many states can be eliminated.

      --      AX := (struct axset *) calloc(lemp->nxstate*2, sizeof(ax[0]));
      --  AX := new AX_Set_Record;  --  (struct axset *) calloc(lemp->nxstate*2, sizeof(ax[0]));
      declare
         use type Types.Symbol_Index;
      begin
         AX := new AX_Set_Array (0 .. Session.Nx_State - 1);

      --  if( ax==0 ){
      --    fprintf(stderr,"malloc failed\n");
      --    exit(1);

         for I in 0 .. Session.Nx_State - 1 loop
            State := Session.Sorted (I);

            AX (I).Token := (STP        => State,
                             Is_Token   => True,
                             Num_Action => State.Num_Token,
                             Order      => <>);

            AX (I).Non_Terminal := (STP        => State,
                                    Is_Token   => False,
                                    Num_Action => State.Num_Nonterminal,
                                    Order      => <>);
         end loop;
      end;

      Mx_Tkn_Ofst := 0;
      Mn_Tkn_Ofst := 0;
      Mx_Nt_Ofst  := 0;
      Mn_Nt_Ofst  := 0;

      --  In an effort to minimize the action table size, use the heuristic
      --  of placing the largest action sets first
--    for(i=0; i<lemp->nxstate*2; i++) ax[i].iOrder = i;
--    qsort(ax, lemp->nxstate*2, sizeof(ax[0]), axset_compare);
      Act_Tab := Action_Tables.Alloc (Integer (Symbols.Last_Index),
                                      Integer (Session.Num_Terminal));
--    for(i=0; i<lemp->nxstate*2 && ax[i].nAction>0; i++){
--      stp = ax[i].stp;
--      if( ax[i].isTkn ){
--        for(ap=stp->ap; ap; ap=ap->next){
--          int action;
--          if( ap->sp->index>=lemp->nterminal ) continue;
--          action = compute_action(lemp, ap);
--          if( action<0 ) continue;
--          acttab_action(pActtab, ap->sp->index, action);
--        }
--        stp->iTknOfst = acttab_insert(pActtab, 1);
--        if( stp->iTknOfst<mnTknOfst ) mnTknOfst = stp->iTknOfst;
--        if( stp->iTknOfst>mxTknOfst ) mxTknOfst = stp->iTknOfst;
--      }else{
--        for(ap=stp->ap; ap; ap=ap->next){
--          int action;
--          if( ap->sp->index<lemp->nterminal ) continue;
--          if( ap->sp->index==lemp->nsymbol ) continue;
--          action = compute_action(lemp, ap);
--          if( action<0 ) continue;
--          acttab_action(pActtab, ap->sp->index, action);
--        }
--        stp->iNtOfst = acttab_insert(pActtab, 0);
--        if( stp->iNtOfst<mnNtOfst ) mnNtOfst = stp->iNtOfst;
--        if( stp->iNtOfst>mxNtOfst ) mxNtOfst = stp->iNtOfst;
--      }
--
--  #if 0  /* Uncomment for a trace of how the yy_action[] table fills out */
--      { int jj, nn;
--        for(jj=nn=0; jj<pActtab->nAction; jj++){
--          if( pActtab->aAction[jj].action<0 ) nn++;
--        }
--        printf("%4d: State %3d %s n: %2d size: %5d freespace: %d\n",
--               i, stp->statenum, ax[i].isTkn ? "Token" : "Var  ",
--               ax[i].nAction, pActtab->nAction, nn);
--      }
--  #endif
--    }
--    free(ax);
--
--    /* Mark rules that are actually used for reduce actions after all
--    ** optimizations have been applied
--    */
--    for(rp=lemp->rule; rp; rp=rp->next) rp->doesReduce = LEMON_FALSE;
--    for(i=0; i<lemp->nxstate; i++){
--      for(ap=lemp->sorted[i]->ap; ap; ap=ap->next){
--        if( ap->type==REDUCE || ap->type==SHIFTREDUCE ){
--          ap->x.rp->doesReduce = 1;
--        }
--      }
--    }

      --  Finish rendering the constants now that the action table has
      --  been computed
      Render_Constants
        (File,
         Render =>
           (Nx_State         => Natural (Session.Nx_State),
            N_Rule           => Integer (Session.Rule.Length),
            N_Terminal       => Integer (Session.Num_Terminal),
            Min_Shift_Reduce => Session.Min_Shift_Reduce,
            Err_Action       => Session.Err_Action,
            Acc_Action       => Session.Acc_Action,
            No_Action        => Session.No_Action,
            Min_Reduce       => Session.Min_Reduce,
            Num_Rule_With_Action => Session.Num_Rule_With_Action),
        Line => Lineno);

      Templates.Transfer (File, Session_Name, Lineno);


      --
      --  Now output the action table and its associates:
      --
      --  yy_action[]        A single table containing all actions.
      --  yy_lookahead[]     A table containing the lookahead for each entry in
      --                     yy_action.  Used to detect hash collisions.
      --  yy_shift_ofst[]    For each state, the offset into yy_action for
      --                     shifting terminals.
      --  yy_reduce_ofst[]   For each state, the offset into yy_action for
      --                     shifting non-terminals after a reduce.
      --  yy_default[]       Default action for each state.

      --
      --  Output the yy_action table
      --
      Session.Num_Action_Tab := Action_Tables.Action_Size (Act_Tab.all);
      N := Session.Num_Action_Tab;
      Session.Table_Size := Session.Table_Size + N * Size_Of_Action_Type;

      Output_Action_Table (File, Act_Tab.all, N, Session.No_Action, Lineno);

      --
      --  Output the yy_lookahead table -- lemon.c:4484
      --
      Output_YY_Lookahead_Table :
      declare
         package Integer_IO is new Ada.Text_IO.Integer_IO (Integer);
         package Action_IO  is new Ada.Text_IO.Integer_IO (Action_Value);
         package Symbol_IO  is new Ada.Text_IO.Integer_IO (Symbol_Index);

         use Integer_IO, Action_IO, Symbol_IO;

         procedure Increment_Line is new Increment (Lineno);

         Column : Column_Number := Column_Number'First;

         Index          : Natural;
         Look_Action    : Action_Value;
         Num_Look_Ahead : Natural;
      begin
         Session.Num_Lookahead_Tab := Action_Tables.Lookahead_Size (Act_Tab.all);
         N                         := Session.Num_Lookahead_Tab;
         Session.Table_Size        := Session.Table_Size + N * Size_Of_Code_Type;

         Put_Line (File, "static const YYCODETYPE yy_lookahead[] = {");
         Increment_Line;

         for Index in 0 .. N - 1 loop

            Look_Action := Act_Tab.Action (Index).Lookahead;
            if Look_Action < 0 then
               Look_Action := Action_Value (Session.Num_Symbol);
            end if;

            if Column = Column_Number'First then
               Put (File, " /* ");
               Put (File, Index);
               Put (File, " */ ");
            end if;

            Put (File, " ");
            Put (File, Look_Action);
            Put (File, ",");

            if Column = Column_Number'Last or Index = N - 1 then
               Put_Line (File);
               Increment_Line;
            end if;
            Column := Column + 1;

         end loop;
         Index := N - 1;  --  Dirty C trick using loop variable

         --  Add extra entries to the end of the yy_lookahead[] table so that
         --  yy_shift_ofst[]+iToken will always be a valid index into the array,
         --  even for the largest possible value of yy_shift_ofst[] and iToken.
         Num_Look_Ahead := Natural (Session.Num_Terminal) + Session.Num_Action_Tab;

         while Index < Num_Look_Ahead loop

            if Column = Column_Number'First then
               Put (File, " /* ");
               Put (File, Index, Width => 5);
               Put (File, " */ ");
            end if;

            Put (File, " ");
            Put (File, Session.Num_Terminal, Width => 4);
            Put (File, ",");

            if Column = Column_Number'Last then
               Put_Line (File);
               Increment_Line;
            end if;
            Column := Column + 1;

            Index := Index + 1;
         end loop;
         if Column > Column_Number'First then
            Put_Line (File);
            Increment_Line;
         end if;
         Put_Line (File, "};");
         Increment_Line;
      end Output_YY_Lookahead_Table;

      --  Output the yy_shift_ofst[] table

      N := Natural (Session.Nx_State);
--      while  N > 0 and Session.Sorted(N - 1).I_Tkn_Ofst = NO_Offset loop
--         N := N - 1;
--      end loop;
--
--    lime_lemp = lemp;
      declare
         use type Offset_Type;
         Num_Terminal : constant Offset_Type := Offset_Type (Session.Num_Terminal);
         Num_Actions  : constant Offset_Type := Offset_Type (Session.Num_Action_Tab);
         Upper        : constant Offset_Type := Num_Terminal + Num_Actions;
         Data_Type    : constant String      := Minimum_Size_Type (Mn_Tkn_Ofst,
                                                                   Upper, SZ);
      begin
         Output_YY_Shift_Offsets
           (File,
            Session, N,
            Mn_Tkn_Ofst,
            Mx_Tkn_Ofst,
            Data_Type,
            Session.Num_Action_Tab,
            No_Offset,
            Lineno);
      end;
      Session.Table_Size := Session.Table_Size + N * SZ;

      --
      --  Output the yy_reduce_ofst[] table
      --
      N := Natural (Session.Nx_State);
--    while( n>0 && lemp->sorted[n-1]->iNtOfst==NO_OFFSET ) n--;
--
      declare
         use type Offset_Type;
      begin
         Output_YY_Reduce_Offsets
           (File,
            Session, N,
            Mn_Nt_Ofst,
            Mx_Nt_Ofst,
            Minimum_Size_Type (Mn_Nt_Ofst - 1, Mx_Nt_Ofst, SZ),
            No_Offset,
            Lineno);
      end;
      Session.Table_Size := Session.Table_Size + N * SZ;

      --
      --  Output the default action table
      --
      Output_Default_Action_Table
        (File,
         Session, Natural (Session.Nx_State),
         Session.Err_Action,
         Session.Min_Reduce,
         Lineno);
      Session.Table_Size := Session.Table_Size + N * Size_Of_Action_Type;

      Templates.Transfer (File, Session_Name, Lineno);

      --
      --  Generate the table of fallback tokens.
      --
      if Session.Has_Fallback then
         declare
            use Symbols;
            use Types;

            MX : Symbol_Index := Session.Num_Terminal - 1;
         begin
            --  while MX > 0 and Session.Symbols (MX).Fallback = 0 loop
            while
              MX > 0 and
              Element_At (Index => MX).Fallback = null
            loop
               MX := MX - 1;
            end loop;
            Session.Table_Size := Session.Table_Size + Integer (MX + 1) * Size_Of_Code_Type;

            for I in 0 .. MX loop
               declare
                  P : constant Symbol_Access := Element_At (I);
               begin
                  if P.Fallback = null then
                     Put (File, "    0,  /* ");
                     Put (File, Name_Of (P));
                     Put (File, " => nothing */");
                     Put_Line (File, "");
                  else
                     Put (File, "  ");
                     Put (File, Symbol_Index'Image (P.Fallback.Index));
                     Put (File, ",  /* ");
                     Put (File, Name_Of (P));
                     Put (File, " => ");
                     Put (File, To_String (P.Fallback.Name));
                     Put (File, " */");
                     Put_Line (File, "");
                  end if;
                  Increment_Line;
               end;
            end loop;
         end;
      end if;

      Templates.Transfer (File, Session_Name, Lineno);

      --
      --  Generate A Table Containing the symbolic name of every symbol
      --
      declare
         use Symbols;
         use Types;

         J : Integer;
      begin
         for I in Symbol_Index range 0 .. Symbols.Last_Index - 1 loop
            declare
               Name : constant String := Name_Of (Element_At (I));
            begin
               --  Session_Sprintf (Line, """" & Name & """,");
               Put (File, "  /* ");
               Put (File, Symbol_Index'Image (I));
               Put (File, " */ """);
               Put (File, Name);
               Put (File, """,");
               Put_Line (File, "");
               Increment_Line;
            end;
         end loop;

         Templates.Transfer (File, Session_Name, Lineno);

         --  Generate a table containing a text string that describes every
         --  rule in the rule set of the grammar.  This information is used
         --  when tracing REDUCE actions.
         J  := 0;

         for Rule of Session.Rule loop
            pragma Assert (Rules."=" (Rule.Number, Rules.Rule_Number (J)));
            Put (File, " /* ");
            Put (File, Integer'Image (J));
            Put (File, " */ """);
            Write_Rule_Text (File, Rule);
            Put_Line (File, """,");
            Put_Line (File, "");
            Increment_Line;
         end loop;
      end;

      Templates.Transfer (File, Session_Name, Lineno);

      --  Generate code which executes every time a symbol is popped from
      --  the stack while processing errors or while destroying the parser.
      --  (In other words, generate the %destructor actions)

--    if( lemp->tokendest ){
--      int once = 1;
--      for(i=0; i<lemp->nsymbol; i++){
--        struct symbol *sp = lemp->symbols[i];
--        if( sp==0 || sp->type!=TERMINAL ) continue;
--        if( once ){
--          lime_put_line ("      /* TERMINAL Destructor */");
--          once = 0;
--        }
--        lime_put ("    case ");
--        lime_put_int (sp->index);
--        lime_put (": /* ");
--        lime_put (sp->name);
--        lime_put_line (" */");
--      }
--      for(i=0; i<lemp->nsymbol && lemp->symbols[i]->type!=TERMINAL; i++);
      declare
         use Symbols;
         use Types;
      begin
         I := 0;
         loop
            exit when I >= Integer (Symbols.Last_Index);
            exit when Element_At (Symbol_Index (I)).Kind = Terminal;
            I := I + 1;
         end loop;

         --  I : Symbols.Symbol_Index;
         --      if( i<lemp->nsymbol ){
         Emit_Destructor_Code (File, Lineno, Element_At (Symbol_Index (I)), Session);
         --        lime_put_line ("      break;");
         --      }
      end;
--    }
--
--    if( lemp->vardest ){
--      struct symbol *dflt_sp = 0;
--      int once = 1;
--      for(i=0; i<lemp->nsymbol; i++){
--        struct symbol *sp = lemp->symbols[i];
--        if( sp==0 || sp->type==TERMINAL ||
--            sp->index<=0 || sp->destructor!=0 ) continue;
--        if( once ){
--          lime_put_line ("      /* Default NON-TERMINAL Destructor */");
--          once = 0;
--        }
--        lime_put ("    case ");
--        lime_put_int (sp->index);
--        lime_put (": /* ");
--        lime_put (sp->name);
--        lime_put_line (" */");
--        dflt_sp = sp;
--      }
--      if( dflt_sp!=0 ){
--        emit_destructor_code (dflt_sp, lemp); // , &lineno);
--      }
--      lime_put_line ("      break;");
--    }
--    for(i=0; i<lemp->nsymbol; i++){
--      struct symbol *sp = lemp->symbols[i];
--      if( sp==0 || sp->type==TERMINAL || sp->destructor==0 ) continue;
--      if( sp->destLineno<0 ) continue;  /* Already emitted */
--
--      lime_put ("    case ");
--      lime_put_int (sp->index);
--      lime_put (": /* ");
--      lime_put (sp->name);
--      lime_put_line (" */");
--
--      /* Combine duplicate destructors into a single case */
--      for(j=i+1; j<lemp->nsymbol; j++){
--        struct symbol *sp2 = lemp->symbols[j];
--        if( sp2 && sp2->type!=TERMINAL && sp2->destructor
--            && sp2->dtnum==sp->dtnum
--            && strcmp(sp->destructor,sp2->destructor)==0 )
--          {
--           lime_put ("    case ");
--           lime_put_int (sp2->index);
--           lime_put (": /* ");
--           lime_put (sp2->name);
--           lime_put_line (" */");
--           sp2->destLineno = -1;  /* Avoid emitting this destructor again */
--          }
--      }
--
--      emit_destructor_code (lemp->symbols[i], lemp); // , &lineno);
--      lime_put_line ("      break;");
--    }
--
--    lime_template_transfer (lemp->name);
--
--    /* Generate code which executes whenever the parser stack overflows */
--    lime_template_print (lemp->overflow, lemp->nolinenosflag, lemp->outname);
--    //lime_template_print (lemp->overflow, lemp->nolinenosflag, lime_get_out_name ());
--    lime_template_transfer (lemp->name);

      --  Generate the tables of rule information.  yyRuleInfoLhs[] and
      --  yyRuleInfoNRhs[].
      --
      --  Note: This code depends on the fact that rules are number
      --  sequentually beginning with 0.
      declare
         Index : Natural := 0;
      begin
         for Rule of Session.Rule loop
            Put (File, "  ");
            Put (File, Symbol_Index'Image (Rule.LHS.Index));
            Put (File, ", /* (");
            Put (File, Natural'Image (I));
            Put (File, ") ");
            Rule_Print (File, Rule);
            Put (File, " */");
            Put_Line (File);
            Increment_Line;
            Index := Index + 1;
         end loop;
      end;

      Templates.Transfer (File, Session_Name, Lineno);
--
--    for(i=0, rp=lemp->rule; rp; rp=rp->next, i++){
--      lime_put ("  ");
--      lime_put_int (-rp->nrhs);
--      lime_put (",  /* (");
--      lime_put_int (i);
--      lime_put (") ");
--      rule_print (file, rp);
--      lime_put_line (" */");
--    }
--
      Templates.Transfer (File, Session_Name, Lineno);

      --  Generate code which execution during each REDUCE action
      Generate_Code_For_Reduce_Action :
      declare
         Total  : Natural := 0;
         Length : Natural;
      begin
         for Rule of Session.Rule loop
            Translate_Code (Session, Rule, Length);
            Total := Total + Length;
         end loop;

         if Total /= 0 then
            Put_Line (File, "        YYMINORTYPE yylhsminor;");
            Increment_Line;
         end if;
      end Generate_Code_For_Reduce_Action;

      --  First output rules other than the default: rule
      for Rule of Session.Rule loop
      --      struct rule *rp2;               /* Other rules with the same action */
--      if( rp->codeEmitted ) continue;
--      if( rp->noCode ){
--        /* No C code actions, so this will be part of the "default:" rule */
--        continue;
--      }
--      lime_put ("      case ");
--      lime_put_int (rp->iRule);
--      lime_put (": /* ");
--      writeRuleText (rp);
--      lime_put_line (" */");
--
--      for(rp2=rp->next; rp2; rp2=rp2->next){
--        if( rp2->code==rp->code && rp2->codePrefix==rp->codePrefix
--               && rp2->codeSuffix==rp->codeSuffix )
--          {
--            lime_put ("      case ");
--            lime_put_int (rp2->iRule);
--            lime_put (": /* ");
--            writeRuleText (rp2);
--            lime_put (" */ yytestcase(yyruleno==");
--            lime_put_int (rp2->iRule);
--            lime_put_line (");");
--            rp2->codeEmitted = 1;
--          }
--      }
         Emit_Code (Out_File, Rule, Session, Lineno);
--      lime_put_line ("        break;");
--      rp->codeEmitted = 1;
      end loop;
--
--    /* Finally, output the default: rule.  We choose as the default: all
--    ** empty actions. */
--    lime_put_line ("      default:");
--    for(rp=lemp->rule; rp; rp=rp->next){
--      if( rp->codeEmitted ) continue;
--      assert( rp->noCode );
--      lime_put ("      /* (");
--      lime_put_int (rp->iRule);
--      lime_put (") ");
--      writeRuleText (rp);
--      if( rp->doesReduce ){
--        lime_put (" */ yytestcase(yyruleno==");
--        lime_put_int (rp->iRule);
--        lime_put_line (");");
--      }else{
--        lime_put (" (OPTIMIZED OUT) */ assert(yyruleno!=");
--        lime_put_int (rp->iRule);
--        lime_put_line (");");
--      }
--    }
--    lime_put_line ("        break;");
--
--    lime_template_transfer (lemp->name);
--
--    /* Generate code which executes if a parse fails */
--    lime_template_print (lemp->failure, lemp->nolinenosflag, lemp->outname);
--    //lime_template_print (lemp->failure, lemp->nolinenosflag, lime_get_out_name ());
--    lime_template_transfer (lemp->name);
--
--    /* Generate code which executes when a syntax error occurs */
--    lime_template_print (lemp->error, lemp->nolinenosflag, lemp->outname);
--    //lime_template_print (lemp->error, lemp->nolinenosflag, lime_get_out_name ());
--    lime_template_transfer (lemp->name);
--    printf ("### 2-55\n");
--
--    /* Generate code which executes when the parser accepts its input */
--    lime_template_print (lemp->accept, lemp->nolinenosflag, lemp->outname);
--    //lime_template_print (lemp->accept, lemp->nolinenosflag, lime_get_out_name ());
--    printf ("### 2-56\n");
--
--    lime_template_transfer (lemp->name);
--    printf ("### 2-57\n");
--
--    /* Append any addition code the user desires */
--    lime_template_print (lemp->extracode, lemp->nolinenosflag, lemp->outname);
--    //lime_template_print (lemp->extracode, lemp->nolinenosflag, lime_get_out_name ());
--    printf ("### 2-58\n");

      --      Ada.Text_IO.Close (File_In);
      Close (Backend.Context.File_Template);
--      Ada.Text_IO.Close (File_Out);
      if Generate_SQL then
         Close (SQL_File);
      end if;
      --    printf ("### 2-58\n");
   end Report_Table;

   ---------------------
   -- Compress_Tables --
   ---------------------

   procedure Compress_Tables (Session : in out Session_Type)
   is
      package Lists renames Action_Lists.Action_DLLs;

      use Lists;
      use Actions;
      use Rules.RHS_Vectors;

      use type Ada.Containers.Count_Type;
      use type Rule_Access;
      use type Symbol_Index;

      subtype Action_Cursor is Lists.Cursor;

      No_Element : Action_Cursor renames Lists.No_Element;

      Rule   : Rule_Access;
      Rule_2 : Rule_Access;

      R_Best : Rule_Access;
      N_Best : Integer;
      N      : Integer;

      Uses_Wildcard : Boolean;
   begin

      for State of Session.Sorted loop
         N_Best := 0;
         R_Best := null;
         Uses_Wildcard := False;

         declare
            Action_Cur   : Action_Cursor;
            Action_Cur_2 : Action_Cursor;
            Action   : Action_Record renames Lists.Element (Action_Cur);
            Action_2 : Action_Record renames Lists.Element (Action_Cur_2);
         begin
            Action_Cur := State.Action.First;
            while Action_Cur /= No_Element loop
               if Action.Kind = Shift and Action.Symbol = Session.Wildcard then
                  Uses_Wildcard := True;
               end if;
               if Action.Kind /= Reduce then goto Continue_Outer; end if;
               Rule := Action.X.Rule;
               if Rule.LHS_Start then goto Continue_Outer; end if;
               if Rule = R_Best  then goto Continue_Outer; end if;

               N := 1;
               Action_Cur_2 := Next (Action_Cur);
               while Action_Cur_2 /= No_Element loop
                  if Action_2.Kind /= Reduce then goto Continue_Inner; end if;
                  Rule_2 := Action_2.X.Rule;
                  if Rule_2 = R_Best then goto Continue_Inner; end if;
                  if Rule_2 = Rule   then N := N + 1; end if;

                  <<Continue_Inner>>
                  Action_Cur_2 := Next (Action_Cur_2);
               end loop;

               if N > N_Best then
                  N_Best := N;
                  R_Best := Rule;
               end if;

               <<Continue_Outer>>
               Action_Cur := Next (Action_Cur);
            end loop;
         end;

         --  Do not make a default if the number of rules to default
         --  is not at least 1 or if the wildcard token is a possible
         --  lookahead.
         if N_Best < 1 or Uses_Wildcard then goto Outmost; end if;

         --  Combine matching REDUCE actions into a single default
         declare
            Action_Cur : Action_Cursor;
            Action     : Action_Record renames Lists.Element (Action_Cur);
         begin
            Action_Cur := State.Action.First;
            while Action_Cur /= No_Element loop
               exit when Action.Kind = Reduce and Action.X.Rule = R_Best;
               Next (Action_Cur);
            end loop;
            pragma Assert (Action_Cur /= No_Element);

            --  Update Symbol
            declare
               Item : Action_Record := Action;
            begin
               Item.Symbol := Symbols.Create ("{default}");
               State.Action.Replace_Element (Action_Cur, Item);
            end;

            Next (Action_Cur);
            while Action_Cur /= No_Element loop
               if Action.Kind = Reduce and Action.X.Rule = R_Best then
                  declare
                     Item : Action_Record := Action;
                  begin
                     Item.Kind := Not_Used;
                     State.Action.Replace_Element (Action_Cur, Item);
                  end;
               end if;
               Next (Action_Cur);
            end loop;
            Action_Lists.Sort (State.Action);
         end;

         declare
            Action_Cur : Action_Cursor;
            Action     : Action_Record renames Lists.Element (Action_Cur);
         begin
            Action_Cur := State.Action.First;
            while Action_Cur /= No_Element loop
               exit when Action.Kind = Shift;
               exit when Action.Kind = Reduce and Action.X.Rule /= R_Best;
               Next (Action_Cur);
            end loop;

            if Action_Cur = No_Element then
               State.Auto_Reduce         := True;
               State.Default_Reduce_Rule := R_Best;
            end if;
         end;

         <<Outmost>>
         null;
      end loop;

      --  Make a second pass over all states and actions.  Convert
      --  every action that is a SHIFT to an autoReduce state into
      --  a SHIFTREDUCE action.
      for State of Session.Sorted loop

         declare
            Action_Cur : Action_Cursor;
            Action     : Action_Record renames Lists.Element (Action_Cur);
         begin
            Action_Cur := State.Action.First;
            while Action_Cur /= No_Element loop
               if Action.Kind /= Shift then goto Local; end if;
               declare
                  Next_State : constant State_Access := Action.X.State;
               begin
                  if
                    Next_State.Auto_Reduce and then
                    Next_State.Default_Reduce_Rule /= null
                  then
                     declare
                        Item : Action_Record := Action;
                     begin
                        Item.Kind   := Shift_Reduce;
                        Item.X.Rule := Next_State.Default_Reduce_Rule;
                        State.Action.Replace_Element (Action_Cur, Item);
                     end;
                  end if;
               end;

               <<Local>>
               Next (Action_Cur);
            end loop;
         end;

      end loop;

      --  If a SHIFTREDUCE action specifies a rule that has a single RHS term
      --  (meaning that the SHIFTREDUCE will land back in the state where it
      --  started) and if there is no C-code associated with the reduce action,
      --  then we can go ahead and convert the action to be the same as the
      --  action for the RHS of the rule.

      for State of Session.Sorted loop

         declare
            Action_Cur   : Action_Cursor;
            Action_Cur_2 : Action_Cursor;
            Action       : Action_Record renames Lists.Element (Action_Cur);
            Action_2     : Action_Record renames Lists.Element (Action_Cur);
            Next_Action  : Action_Cursor;
         begin
            Action_Cur := State.Action.First;
            while Action_Cur /= No_Element loop
               Next (Action_Cur);
               if Action.Kind /= Shift_Reduce then goto Looper; end if;
               Rule := Action.X.Rule;
               if Rule.No_Code = False then goto Looper; end if;
               if Length (Rule.RHS) /= 1 then goto Looper; end if;
               --  #if 1
               --  Only apply this optimization to non-terminals.  It would be OK to
               --  apply it to terminal symbols too, but that makes the parser tables
               --  larger.
               if Action.Symbol.Index < Session.Num_Terminal then goto Looper; end if;
               --  #endif

               --  If we reach this point, it means the optimization can be applied
               Next_Action := Action_Cur;
               Action_Cur_2 := State.Action.First;
               while
                 Action_Cur_2 /= No_Element and then
                 (Action_Cur_2 = Action_Cur or
                    Action_2.Symbol /= Rule.LHS)
               loop
                  Next (Action_Cur_2);
               end loop;

               pragma Assert (Action_Cur_2 /= No_Element);
               declare
                  Item : Action_Record := Action;
               begin
                  Item.Symbol_Link := Action_2.Symbol;
                  Item.Kind        := Action_2.Kind;
                  Item.X           := Action_2.X;
                  State.Action.Replace_Element (Action_Cur, Item);
               end;

               <<Looper>>
               Action_Cur := Next_Action;
            end loop;
         end;

      end loop;

   end  Compress_Tables;

   -------------------
   -- Resort_States --
   -------------------
   --  lemon.c:4866

   procedure Resort_States (Session : in out Session_Type)
   is
      use Sessions.State_Vectors;

      use type Sessions.State_Index;
      use type Action_Tables.Action_Value;

      subtype State_Index is Sessions.State_Index;

      Num_State : constant State_Index :=
        State_Index (Length (Session.Sorted));

      State   : State_Access;
--      Action  : Action_Record; -- Access;
   begin
      for I in 0 .. Num_State - 1 loop
         State := Session.Sorted (I);

         State.Default_Reduce  := States.Syntax_Error;
         State.Num_Token       := (if State.Num_Nonterminal = 0
                                     then 1 else 0);
         State.Token_Offset    := Sessions.No_Offset;
         State.Nonterm_Offset  := Sessions.No_Offset;

         for Action of State.Action loop
            declare
               I_Action : constant Integer :=
                 Action_Algorithms.Compute_Action (Session, Action);
            begin
               if I_Action >= 0 then

                  if Action.Symbol.Index < Session.Num_Terminal then
                     State.Num_Token := State.Num_Token + 1;

                  elsif Action.Symbol.Index < Session.Num_Symbol then
                     State.Num_Nonterminal := State.Num_Nonterminal + 1;

                  else
                     pragma Assert
                       (not State.Auto_Reduce or else
                          State.Default_Reduce_Rule = Action.X.Rule);

                     State.Default_Reduce :=
                       (if I_Action /= 0 then States.True else States.False);
                  end if;

               end if;
            end;
--            Action := Action.Next;
         end loop;
      end loop;
--  qsort(&lemp->sorted[1], lemp->nstate-1, sizeof(lemp->sorted[0]),
--        stateResortCompare);
      for I in 0 .. Num_State - 1 loop
         Session.Sorted (I).Number := State_Number (I);
      end loop;
      Session.Nx_State := Num_State;
      while
        Session.Nx_State > 1 and
        Session.Sorted (Session.Nx_State - 1).Auto_Reduce
      loop
         Session.Nx_State := Session.Nx_State - 1;
      end loop;

   end Resort_States;

   ----------------
   -- Rule_Print --
   ----------------
   --  lemon.c:3157 - OVERLOADED

   procedure Rule_Print (File   : File_Type;
                         Rule   : Rule_Access;
                         Cursor : Dot_Type)
   is
      use Ada.Strings.Unbounded;
      use Symbols;
      use type Rules.Dot_Type;
   begin
      --  Print LHS
      Put (File, To_String (Rule.LHS.Name));
      Put (File, " ::=");

      --  Print RHS
      for I in Rule.RHS.First_Index .. Rule.RHS.Last_Index loop

         if I = Cursor then
            Put (File, " *");
         end if;

         exit when I = Rule.RHS.Last_Index;

         declare
            Symbol : constant Symbols.Symbol_Access :=
              Symbol_Access (Rule.RHS.Element (I));

            First  : Boolean := True;
         begin
            if Symbol.Kind = Symbols.Multi_Terminal then
               Put (File, " ");
               for Sub_Symbol of Symbol.Sub_Symbol loop
                  if not First then
                     Put (File, "|");
                  end if;
                  Put (File, To_String (Sub_Symbol.Name));
                  First := False;
               end loop;
            else
               Put (File, " ");
               Put (File, To_String (Symbol.Name));
            end if;
         end;
      end loop;
   end Rule_Print;

   ------------------
   -- Print_Action --
   ------------------

   procedure Print_Action
     (Action :     Action_Record;
      File   :     File_Type;
      Indent :     Natural;
      Emit   : out Boolean)
   is
      use Actions;

      subtype Rule_Access is Rules.Rule_Access;
      subtype Rule_Number is Rules.Rule_Number;

      procedure Put_Indent (Item : String);

      function Name_Of (Symbol : Symbols.Symbol_Access) return String
        renames Symbols.Name_Of;

      procedure Put_Indent (Item : String) is
         Image : String (1 .. Indent) := (others => ' ');
      begin
         Image (Image'Last - Item'Length .. Image'Last) := Item;
         Put (File, Image);
      end Put_Indent;

      package Rule_IO  is new Ada.Text_IO.Integer_IO (Rule_Number);
      package State_IO is new Ada.Text_IO.Integer_IO (State_Number);
      use Rule_IO, State_IO;

      Ignore      : constant Dot_Type     := Dot_Type'(Rules.Ignore);
      Symbol_Name : constant String       := Name_Of (Action.Symbol);
      State_Num   : constant State_Number := Action.X.State.Number;
      Rule        : constant Rule_Access  := Action.X.Rule;
   begin
      Rule_IO .Default_Width := 7;
      State_IO.Default_Width := 7;

      Emit := True;

      case Action.Kind is

         when Shift =>
            Put_Indent (Symbol_Name);
            Put (File, " shift        ");
            Put (File, State_Num);

         when Reduce =>
            Put_Indent (Symbol_Name);
            Put (File, " reduce       ");
            Put (File, Rule.Number);
            Rule_Print (File, Rule, Ignore);

         when Shift_Reduce =>
            Put_Indent (Symbol_Name);
            Put (File, " shift-reduce ");
            Put (File, Rule.Number);
            Rule_Print (File, Rule, Ignore);

         when C_Accept =>
            Put_Indent (Symbol_Name);
            Put (File, " accept");

         when Error =>
            Put_Indent (Symbol_Name);
            Put (File, " error");

         when SR_Conflict | RR_Conflict =>
            Put_Indent (Symbol_Name);
            Put (File, " reduce       ");
            Put (File, Rule.Number);
            Put (File, " ** Parsing conflict **");

         when SS_Conflict =>
            Put_Indent (Symbol_Name);
            Put (File, " shift        ");
            Put (File, Action.X.State.Number);
            Put (File, " ** Parsing conflict **");

         when SH_Resolved =>
            if Options.Show_Conflict then
               Put_Indent (Symbol_Name);
               Put (File, " shift        ");
               Put (File, Action.X.State.Number);
               Put (File, " -- dropped by precedence");
            else
               Emit := False;
            end if;

         when RD_Resolved =>
            if Options.Show_Conflict then
               Put_Indent (Symbol_Name);
               Put (File, " reduce       ");
               Put (File, Action.X.State.Number);
               Put (File, " -- dropped by precedence");
            else
               Emit := False;
            end if;

         when Not_Used =>
            Emit := False;

      end case;

      if Emit and Action.Symbol_Link /= null then
         Put (File, "  /* because ");
         Put (File, Symbol_Name);
         Put (File, "==");
         Put (File, Name_Of (Action.Symbol_Link));
         Put (File, " */");
      end if;

   end Print_Action;

   ------------------
   -- Config_Print --
   ------------------

   procedure Config_Print (File   : File_Type;
                           Config : Config_Access)
   is
   begin
      Rule_Print (File, Config.Rule, Config.Dot);
   end Config_Print;

   -----------------------
   -- Minimum_Size_Type --
   -----------------------

   function Minimum_Size_Type (LWS     : in     Offset_Type;
                               UPR     : in     Offset_Type;
                               PnBytes :    out Integer)
                              return String
   is
      pragma Unreferenced (LWS, UPR, PnBytes);
   begin
--  static const char *minimum_size_type(int lwr, int upr, int *pnByte){
--    const char *zType = "int";
--    int nByte = 4;
--    if( lwr>=0 ){
--      if( upr<=255 ){
--        zType = "unsigned char";
--        nByte = 1;
--      }else if( upr<65535 ){
--        zType = "unsigned short int";
--        nByte = 2;
--      }else{
--        zType = "unsigned int";
--        nByte = 4;
--      }
--    }else if( lwr>=-127 && upr<=127 ){
--      zType = "signed char";
--      nByte = 1;
--    }else if( lwr>=-32767 && upr<32767 ){
--      zType = "short";
--      nByte = 2;
--    }
--    if( pnByte ) *pnByte = nByte;
--    return zType;
--  }
      return "";
   end Minimum_Size_Type;

   -------------------
   -- File_Makename --
   -------------------

   function File_Makename (Session   : in Sessions.Session_Type;
                           Extension : in String) return String
   is
      use Ada.Strings;
      use Ada.Strings.Unbounded;

      File_Name    : constant String  := To_String (Session.File_Name);
      Dot_Position : constant Natural := Fixed.Index (File_Name, ".", Backward);
   begin
      if Dot_Position = 0 then
         return File_Name & Extension;
      else
         return File_Name (File_Name'First .. Dot_Position) & Extension;
      end if;
   end File_Makename;

   ---------------------
   -- Write_Rule_Text --
   ---------------------

   procedure Write_Rule_Text (File : File_Type;
                              Rule : Rule_Access)
   is
      use Symbols;
   begin
      Put (File, Name_Of (Symbol_Access (Rule.LHS)));
      Put (File, " ::=");
      for Symbol of Rule.RHS loop
         Put (File, " ");

         if Symbol.Kind = Multi_Terminal then
            Put (File, Name_Of (Symbol.Sub_Symbol.First_Element));
            for K in 1 .. Symbol.Sub_Symbol.Last_Index loop
               Put (File, "|");
               Put (File, Name_Of (Symbol.Sub_Symbol.Element (K)));
            end loop;
         else
            Put (File, Name_Of (Symbol_Access (Symbol)));
         end if;

      end loop;
   end Write_Rule_Text;

   --------------------------
   -- Emit_Destructor_Code --
   --------------------------

   procedure Emit_Destructor_Code
     (File    :        File_Type;
      Line    : in out Line_Number;
      Symbol  :        Symbol_Access;
      Session :        Session_Type)
   is
      use Ada.Strings.Unbounded;
      use type Symbols.Symbol_Kind;

      procedure Increment_Line is new Increment (Line);

      Item : access Unbounded_String;
   begin

      if Symbol.Kind = Symbols.Terminal then
         Item := Session.Names.Token_Dest'Access;
         if Item.all = Null_Unbounded_String then
            return;
         end if;
         Put_Line (File, "{");  Increment_Line;

      elsif Symbol.Destructor = Null_Unbounded_String then
         Item := Symbol.Destructor'Access;
         Put_Line (File, "{");  Increment_Line;
         if not Session.No_Linenos_Flag then
            Line := Line + 1;
            Templates.Line_Directive (File, Symbol.Dest_Lineno,
                                      To_String (Session.File_Name));
         end if;

      elsif Session.Names.Var_Dest = Null_Unbounded_String then
         Item := Session.Names.Var_Dest'Access;
         if Item.all = Null_Unbounded_String then
            return;
         end if;
         Put_Line (File, "{");  Increment_Line;

      else
         pragma Assert (False);  --  Cannot happen
      end if;

      for Index in 1 .. Length (Item.all) loop
         if
           Element (Item.all, Index + 0) = '$' and then
           Element (Item.all, Index + 1) = '$'
         then
            Put (File, "(yypminor->yy");
            Put (File, Integer'Image (Symbol.DT_Num));
            Put (File, ")");
         else
            if Element (Item.all, Index) = ASCII.LF then
               Increment_Line;
            end if;
            Put (File, "" & Element (Item.all, Index));
         end if;
      end loop;

      Put_Line (File, "");  Increment_Line;

      if not Session.No_Linenos_Flag then
         Increment_Line;
         Templates.Line_Directive (File, Line, To_String (Session.Out_Name));
      end if;

      Put_Line (File, "}");  Increment_Line;
   end Emit_Destructor_Code;

   --------------------
   -- Translate_Code --
   --------------------

   procedure Translate_Code
     (Session :     Session_Type;
      Rule    :     Rule_Access;
      Length  : out Natural)
   is
   begin
      null;
--  PRIVATE int translate_code(struct lemon *lemp, struct rule *rp){
--    char *cp, *xp;
--    int i;
--    int rc = 0;            /* True if yylhsminor is used */
--    int dontUseRhs0 = 0;   /* If true, use of left-most RHS label is illegal */
--    const char *zSkip = 0; /* The zOvwrt comment within rp->code, or NULL */
--    char lhsused = 0;      /* True if the LHS element has been used */
--    char lhsdirect;        /* True if LHS writes directly into stack */
--    char used[MAXRHS];     /* True for each RHS element which is used */
--    char zLhs[50];         /* Convert the LHS symbol into this string */
--    char zOvwrt[900];      /* Comment that to allow LHS to overwrite RHS */

--    for(i=0; i<rp->nrhs; i++) used[i] = 0;
--    lhsused = 0;

--    if( rp->code==0 ){
--      static char newlinestr[2] = { '\n', '\0' };
--      rp->code = newlinestr;
--      rp->line = rp->ruleline;
--      rp->noCode = 1;
--    }else{
--      rp->noCode = 0;
--    }


--    if( rp->nrhs==0 ){
--      /* If there are no RHS symbols, then writing directly to the LHS is ok */
--      lhsdirect = 1;
--    }else if( rp->rhsalias[0]==0 ){
--      /* The left-most RHS symbol has no value.  LHS direct is ok.  But
--      ** we have to call the distructor on the RHS symbol first. */
--      lhsdirect = 1;
--      if( has_destructor(rp->rhs[0],lemp) ){
--        append_str(0,0,0,0);
--        append_str("  yy_destructor(yypParser,%d,&yymsp[%d].minor);\n", 0,
--                   rp->rhs[0]->index,1-rp->nrhs);
--        rp->codePrefix = Strsafe(append_str(0,0,0,0));
--        rp->noCode = 0;
--      }
--    }else if( rp->lhsalias==0 ){
--      /* There is no LHS value symbol. */
--      lhsdirect = 1;
--    }else if( strcmp(rp->lhsalias,rp->rhsalias[0])==0 ){
--      /* The LHS symbol and the left-most RHS symbol are the same, so
--      ** direct writing is allowed */
--      lhsdirect = 1;
--      lhsused = 1;
--      used[0] = 1;
--      if( rp->lhs->dtnum!=rp->rhs[0]->dtnum ){
--        ErrorMsg(lemp->filename,rp->ruleline,
--          "%s(%s) and %s(%s) share the same label but have "
--          "different datatypes.",
--          rp->lhs->name, rp->lhsalias, rp->rhs[0]->name, rp->rhsalias[0]);
--        lemp->errorcnt++;
--      }
--    }else{
--      lemon_sprintf(zOvwrt, "/*%s-overwrites-%s*/",
--                    rp->lhsalias, rp->rhsalias[0]);
--      zSkip = strstr(rp->code, zOvwrt);
--      if( zSkip!=0 ){
--        /* The code contains a special comment that indicates that it is safe
--        ** for the LHS label to overwrite left-most RHS label. */
--        lhsdirect = 1;
--      }else{
--        lhsdirect = 0;
--      }
--    }
--    if( lhsdirect ){
--      sprintf(zLhs, "yymsp[%d].minor.yy%d",1-rp->nrhs,rp->lhs->dtnum);
--    }else{
--      rc = 1;
--      sprintf(zLhs, "yylhsminor.yy%d",rp->lhs->dtnum);
--    }

--    append_str(0,0,0,0);

--    /* This const cast is wrong but harmless, if we're careful. */
--    for(cp=(char *)rp->code; *cp; cp++){
--      if( cp==zSkip ){
--        append_str(zOvwrt,0,0,0);
--        cp += lemonStrlen(zOvwrt)-1;
--        dontUseRhs0 = 1;
--        continue;
--      }
--      if( ISALPHA(*cp) && (cp==rp->code || (!ISALNUM(cp[-1]) && cp[-1]!='_')) ){
--        char saved;
--        for(xp= &cp[1]; ISALNUM(*xp) || *xp=='_'; xp++);
--        saved = *xp;
--        *xp = 0;
--        if( rp->lhsalias && strcmp(cp,rp->lhsalias)==0 ){
--          append_str(zLhs,0,0,0);
--          cp = xp;
--          lhsused = 1;
--        }else{
--          for(i=0; i<rp->nrhs; i++){
--            if( rp->rhsalias[i] && strcmp(cp,rp->rhsalias[i])==0 ){
--              if( i==0 && dontUseRhs0 ){
--                ErrorMsg(lemp->filename,rp->ruleline,
--                   "Label %s used after '%s'.",
--                   rp->rhsalias[0], zOvwrt);
--                lemp->errorcnt++;
--              }else if( cp!=rp->code && cp[-1]=='@' ){
--                /* If the argument is of the form @X then substituted
--                ** the token number of X, not the value of X */
--                append_str("yymsp[%d].major",-1,i-rp->nrhs+1,0);
--              }else{
--                struct symbol *sp = rp->rhs[i];
--                int dtnum;
--                if( sp->type==MULTITERMINAL ){
--                  dtnum = sp->subsym[0]->dtnum;
--                }else{
--                  dtnum = sp->dtnum;
--                }
--                append_str("yymsp[%d].minor.yy%d",0,i-rp->nrhs+1, dtnum);
--              }
--              cp = xp;
--              used[i] = 1;
--              break;
--            }
--          }
--        }
--        *xp = saved;
--      }
--      append_str(cp, 1, 0, 0);
--    } /* End loop */

--    /* Main code generation completed */
--    cp = append_str(0,0,0,0);
--    if( cp && cp[0] ) rp->code = Strsafe(cp);
--    append_str(0,0,0,0);

--    /* Check to make sure the LHS has been used */
--    if( rp->lhsalias && !lhsused ){
--      ErrorMsg(lemp->filename,rp->ruleline,
--        "Label \"%s\" for \"%s(%s)\" is never used.",
--          rp->lhsalias,rp->lhs->name,rp->lhsalias);
--      lemp->errorcnt++;
--    }

--    /* Generate destructor code for RHS minor values which are not referenced.
--    ** Generate error messages for unused labels and duplicate labels.
--    */
--    for(i=0; i<rp->nrhs; i++){
--      if( rp->rhsalias[i] ){
--        if( i>0 ){
--          int j;
--          if( rp->lhsalias && strcmp(rp->lhsalias,rp->rhsalias[i])==0 ){
--            ErrorMsg(lemp->filename,rp->ruleline,
--              "%s(%s) has the same label as the LHS but is not the left-most "
--              "symbol on the RHS.",
--              rp->rhs[i]->name, rp->rhsalias[i]);
--            lemp->errorcnt++;
--          }
--          for(j=0; j<i; j++){
--            if( rp->rhsalias[j] && strcmp(rp->rhsalias[j],rp->rhsalias[i])==0 ){
--              ErrorMsg(lemp->filename,rp->ruleline,
--                "Label %s used for multiple symbols on the RHS of a rule.",
--                rp->rhsalias[i]);
--              lemp->errorcnt++;
--              break;
--            }
--          }
--        }
--        if( !used[i] ){
--          ErrorMsg(lemp->filename,rp->ruleline,
--            "Label %s for \"%s(%s)\" is never used.",
--            rp->rhsalias[i],rp->rhs[i]->name,rp->rhsalias[i]);
--          lemp->errorcnt++;
--        }
--      }else if( i>0 && has_destructor(rp->rhs[i],lemp) ){
--        append_str("  yy_destructor(yypParser,%d,&yymsp[%d].minor);\n", 0,
--           rp->rhs[i]->index,i-rp->nrhs+1);
--      }
--    }

--    /* If unable to write LHS values directly into the stack, write the
--    ** saved LHS value now. */
--    if( lhsdirect==0 ){
--      append_str("  yymsp[%d].minor.yy%d = ", 0, 1-rp->nrhs, rp->lhs->dtnum);
--      append_str(zLhs, 0, 0, 0);
--      append_str(";\n", 0, 0, 0);
--    }

--    /* Suffix code generation complete */
--    cp = append_str(0,0,0,0);
--    if( cp && cp[0] ){
--      rp->codeSuffix = Strsafe(cp);
--      rp->noCode = 0;
--    }

--    return rc;
   end Translate_Code;

   ---------------
   -- Emit_Code --
   ---------------

   procedure Emit_Code
     (File    :        File_Type;
      Rule    :        Rule_Access;
      Session :        Session_Type;
      Line    : in out Line_Number)
   is
      use Ada.Strings.Unbounded;

      Code_Prefix : String renames To_String (Rule.Code_Prefix);
      Code        : String renames To_String (Rule.Code);
      Code_Suffix : String renames To_String (Rule.Code_Suffix);
   begin
      --  Setup code prior to the #line directive
      if Code_Prefix /= "" then
         Put (File, "{");
         Put (File, Code_Prefix);
         for Char of Code_Prefix loop
            if Char = ASCII.LF then
               Line := Line + 1;
            end if;
         end loop;
      end if;

      --  Generate code to do the reduce action
      if Code /= "" then
         if not Session.No_Linenos_Flag then
            Line := Line + 1;
            Templates.Line_Directive (File, Line, To_String (Session.File_Name));
         end if;
         Put (File, "{");
         Put (File, Code);
         for Char of Code loop
            if Char = ASCII.LF then
               Line := Line + 1;
            end if;
         end loop;
         Put_Line (File, "}");
         Line := Line + 1;
         if not Session.No_Linenos_Flag then
            Line := Line + 1;
            Templates.Line_Directive (File, Line, To_String (Session.Out_Name));
         end if;
      end if;

      --  Generate breakdown code that occurs after the #line directive
      if Code_Suffix /= "" then
         Put (File, Code_Suffix);
         for Char of Code_Suffix loop
            if Char = ASCII.LF then
               Line := Line + 1;
            end if;
         end loop;
      end if;

      if Code_Prefix /= "" then
         Put_Line (File, "}");
      end if;

   end Emit_Code;

   -----------------------
   -- Print_Stack_Union --
   -----------------------

   procedure Print_Stack_Union
     (File        :        File_Type;
      Line        : in out Line_Number;
      Session     :        Session_Type;
      Make_Header :        Boolean)
   is
      use Ada.Strings.Unbounded;
      use type Types.Symbol_Index;
      use type Symbol_Access;
      use type Symbols.Symbol_Kind;

      type String_Access is access String;
      type Type_Array    is array (Long_Integer range <>) of String_Access;
      type Type_Array_Access is access all Type_Array;

      procedure Increment_Line is new Increment (Line);
      procedure Free is new Ada.Unchecked_Deallocation (Type_Array,
                                                        Type_Array_Access);
      procedure Free is new Ada.Unchecked_Deallocation (String,
                                                        String_Access);

      Types : Type_Array_Access; --  A hash table of datatypes
      Array_Size    : Long_Integer;   --  Size of the "types" array
      Max_DT_Length : Integer;   --  Maximum length of any ".datatype" field.
      Std_DT : String_Access;    --  Standardized name for a datatype
      J      : Integer;          --  Loop counters
      Hash   : Long_Integer;     --  For hashing the name of a type
   begin
      --  Allocate and initialize types[] and allocate stddt[]
      Array_Size := Long_Integer (Session.Num_Symbol * 2);
      Types      := new Type_Array'(0 .. Array_Size - 1 => null);

      Max_DT_Length := 0;

      if Session.Names.Var_Type /= Null_Unbounded_String then
         Max_DT_Length := Length (Session.Names.Var_Type);
      end if;

      for I in 0 .. Session.Num_Symbol - 1 loop
         declare
            Symbol : constant Symbol_Access := Symbols.Element_At (I);
         begin
            if Symbol.Data_Type /= Null_Unbounded_String then
               Max_DT_Length := Integer'Max (Max_DT_Length,
                                             Length (Symbol.Data_Type));
            end if;
         end;
      end loop;

      Std_DT := new String'(0 .. Max_DT_Length * 2 - 1 + 1 => ASCII.NUL);

      --  Build a hash table of datatypes. The ".dtnum" field of each symbol
      --  is filled in with the hash index plus 1.  A ".dtnum" value of 0 is
      --  used for terminal symbols.  If there is no %default_type defined
      --  then 0 is also used as the .dtnum value for nonterminals which do
      --  not specify a datatype using the %type directive.

      for I in 0 .. Session.Num_Symbol - 1 loop
         declare
            Symbol : constant Symbol_Access := Symbols.Element_At (I);
         begin
            if Symbol = Session.Error_Symbol then
               Symbol.DT_Num := Integer (Array_Size + 1);
               goto Continue;
            end if;
            if
              Symbol.Kind /= Symbols.Non_Terminal or
              (Symbol.Data_Type = Null_Unbounded_String and
                 Session.Names.Var_Type = Null_Unbounded_String)
            then
               Symbol.DT_Num := 0;
               goto Continue;
            end if;

            declare
               Item  : access Unbounded_String := Symbol.Data_Type'Access;
               Index : Natural := 1;
--               Cp : Char_Access := Symbol.Data_Type;
            begin
               if Item.all = Null_Unbounded_String then
                  Item := Session.Names.Var_Type'Access;
               end if;
               --  if Cp = 0 then
               --     Cp := Session.Names.Var_Type;
               --  end if;

               while Element (Item.all, Index) = ' ' loop
                  Index := Index + 1;
               end loop;
               --  while IS_SPACE (Cp.all) loop
               --     Cp := Cp + 1;
               --  end loop;

               J := 0;
--               while Cp.all loop
               while Index <= Length (Item.all) loop
                  Std_DT (J) := Element (Item.all, Index); --  Cp.all;
                  J  := J  + 1;
--                  Cp := Cp + 1;
                  Index := Index + 1;
               end loop;
            end;

            while J > 0 and Std_DT (J - 1) = ' ' loop
               J := J - 1;
            end loop;
            Std_DT (J) := ASCII.NUL;

            if
              Session.Names.Token_Type /= Null_Unbounded_String and then
              Std_DT.all = To_String (Session.Names.Token_Type)
            then
               Symbol.DT_Num := 0;
               goto Continue;
            end if;

            Hash := 0;
            J    := 0;
            while Std_DT (J) /= ASCII.NUL loop
               Hash := Hash * 53 + Character'Pos (Std_DT (J));
               J    := J + 1;
            end loop;
            Hash := (Hash mod 16#8000_0000#) mod Array_Size;

            while Types (Hash) /= null loop
               if Types (Hash) =  Std_DT then
                  Symbol.DT_Num := Integer (Hash + 1);
                  exit;
               end if;
               Hash := Hash + 1;
               if Hash >= Array_Size then
                  Hash := 0;
               end if;
            end loop;

            if Types (Hash) = null then
               Symbol.DT_Num := Integer (Hash + 1);

               Types (Hash) := new String'(0 .. Std_DT'Length + 1 - 1
                                             => ASCII.NUL);
               Types (Hash) := Std_DT;
            end if;
         end;
         <<Continue>>
      end loop;

      --  Print out the definition of YYTOKENTYPE and YYMINORTYPE
      --  declare
      --     Name : constant String :=
      --       (if Session.File_Name /= Null_Unbounded_String
      --          then To_String (Session.File_Name)
      --          else "Parse");

      --     Token_Type : constant String :=
      --       (if Session.Names.Token_Type /= Null_Unbounded_String
      --          then To_String (Session.Names.Token_Type)
      --          else "void*");
      --  begin
      --     Write_Interface_begin (File, Name, Token_Type, Line);
      --  end;

      declare
         Name : constant String :=
           (if Session.File_Name /= Null_Unbounded_String
              then To_String (Session.File_Name) else "Parse");
      begin
         Write_Interface_Begin (File, Line, Make_Header);

         Put (File, "#define ");
         Put (File, Name);
         Put (File, "TOKENTYPE ");
         Put_Line (File, (if Session.Names.Token_Type /= Null_Unbounded_String
                            then To_String (Session.Names.Token_Type)
                            else "void*"));
         Increment_Line;

         Write_Interface_End (File, Line, Make_Header);

         Put_Line (File, "typedef union {");  Increment_Line;
         Put_Line (File, "  int yyinit;");    Increment_Line;
         Put (File, "  ");
         Put (File, Name);
         Put_Line (File, "TOKENTYPE yy0;");   Increment_Line;
      end;

      for I in 0 .. Array_Size - 1 loop
         if Types (I) /= null then
            Put (File, "  ");
            Put (File, Types (I).all);
            Put (File, " yy");
            Put (File, Long_Integer'Image (I + 1));
            Put_Line (File, ";");    Increment_Line;
            Free (Types (I));
         end if;
      end loop;

      if
        Session.Error_Symbol /= null and then
        Session.Error_Symbol.Use_Count /= 0
      then
         Put (File, "  int yy");
         Put (File, Integer'Image (Session.Error_Symbol.DT_Num));
         Put_Line (File, ";");  Increment_Line;
      end if;

      Free (Std_DT);
      Free (Types);
      Put_Line (File, "} YYMINORTYPE;");  Increment_Line;
   end Print_Stack_Union;

   ----------------
   -- Rule_Print --
   ----------------
   --  lemon.c:3101 - OVERLOADED

   procedure Rule_Print (File : File_Type;
                         Rule : Rule_Access)
   is
      use Ada.Strings.Unbounded;
      use Symbols;
      use type Rules.Dot_Type;
   begin
      --  Print LHS
      Put (File, To_String (Rule.LHS.Name));
      Put (File, " ::=");

      --  This part is uncommented in lemon.c
      --  Print LHS alias
      --  if False then
      --     if Rule.LHS_Alias /= Null_Unbounded_String then
      --        Put (File, "(");
      --        Put (File, To_String (Rule.LHS_Alias));
      --        Put (File, ")");
      --     end if;
      --  end if;

      --  Print RHS
      for I in Rule.RHS.First_Index .. Rule.RHS.Last_Index loop

         exit when I = Rule.RHS.Last_Index;

         declare
            Symbol : constant Symbols.Symbol_Access :=
              Symbol_Access (Rule.RHS.Element (I));

            First  : Boolean := True;
         begin
            if Symbol.Kind = Symbols.Multi_Terminal then
               Put (File, " ");
               for Sub_Symbol of Symbol.Sub_Symbol loop
                  if not First then
                     Put (File, "|");
                  end if;
                  Put (File, To_String (Sub_Symbol.Name));
                  First := False;
               end loop;
            else
               Put (File, " ");
               Put (File, To_String (Symbol.Name));
            end if;

            --  This part is uncommented i lemon.c
            --  if Rule.RHS_Alias.Element (I) /= Null_Unbounded_String then
            --     Put (File, "(");
            --     Put (File, To_String (Rule.RHS_Alias.Element (I)));
            --     Put (File, ")");
            --  end if;
         end;
      end loop;
   end Rule_Print;

   ---------------------
   -- Generate_Tokens --
   ---------------------

   procedure Generate_Tokens
     (Session      : in Session_Type;
      Token_Prefix : in String;
      First        : in Integer;
      Last         : in Integer)
   is
      File : File_Type renames Ada.Text_IO.Standard_Output;
      Line : Types.Line_Number := 0;

      pragma Unreferenced (Session);

      function Get_Prefix return String;
      procedure Increment_Line is new Increment (Line);

      function Get_Prefix return String is
      begin
         if Token_Prefix /= "" then
            return Token_Prefix;
         else
            return "";
         end if;
      end Get_Prefix;

      use Symbols;
      use Types;

      Prefix : constant String := Get_Prefix;
   begin
      if Options.Make_Headers then
         --  const char *prefix; */
         Put_Line (File, "#if INTERFACE");
         Increment_Line;

         for I in First .. Last loop
--              Put_Line (Context.File_Implementation,
--                        "#define " &
--                          Value (Prefix)   &
--                          Value (Get_Token_Callback (I)) &
--                          " " & Integer'Image (I));
--              Line_Number := Line_Number + 1;
            Put (File, "#define ");
            Put (File, Prefix);
            --  Put_CP (Get_Token_Callback (I));
            --  return lime_lemp_copy->symbols[index]->name;
            Put (File, Name_Of (Element_At (Symbol_Index (I))));
            Put (File, " ");
            Put (File, Integer'Image (I));
            Put_Line (File, "");
            Increment_Line;
         end loop;
         Put_Line (File, "#endif");
         Increment_Line;
      end if;
   end Generate_Tokens;

   ----------------------
   -- Render_Constants --
   ----------------------

   procedure Render_Constants
     (File   :        File_Type;
      Render :        Render_Record;
      Line   : in out Line_Number)
   is
      procedure Put (Item  : in String; Value : in Integer);
      procedure Put (Item  : in String; Value : in Action_Value);

      procedure Put (Item  : in String;
                     Value : in Integer) is
      begin
         Put (File, Item);
         Put (File, Integer'Image (Value));
         Put_Line (File, "");
         Line := Line + 1;
      end Put;

      procedure Put (Item  : in String;
                     Value : in Action_Value) is
      begin
         Put (Item, Integer (Value));
      end Put;

      I : Integer;
   begin
      Put ("#define YYNSTATE             ", Render.Nx_State);
      Put ("#define YYNRULE              ", Render.N_Rule);
      Put ("#define YYNRULE_WITH_ACTION  ", Render.Num_Rule_With_Action);
      Put ("#define YYNTOKEN             ", Render.N_Terminal);
      Put ("#define YY_MAX_SHIFT         ", Render.Nx_State - 1);
      I := Integer (Render.Min_Shift_Reduce);
      Put ("#define YY_MIN_SHIFTREDUCE   ", I);
      I := I + Render.N_Rule;
      Put ("#define YY_MAX_SHIFTREDUCE   ", I - 1);
      Put ("#define YY_ERROR_ACTION      ", Render.Err_Action);
      Put ("#define YY_ACCEPT_ACTION     ", Render.Acc_Action);
      Put ("#define YY_NO_ACTION         ", Render.No_Action);
      Put ("#define YY_MIN_REDUCE        ", Render.Min_Reduce);
      I := Integer (Render.Min_Reduce) + Render.N_Rule;
      Put ("#define YY_MAX_REDUCE        ", I - 1);
   end Render_Constants;

   function Image is new Auxiliary.Trim_Image (Integer);
   function Image is new Auxiliary.Trim_Image (Offset_Type);
   function Image is new Auxiliary.Trim_Image (Action_Value);

   -------------------------
   -- Output_Action_Table --
   -------------------------
   --  lemon.c:4377

   procedure Output_Action_Table
     (File      :        File_Type;
      Table     :        Action_Table;
      N         :        Integer;
      No_Action :        Action_Value;
      Line      : in out Line_Number)
   is
      use type Action_Tables.Action_Value;

      procedure Increment_Line is new Increment (Line);

      J      : Integer;
      Action : Action_Tables.Action_Value;
   begin
      Put_Line (File, "#define YY_ACTTAB_COUNT (" & Image (N) & ")"); Increment_Line;
      Put_Line (File, "static const YYACTIONTYPE yy_action[] = {");   Increment_Line;
      J := 0;
      for I in 0 .. N - 1 loop
         --  #define acttab_yyaction(X,N)  ((X)->aAction[N].action)
         --  return acttab_yyaction (lime_pActtab, i);
         --  struct acttab *lime_pActtab;
         --  Action := Get_Acttab_YY_Action (I);
         Action := Table.Action (I).Action;
         if Action < 0 then
            Action := No_Action;
         end if;
         if J = 0 then
            Put (File, " /* ");
            Put (File, Image (I));
            Put (File, " */ ");
         end if;
         Put (File, " ");
         Put (File, Image (Integer (Action)));
         Put (File, ",");
         if J = 9 or I = N - 1 then
            Put_Line (File, "");  Increment_Line;
            J := 0;
         else
            J := J + 1;
         end if;
      end loop;
      Put_Line (File, "};");  Increment_Line;

   end Output_Action_Table;

   -------------------------
   -- Output_YY_lookahead --
   -------------------------

   --  procedure Output_YY_Lookahead
   --    (File    :        File_Type;
   --     Table   :        Action_Table;
   --     N       :        Integer;
   --     Nsymbol :        Integer;
   --     Line    : in out Line_Number)
   --  is
   --     use type Action_Tables.Action_Value;

   --     procedure Increment_Line is new Increment (Line);

   --     LA : Action_Value;
   --     J  : Integer := 0;
   --  begin
   --     Put_Line (File, "static const YYCODETYPE yy_lookahead[] = {");  Increment_Line;
   --     for I in 0 .. N - 1 loop
   --        --  LA := Get_Acttab_YY_Lookahead (I);
   --        LA := Table.Lookahead (I).Action;
   --        if LA < 0 then
   --           LA := Action_Value (Nsymbol);
   --        end if;
   --        if J = 0 then
   --           Put (File, " /* ");
   --           Put (File, Image (I));
   --           Put (File, " */ ");
   --        end if;
   --        Put (File, " ");
   --        Put (File, Image (Integer (LA)));
   --        Put (File, ",");
   --        if J = 9 or I = N - 1 then
   --           Put_Line (File);  Increment_Line;
   --           J := 0;
   --        else
   --           J := J + 1;
   --        end if;
   --     end loop;
   --     Put_Line (File, "};");  Increment_Line;
   --  end Output_YY_Lookahead;

   -----------------------------
   -- Output_YY_Shift_Offsets --
   -----------------------------
   --  lemon.c:4414

   procedure Output_YY_Shift_Offsets
     (File          :        File_Type;
      Session       :        Session_Type;
      N             :        Integer;
      MnTknOfst     :        Offset_Type;
      MxTknOfst     :        Offset_Type;
      Min_Size_Type :        String;
      Nactiontab    :        Integer;
      No_Offset     :        Offset_Type;
      Line          : in out Line_Number)
   is
      use type Offset_Type;
      procedure Increment_Line is new Increment (Line);

      Offset : Offset_Type;
      J      : Integer := 0;
   begin
      Put (File, "#define YY_SHIFT_COUNT    (");
      Put (File, Image (N - 1));
      Put_Line (File, ")");      Increment_Line;

      Put (File, "#define YY_SHIFT_MIN      (");
      Put (File, Image (MnTknOfst));
      Put_Line (File, ")");      Increment_Line;

      Put (File, "#define YY_SHIFT_MAX      (");
      Put (File, Image (MxTknOfst));
      Put_Line (File, ")");      Increment_Line;

      Put (File, "static const ");
      Put (File, Min_Size_Type);
      Put (File, " yy_shift_ofst[] = {");
      Put_Line (File);  Increment_Line;

--  lemp->tablesize += n*sz;
      for I in 0 .. N - 1 loop
         declare
            use Sessions;

            State : access States.State_Record;
         begin
            State  := Session.Sorted (State_Index (I));
            Offset := State.Token_Offset;
         end;
         if Offset = No_Offset then
            Offset := Offset_Type (Nactiontab);
         end if;
         if J = 0 then
            Put (File, " /* ");
            Put (File, Image (I));
            Put (File, " */ ");
         end if;
         Put (File, " ");
         Put (File, Image (Offset));
         Put (File, ",");
         if J = 9 or I = N - 1 then
            Put_Line (File);  Increment_Line;
            J := 0;
         else
            J := J + 1;
         end if;
      end loop;
      Put_Line (File, "};");  Increment_Line;
   end Output_YY_Shift_Offsets;

   ------------------------------
   -- Output_YY_Reduce_Offsets --
   ------------------------------
   --  lemon.c:4440

   procedure Output_YY_Reduce_Offsets
     (File          :        File_Type;
      Session       :        Session_Type;
      N             :        Integer;
      MnNtOfst      :        Offset_Type;
      MxNtOfst      :        Offset_Type;
      Min_Size_Type :        String;
      No_Offset     :        Offset_Type;
      Line          : in out Line_Number)
   is
      use type Offset_Type;
      procedure Increment_Line is new Increment (Line);

      J      : Integer := 0;
      Offset : Offset_Type;
   begin
      Put (File, "#define YY_REDUCE_COUNT (");
      Put (File, Image (N - 1));
      Put_Line (File, ")");     Increment_Line;

      Put (File, "#define YY_REDUCE_MIN   (");
      Put (File, Image (MnNtOfst));
      Put_Line (File, ")");     Increment_Line;

      Put (File, "#define YY_REDUCE_MAX   (");
      Put (File, Image (MxNtOfst));
      Put_Line (File, ")");     Increment_Line;

      Put (File, "static const ");
      Put (File, Min_Size_Type);
      Put (File, " yy_reduce_ofst[] = {");
      Put_Line (File);          Increment_Line;

--  lemp->tablesize += n*sz;
      for I in 0 .. N - 1 loop
         declare
            State : access States.State_Record;
         begin
            State  := Session.Sorted (Sessions.State_Index (I));
            Offset := State.Nonterm_Offset;
         end;
         if Offset = No_Offset then
            Offset := Offset_Type (MnNtOfst - 1);
         end if;
         if J = 0 then
            Put (File, " /* ");
            Put (File, Image (I));
            Put (File, " */ ");
         end if;
         Put (File, " ");
         Put (File, Image (Offset));
         Put (File, ",");
         if J = 9 or I = N - 1 then
            Put_Line (File);  Increment_Line;
            J := 0;
         else
            J := J + 1;
         end if;
      end loop;
         Put_Line (File, "};");  Increment_Line;
   end Output_YY_Reduce_Offsets;

   ---------------------------------
   -- Output_Default_Action_Table --
   ---------------------------------
   --  lemon.c:4465

   procedure Output_Default_Action_Table
     (File         :        File_Type;
      Session      :        Session_Type;
      N            :        Integer;
      Error_Action :        Action_Value;
      Min_Reduce   :        Action_Value;
      Line         : in out Line_Number)
   is
      Num_Column : constant       := 8;
      type Column_Range is mod Num_Column;
      Column     : Column_Range := Column_Range'First;

      procedure Increment_Line is new Increment (Line);
   begin
      Put_Line (File, "static const YYACTIONTYPE yy_default[] = {");
      Increment_Line;
      for I in 0 .. N - 1 loop
         declare
            use States;
            State : constant access States.State_Record :=
              Session.Sorted (Sessions.State_Index (I));
         begin
            if Column = Column_Range'First then
               Put (File, " /* ");
               Put (File, Image (I));
               Put (File, " */ ");
            end if;

            Put (File, " ");
            if State.Default_Reduce = True then
               Put (File, Image (Error_Action));
            else
               declare
                  Auto_State : constant Integer :=
                    (case State.Default_Reduce is
                       when Syntax_Error => -1,
                       when False        =>  0,
                       when True         =>  1);
                  Reduce       : constant Integer := Integer (Min_Reduce);
                  State_Reduce : constant Integer := Auto_State + Reduce;
               begin
                  Put (File, Image (State_Reduce));
               end;
            end if;
            Put (File, ",");

         end;
         if Column = Column_Range'Last or I = N - 1 then
            Put_Line (File);
            Increment_Line;
         end if;
         Column := Column + 1;
      end loop;
      Put_Line (File, "};"); Increment_Line;
   end Output_Default_Action_Table;


--     procedure Template_Print_2
--       (Line        : in String;
--        No_Line_Nos : in Integer;
--  --      Line_Number : in Line_Number_Index;
--        Out_Name    : in String)
--     is
--  --      pragma Unreferenced (Out_Name);
--     begin
--        if Line = "" then
--           Ada.Text_IO.Put_Line ("RETURN");
--           return;
--        end if;
--        Text_Out.Put_Line (Line);

--        --  XXX mystisk kode
--  --      if( str[-1]!='\n' ){
--  --        putc('\n',out);
--  --        (*lineno)++;
--  --        }
--        Ada.Text_IO.Put ("WLD - ");
--        if No_Line_Nos /= 1 then
--           Ada.Text_IO.Put_Line ("2");
--           --  (*lineno)++; tplt_linedir(out,*lineno,lemp->outname);
--           --  Write_Line_Directive (Line_Number, Out_Name);
--           --  Write_Line_Directive (0, Out_Name);
--           Text_Out.Put_Line_Directive (Out_Name);
--        end if;

--     end Template_Print_2;

   -----------------------
   -- Write_Arg_Defines --
   -----------------------

   procedure Write_Arg_Defines
     (File    :        File_Type;
      Name    :        String;
      Arg_Ctx :        String;
      Extend  :        Boolean;
      Arg     :        String;
      Arg_I   :        String;
      Line    : in out Line_Number)
   is
      procedure Increment_Line is new Increment (Line);

      procedure Write (Decl : in String);
      procedure Write (Decl : in String) is
      begin
         Put (File, "#define ");
         Put (File, Name);
         Put (File, Arg_Ctx);
         Put (File, Decl);
         Put (File, Arg);
         Put (File, ";");
         Put_Line (File);
         Increment_Line;
      end Write;

   begin
      Write ("_SDECL ");
      Write ("_PDECL ,");
      Write ("_PARAM ,");
      if Extend = False then
         Put (File, "#define ");
         Put (File, Name);
         Put (File, "_FETCH ");
         Put (File, Arg);
         Put (File, "=yypParser->");
         Put (File, Arg_I);
         Put_Line (File, ";");
         Increment_Line;

         Put (File, "#define ");
         Put (File, Name);
         Put (File, "_STORE ");
         Put (File, Arg_I);
         Put (File, "=yypParser->");
         Put (File, Arg_I);
         Put_Line (File, ";");
         Increment_Line;
      else
         Write ("_FETCH ");
         Write ("_STORE ");
      end if;
   end Write_Arg_Defines;

   ---------------------
   -- Write_Interface --
   ---------------------

   procedure Write_Interface
     (File      :        File_Type;
      Name      :        String;
      Tokentype :        String;
      Line      : in out Line_Number)
   is
   begin
      if Options.Make_Headers then
         Put_Line (File, "#if INTERFACE");
         Line := Line + 1;
      end if;

      Put (File, "#define ");
      Put (File, Name);
      Put (File, "TOKENTYPE ");
      Put (File, Tokentype);
      Put_Line (File);
      Line := Line + 1;

      if Options.Make_Headers then
         Put_Line (File, "#endif");
         Line := Line + 1;
      end if;
   end Write_Interface;

   ---------------------------
   -- Write_Interface_Begin --
   ---------------------------

   procedure Write_Interface_Begin
     (File        :        File_Type;
      Line        : in out Line_Number;
      Make_Header :        Boolean) is
   begin
      if Make_Header then
         Put_Line (File, "#if INTERFACE");
         Line := Line + 1;
      end if;
   end Write_Interface_Begin;

   -------------------------
   -- Write_Interface_End --
   -------------------------

   procedure Write_Interface_End
     (File        :        File_Type;
      Line        : in out Line_Number;
      Make_Header :        Boolean) is
   begin
      if Make_Header then
         Put_Line (File, "#endif");
         Line := Line + 1;
      end if;
   end Write_Interface_End;

   -------------------
   -- Report_Header --
   -------------------

   procedure Report_Header
     (Session       : Session_Type;
      Token_Prefix  : String;
      Base_Name     : String;
      Module_Name   : String;
      Terminal_Last : Natural)
   is
      Prefix : constant String := Token_Prefix;
   begin

      if not Options.Make_Headers then
         return;
      end if;

--      if Token_Prefix = Null_Ptr then
--         Prefix := New_String ("");
--      end if;

      --  Generate parse.h.ads
      Generate_Spec (Session,
                     Base_Name, Prefix, Module_Name,
                     First => 1,
                     Last  => Terminal_Last);
   end Report_Header;

   -------------------
   -- Generate_Spec --
   -------------------

   procedure Generate_Spec
     (Session   : Session_Type;
      Base_Name : String;
      Prefix    : String;
      Module    : String;
      First     : Integer;
      Last      : Integer)
   is
      use Backend;
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
              (Session   => Session,
               Context   => Context,
               File_Name => Base_Name,
               Module    => Module,
               Prefix    => Prefix,
               First     => First,
               Last      => Last);

      end case;
   end Generate_Spec;

   -------------------
   -- Write_Include --
   -------------------

   procedure Write_Include
     (File         :        File_Type;
      Line         : in out Line_Number;
      Include_Name :        String)
   is
   begin
      if Options.Make_Headers then
         Put (File, "#include <");
         Put (File, Include_Name);
         Put_Line (File, ">;"); Line := Line + 1;
      end if;
   end Write_Include;

   ----------------------------
   -- Generate_The_Defines_1 --
   ----------------------------

   procedure Generate_The_Defines_1
     (File           :        File_Type;
      Line           : in out Line_Number;
      YY_Code_Type   :        String;
      Symbol_Count   :        Symbol_Index;
      YY_Action_Type :        String;
      Is_Wildcard    :        Boolean;
      Wildcard_Index :        Symbol_Index)
   is
      procedure Increment_Line is new Increment (Line);
   begin
      Put (File, "#define YYCODETYPE ");
      Put (File, YY_Code_Type);
      Put_Line (File);  Increment_Line;

      Put (File, "#define YYNOCODE ");
      Put (File, Symbol_Index'Image (Symbol_Count));
      Put_Line (File);  Increment_Line;

      Put (File, "#define YYACTIONTYPE ");
      Put (File, YY_Action_Type);
      Put_Line (File);  Increment_Line;

      if Is_Wildcard then
         Put (File, "#define YYWILDCARD ");
         Put (File, Symbol_Index'Image (Wildcard_Index));
         Put_Line (File);  Increment_Line;
      end if;
   end Generate_The_Defines_1;

   ----------------------------
   -- Generate_The_Defines_2 --
   ----------------------------

   procedure Generate_The_Defines_2
     (File       :        File_Type;
      Line       : in out Line_Number;
      Stack_Size :        String)
   is
      procedure Increment_Line is new Increment (Line);
   begin
      Put_Line (File, "#ifndef YYSTACKDEPTH");  Increment_Line;
      if Stack_Size /= "" then
         Put (File, "#define YYSTACKDEPTH ");
         Put (File, Stack_Size);
         Put_Line (File);  Increment_Line;
      else
         Put_Line (File, "#define YYSTACKDEPTH 100");  Increment_Line;
      end if;
      Put_Line (File, "#endif");  Increment_Line;
   end Generate_The_Defines_2;

   --------------------
   -- Error_Fallback --
   --------------------

   procedure Error_Fallback
     (File         :        File_Type;
      Line         : in out Line_Number;
      Error_Sym    :        String;
      Struct       :        Mystruct_Record;
      Has_Fallback :        Boolean)
   is
      procedure Increment_Line is new Increment (Line);
   begin

      if Error_Sym /= "" and Struct.Use_Count /= 0 then
         Put (File, "#define YYERRORSYMBOL ");
         Put (File, Symbol_Index'Image (Struct.Index));
         Put_Line (File);  Increment_Line;

         Put (File, "#define YYERRSYMDT yy");
         Put (File, Integer'Image (Struct.DT_Num));
         Put_Line (File);  Increment_Line;
      end if;

      if Has_Fallback then
         Put_Line (File, "#define YYFALLBACK 1"); Increment_Line;
      end if;

   end Error_Fallback;

end Reports;
