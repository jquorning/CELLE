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
with Ada.IO_Exceptions;

with DK8543.Auxiliary;
with DK8543.Interfaces.C.Strings;
with Rules;
with Symbols;
with Report_Parsers;
with Text_Out;
with Actions;
with Configs;
with States;
with Options;
with Extras;
with Generate_Ada;
with Generate_C;
with Backend;
with Setup;
with Sets;

package body Reports is

   procedure Reprint_C (Session : access Sessions.Session_Type) is
   begin
      Reprint (Session.all);
   end Reprint_C;

   procedure Report_Output_C (Session : access Sessions.Session_Type) is
   begin
      Reprint (Session.all);
   end Report_Output_C;

   procedure Report_Table_C (Session : access Sessions.Session_Type) is
   begin
      Report_Table
        (Session.all,
        User_Template_Name => Options.User_Template.all);
   end Report_Table_C;

   procedure Compress_Tables_C (Session : access Sessions.Session_Type) is
   begin
      Compress_Tables (Session.all);
   end Compress_Tables_C;

   procedure Resort_States_C (Session : access Sessions.Session_Type) is
   begin
      Resort_States (Session.all);
   end Resort_States_C;

   procedure Rule_Print (File : in Ada.Text_IO.File_Type;
                         Rule : in Rules.Rule_Access);
   --  Print the text of a rule.

   procedure Print_Action
     (Action : in     Actions.Action_Record;
      File   : in     Ada.Text_IO.File_Type;
      Indent : in     Integer;
      Result :    out Boolean);
   --  Print an action to the given file descriptor.  Return FALSE if
   --  nothing was actually printed.

   procedure Config_Print (File : in Ada.Text_IO.File_Type;
                           CFP  : in Configs.Config_Access);
   --  Print the rule for a configuration.

   function Minimum_Size_Type (LWS     : in     Integer;
                               UPR     : in     Integer;
                               PnBytes :    out Integer)
                              return String;


   --  Return the name of a C datatype able to represent values between
   --  lwr and upr, inclusive.  If pnByte!=NULL then also write the sizeof
   --  for that type (1, 2, or 4) into *pnByte.

   function File_Makename (Session   : in Sessions.Session_Type;
                           Extension : in String) return String;


   procedure Write_Rule_Text (Rule : in Rules.Rule_Access);
   --  Write text on "out" that describes the rule "rp".

   procedure Emit_Destructor_Code
     (Symbol  : in Symbols.Symbol_Access;
      Session : in Sessions.Session_Type);
   --  The following routine emits code for the destructor for the
   --  symbol sp

   procedure Emit_Code
     (Rule    : in Rules.Rule_Access;
      Session : in Sessions.Session_Type);
   --  Generate code which executes when the rule "rp" is reduced.  Write
   --  the code to "out".  Make sure lineno stays up-to-date.


   procedure Print_Stack_Union
     (Session : in Sessions.Session_Type);  --  The main info structure for this parser

   --  struct lemon *lemp //,
   --  //  int mhflag                  /* True if generating makeheaders output */

   --  Print the definition of the union used for the parser's data stack.
   --  This union contains fields for every possible data type for tokens
   --  and nonterminals.  In the process of computing and printing this
   --  union, also set the ".dtnum" field of every terminal and nonterminal
   --  symbol.


   procedure Generate_Tokens
     (Session        : in Sessions.Session_Type;
      Token_Prefix : in String;
      First        : in Integer;
      Last         : in Integer);

   type Render_Record is
      record
         Nx_State         : Integer;
         N_Rule           : Integer;
         N_Terminal       : Integer;
         Min_Shift_Reduce : Integer;
         Err_Action       : Integer;
         Acc_Action       : Integer;
         No_Action        : Integer;
         Min_Reduce       : Integer;
      end record;

   procedure Render_Constants (Render : in Render_Record);
   --
   --

   procedure Output_Action_Table
     (Action_Table : in Actions.A_Action_Table;
      N            : in Integer;
      No_Action    : in Integer);
   --
   --

   procedure Output_YY_Lookahead
     (Action_Table : in Actions.A_Action_Table;
      N            : in Integer;
      Nsymbol      : in Integer);
   --
   --

   procedure Output_YY_Shift_Offsets
     (Session       : in Sessions.Session_Type;
      N             : in Integer;
      MnTknOfst     : in Integer;
      MxTknOfst     : in Integer;
      Min_Size_Type : in String;
      Nactiontab    : in Integer;
      NO_OFFSET     : in Integer);

   --
   --
   --

   procedure Output_YY_Reduce_Offsets
     (Session       : in Sessions.Session_Type;
      N             : in Integer;
      MnNtOfst      : in Integer;
      MxNtOfst      : in Integer;
      Min_Size_Type : in String;
      NO_OFFSET     : in Integer);


   procedure Output_Default_Action_Table
     (Session      : in Sessions.Session_Type;
      N            : in Integer;
      Error_Action : in Integer;
      Min_Reduce   : in Integer);

--     procedure Template_Print_2
--       (Line        : in String;
--        No_Line_Nos : in Integer;
--        Out_Name    : in String);
   --  Print a string to the file and keep the linenumber up to date

   procedure Write_Arg_Defines
     (Name    : in String;
      Arg_Ctx : in String;
      Extend  : in Boolean;
      Arg     : in String;
      Arg_I   : in String);

   procedure Write_Interface
     (Name      : in String;
      Tokentype : in String);
   --

   procedure Write_Interface_Begin;
   procedure Write_Interface_End;

   --
   --  Each state contains a set of token transaction and a set of
   --  nonterminal transactions.  Each of these sets makes an instance
   --  of the following structure.  An array of these structures is used
   --  to order the creation of entries in the yy_action[] table.
   --
   type AX_Record is
     record
        STP      : access States.State_Record; --  A pointer to a state
        Is_Token : Boolean;
        --  True to use tokens.  False for non-terminals

        N_Action : Integer;                    --  Number of actions
        Order    : Integer;                    --  Original order of action sets
     end record;


   procedure Generate_Spec
     (Session   : in Sessions.Session_Type;
      Base_Name : in String;
      Prefix    : in String;    --  Prefix of symbols in spec
      Module    : in String;    --  Prefix of symbols in spec
      First     : in Integer;   --  Index of first symbol
      Last      : in Integer);  --  Index of last symbol
   --  Create spec file with name File_Name including symols found by
   --  iterating from First to Last calling callback prepended with
   --  Suffix.


   procedure Implementation_Open (File_Name : in String);
   --  Open a file for writing then implementaion (parse.adb/parse.c).
   --  File handler is located in the context structure.

   --  function Get_Token (Index : in Integer) return String;
   --  Get token for token symbol creation.

   procedure Open_If_Possible
     (File      : in out Ada.Text_IO.File_Type;
      File_Name : in     String;
      Success   :    out Boolean);
   --  Open File_Name to File. Success is True when success.

   procedure Template_Open
     (User_Template : in     String;
      Error_Count   : in out Integer;
      Success       :    out Integer);
   --  Thisk function finds the template file and opens it. File handle
   --  is located in the context structure.

   procedure Template_Transfer (Name : in String);
   --


   procedure Template_Print
     (Out_Name    : in String;
      No_Line_Nos : in Integer;
      Include     : in String);

   procedure Write_Include
     (Include_Name : in String);

   procedure Generate_The_Defines_1
     (YY_Code_Type   : in String;
      Symbol_Count   : in Symbols.Symbol_Index;
      YY_Action_Type : in String;
      Is_Wildcard    : in Boolean;
      Wildcard_Index : in Symbols.Symbol_Index);

   procedure Generate_The_Defines_2
     (Stack_Size : in String);

   type Mystruct_Record is record
      Use_Count : Integer;
      Index     : Integer;
      DT_Num    : Integer;
   end record;
   pragma Convention (C, Mystruct_Record);

   type Struct_Access is access all Mystruct_Record;
   pragma Convention (C, Struct_Access);

   procedure Error_Fallback
     (Error_Sym    : in String;
      Struct       : in Struct_Access;
      Has_Fallback : in Integer);
   --
   --

   procedure Close_Out;
   --  Close out file

   procedure Close_In;
   --  Close in file


   type AX_Set_Record is
      record
         Token        : AX_Record;
         Non_Terminal : AX_Record;
      end record;

   type AX_Set_Array is array (Symbols.Symbol_Index range <>) of AX_Set_Record;
   type A_AX_Set_Array is access all AX_Set_Array;


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

   procedure Reprint (Session : in Sessions.Session_Type)
   is
      use Ada.Text_IO;
      use Ada.Strings.Unbounded;
      use Symbols;
      use type Rules.Rule_Access;
      use type Rules.Rule_Symbol_Access;

      package Symbol_Index_IO is
         new Ada.Text_IO.Integer_IO (Num => Symbol_Index);

      Rule     : Rules.Rule_Access;
      Symbol : Symbol_Access;
      J      : Symbol_Index;
      Max_Len, Len, N_Columns, Skip : Integer;
      Column : Natural;
   begin
      Put ("// Reprint of input file """);
      Put (To_String (Unbounded_String'(Session.File_Name)));
      Put_Line (""".");
      Put_Line ("// Symbols:");
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
      Skip := (Integer (Session.N_Symbol) + N_Columns - 1) / N_Columns;
      for I in 0 .. Skip - 1 loop
         Put ("//");
         J := Symbols.Symbol_Index (I);
         Column := 0;
         while J < Session.N_Symbol loop
            Symbol := Symbols.Element_At (J);
            pragma Assert (Symbol.Index = J);

            Put (" ");
            Symbol_Index_IO.Put (J, Width => 3);
            Put (" ");
            declare
               Name  : constant String := Symbols.Name_Of (Symbol);
               Field : String (1 .. Max_Len) := (others => ' ');
            begin
               Field (Name'Range) := Name;
               Put (Field);
            end;
            J := J + Symbol_Index (Skip);
            Column := Column + 1;
         end loop;
         New_Line;
      end loop;

      --  Print rules
      Rule := Session.Rule;
      while Rule /= null loop
         Rule_Print (Standard_Output, Rule);
         Put (".");
         if Rule.Prec_Symbol /= null then
            Put (" [" & Name_Of (Symbol_Access (Rule.Prec_Symbol)) & "]");
         end if;
         --  /* if( rp->code ) printf("\n    %s",rp->code); */
         New_Line;
         Rule := Rule.Next;
      end loop;
   end Reprint;


   procedure Report_Output (Session : in Sessions.Session_Type)
   is
      use Ada.Text_IO;
      use Symbols;
      use Rules;
      use Configs;
      use Actions;
      use Extras;

      File : File_Type;

      Action_Result : Boolean;

      N      : Integer;
      State  : States.State_Access;
      Config : Config_Access;
      Rule   : Rule_Access;
   begin
--  fp = file_open(lemp,".out","wb");
      Open (File, Out_File, "XXX.out");

      for I in 0 .. Session.Nx_State - 1 loop
         State := Session.Sorted (I);
         Put (File, "State ");
         Put (File, Integer'Image (State.State_Num));
         Put (File, ":");
         New_Line;

         if Session.Basis_Flag then
            Config := State.Basis;
         else
            Config := State.Config;
         end if;

         while Config /= null loop
            if Config.Dot = Config.Rule.RHS'Length then
               Put (File, "    (");
               Put (File, Config.Rule.Rule'Img);
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

      for I in 0 .. Extras.Symbol_Count - 1 loop
         declare
            use type Sets.Set_Type;

            Symbol : Symbol_Access;
         begin
            Symbol := Element_At (I);
            Put (File, "  " & I'Img & ": " & Name_Of (Symbol));
            if Symbol.Kind = Non_Terminal then
               Put (File, ":");
               if Symbol.Lambda then
                  Put (File, " <lambda>");
               end if;
               for J in 0 .. Session.N_Terminal - 1 loop
                  if
                    Symbol.First_Set /= Sets.Null_Set and then
                    Sets.Set_Find (Symbol.First_Set, Natural (J))
                  then
                     Put (File, " ");
                     Put (File, Name_Of (Element_At (J)));
                  end if;
               end loop;
            end if;
            if Symbol.Prec >= 0 then
               Put (File, " (precedence=");
               Put (File, Symbol.Prec'Img);
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
      for I in 0 .. Extras.Symbol_Count loop
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

      Rule := Session.Rule;
      loop
         exit when Rule = null;
         Put (File, Rule.Rule'Img); -- XXX "%4d: ", rp->iRule);
         Put (File, ": ");
         Rule_Print (File, Rule);
         Put (File, ".");
         if Rule.Prec_Symbol /= null then
            Put (File, " [");
            Put (File, Name_Of (Symbol_Access (Rule.Prec_Symbol)));
            Put (File, " precedence=");
            Put (File, Rule.Prec_Symbol.Prec'Img);
            Put (File, "]");
         end if;
         New_Line (File);
         Rule := Rule.Next;
      end loop;

      Close (File);
   end Report_Output;


   procedure Report_Table
     (Session            : in out Sessions.Session_Type;
      User_Template_Name : in     String)
   is
      use Ada.Strings.Unbounded;
      use Sessions;
      use Rules;
      package Acttab renames Actions;

      Session_Name : constant String := To_String (Session.Names.Name);
--    char line[LINESIZE];
      State : States.State_Access;
--    struct action *ap;
      Rule  : Rules.Rule_Access;

      Act_Tab : Actions.A_Action_Table;
--    int i, j, n, sz;
      I  : Integer;
      N  : Integer;
      SZ : Integer;
      Size_Of_Action_Type : Integer;
      Size_Of_Code_Type   : Integer;
--    const char *name;
      Mn_Tkn_Ofst, Mx_Tkn_Ofst : Integer;
      Mn_Nt_Ofst,  Mx_Nt_Ofst  : Integer;

      AX : A_AX_Set_Array;

      Template_Open_Success : Integer;
      Error_Count           : Natural := 0;
   begin
      Session.Min_Shift_Reduce := Natural (Session.N_State);
      Session.Err_Action       := Session.Min_Shift_Reduce + Session.N_Rule;
      Session.Acc_Action       := Session.Err_Action + 1;
      Session.No_Action        := Session.Acc_Action + 1;
      Session.Min_Reduce       := Session.No_Action + 1;
      Session.Max_Action       := Session.Min_Reduce + Session.N_Rule;

      Template_Open (User_Template_Name, Error_Count, Template_Open_Success);
      Implementation_Open (File_Makename (Session, ".c"));

      Template_Transfer (Session_Name);

      --  Generate the include code, if any
      --  Sessions_Print (Session.Outname, Session.No_Linenos_Flag, Session.Include);
      Template_Print (Ada.Strings.Unbounded.To_String (Session.Out_Name),
                      Boolean'Pos (Session.No_Linenos_Flag),
                      To_String (Session.Names.Include));
      --  lime_print (lime_get_ouüt_name (), lemp->nolinenosflag, lemap->include);
      --  lime_write_include (lime_get_mh_flag(), file_makename(lemp, ".h"));
      Write_Include (File_Makename (Session, ".h"));

      Template_Transfer (Session_Name);

      --  Generate #defines for all tokens
--  XXX    Sessions_Session_Copy := Session;
      --  lime_generate_tokens (lime_get_mh_flag(), lemp->tokenprefix, 1, lemp->nterminal);
      Generate_Tokens (Session, To_String (Session.Names.Token_Prefix),
                       1, Integer (Session.N_Terminal));

      Template_Transfer (Session_Name);

      --  Generate the defines
      declare
         use Symbols;
         use Extras;

         Code     : constant String := Minimum_Size_Type (0,
                                                          Integer (Extras.Symbol_Count),
                                                          Size_Of_Code_Type);
         Action   : constant String := Minimum_Size_Type (0, Session.Max_Action,
                                                          Size_Of_Action_Type);
         Wildcard    : constant Symbol_Access := Get_Wildcard (Session.Extra);
         Is_Wildcard : constant Boolean       := (Wildcard /= null);
      begin
         if Is_Wildcard then
            Generate_The_Defines_1
              (Code,
               Extras.Symbol_Count,
               Action,
               Is_Wildcard    => True,
               Wildcard_Index => Wildcard.Index);
         else
            Generate_The_Defines_1
            (Code,
             Extras.Symbol_Count,
             Action,
             Is_Wildcard    => False,
             Wildcard_Index => 0);
         end if;
      end;

      --  print_stack_union (lemp, lime_get_mh_flag());
      Print_Stack_Union (Session);
      Generate_The_Defines_2 (To_String (Session.Names.Stack_Size));

--    //if( lime_get_mh_flag() ){
--    //  lime_put_line ("#if INTERFACE");
--    //}
      Write_Interface_Begin;

      declare

         function Get_Name return String;

         function Get_Name return String is
         begin
            if Session.Names.Name /= "" then
               return To_String (Session.Names.Name);
            else
               return "Parse";
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
         if ARG /= "" then
            Write_Arg_Defines (Name, "ARG", True, ARG, ARG (ARG_I .. ARG'Last));
         else
            Write_Arg_Defines (Name, "ARG", False, "", "");
         end if;

         Trim_Right_Symbol (CTX, CTX_I);
         if CTX /= "" then
            Write_Arg_Defines (Name, "CTX", True, CTX, CTX (CTX_I .. CTX'Last));
         else
            Write_Arg_Defines (Name, "CTX", False, "", "");
         end if;
      end;

      Write_Interface_Begin;
--    //  if( lime_get_mh_flag() ){
--    //    lime_put_line ("#endif");
--    //  }
--
--    struct mystruct lime_mystruct;
--    if (lemp->errsym) {
--      lime_mystruct.use_count = lemp->errsym->useCnt;
--      lime_mystruct.index     = lemp->errsym->index;
--      lime_mystruct.dt_num    = lemp->errsym->dtnum;
--    }
--
--    lime_error_fallback
--      ((const char*)lemp->errsym,
--       &lime_mystruct,
--       lemp->has_fallback);

      --  Compute the action table, but do not output it yet.  The action
      --  table must be computed before generating the YYNSTATE macro because
      --  we need to know how many states can be eliminated.

      --      AX := (struct axset *) calloc(lemp->nxstate*2, sizeof(ax[0]));
      --  AX := new AX_Set_Record;  --  (struct axset *) calloc(lemp->nxstate*2, sizeof(ax[0]));
      declare
         use Symbols;
         use Extras;
      begin
         AX := new AX_Set_Array (0 .. Symbol_Index (Session.Nx_State) - 1);

      --  if( ax==0 ){
      --    fprintf(stderr,"malloc failed\n");
      --    exit(1);

         for I in 0 .. Session.Nx_State - 1 loop
            State := Session.Sorted (I);

            AX (I).Token := (STP      => State,
                             Is_Token => True,
                             N_Action => State.N_Tkn_Act,
                             Order    => <>);

            AX (I).Non_Terminal := (STP      => State,
                                    Is_Token => False,
                                    N_Action => State.N_Nt_Act,
                                    Order    => <>);
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
      Act_Tab := Acttab.Alloc (Integer (Extras.Symbol_Count), Integer (Session.N_Terminal));
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
        (Render =>
           (Nx_State         => Natural (Session.Nx_State),
            N_Rule           => Session.N_Rule,
            N_Terminal       => Integer (Session.N_Terminal),
            Min_Shift_Reduce => Session.Min_Shift_Reduce,
            Err_Action       => Session.Err_Action,
            Acc_Action       => Session.Acc_Action,
            No_Action        => Session.No_Action,
            Min_Reduce       => Session.Min_Reduce));

      Template_Transfer (Session_Name);


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

      declare
      begin

         --
         --  Output the yy_action table
         --
         Session.N_Action_Tab := Acttab.Action_Size (Act_Tab.all);
         N := Session.N_Action_Tab;
         Session.Table_Size := Session.Table_Size + N * Size_Of_Action_Type;

         Output_Action_Table (Act_Tab, N, Session.No_Action);

         --
         --  Output the yy_lookahead table
         --
         Session.N_Lookahead_Tab := Acttab.Lookahead_Size (Act_Tab.all);
         N := Session.N_Lookahead_Tab;
         Session.Table_Size := Session.Table_Size + N * Size_Of_Code_Type;
      end;

      Output_YY_Lookahead (Act_Tab, N, Integer (Extras.Symbol_Count));

      --
      --  Output the yy_shift_ofst[] table
      --

      N := Natural (Session.Nx_State);
--      while  N > 0 and Session.Sorted(N - 1).I_Tkn_Ofst = NO_Offset loop
--         N := N - 1;
--      end loop;
--
--    lime_lemp = lemp;
      Output_YY_Shift_Offsets
        (Session, N,
         Mn_Tkn_Ofst,
         Mx_Tkn_Ofst,
         Minimum_Size_Type (Mn_Tkn_Ofst, Integer (Session.N_Terminal) + Session.N_Action_Tab, SZ),
         Session.N_Action_Tab,
         No_Offset);

      Session.Table_Size := Session.Table_Size + N * SZ;

      --
      --  Output the yy_reduce_ofst[] table
      --
      N := Natural (Session.Nx_State);
--    while( n>0 && lemp->sorted[n-1]->iNtOfst==NO_OFFSET ) n--;
--
      Output_YY_Reduce_Offsets
        (Session, N,
         Mn_Nt_Ofst,
         Mx_Nt_Ofst,
         Minimum_Size_Type (Mn_Nt_Ofst - 1, Mx_Nt_Ofst, SZ),
         No_Offset);
      Session.Table_Size := Session.Table_Size + N * SZ;

      --
      --  Output the default action table
      --
      Output_Default_Action_Table
        (Session, Natural (Session.Nx_State),
         Session.Err_Action,
         Session.Min_Reduce);
      Session.Table_Size := Session.Table_Size + N * Size_Of_Action_Type;

      Template_Transfer (Session_Name);

      --
      --  Generate the table of fallback tokens.
      --
      if Session.Has_Fallback then
         declare
--            use Ada.Strings.Unbounded;
            use Symbols;
            use Extras;

            MX : Symbol_Index := Session.N_Terminal - 1;
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
                  use Text_Out;
                  P : constant Symbol_Access := Element_At (I);
               begin
                  if P.Fallback = null then
                     Put ("    0,  /* ");
                     Put (Name_Of (P));
                     Put_Line (" => nothing */");
                  else
                     Put ("  ");
                     Put_Int (Integer (P.Fallback.Index));
                     Put (",  /* ");
                     Put (Name_Of (P));
                     Put (" => ");
                     Put (To_String (P.Fallback.Name));
                     Put_Line (" */");
                  end if;
               end;
            end loop;
         end;
      end if;

      Template_Transfer (Session_Name);

      --
      --  Generate A Table Containing the symbolic name of every symbol
      --
      declare
         use Text_Out;
         use Symbols;
         use Extras;

         J : Integer;
         Rule : Rules.Rule_Access;
      begin
         for I in Symbol_Index range 0 .. Extras.Symbol_Count - 1 loop
            declare
               Name : constant String := Name_Of (Element_At (I));
            begin
               --  Session_Sprintf (Line, """" & Name & """,");
               Put ("  /* "); --  %4d */ \"%s\",\n",i, lemp->symbols[i]->name);
               --  lineno++;
               Put_Int (Integer (I));
               Put_Line (" */ """ & Name & """,");
               --  lineno++;
               Put (Name);  --
               Put_Line (""",");
               --  fprintf(out,"  /* %4d */ \"%s\",\n",i, lemp->symbols[i]->name);
               --  lineno++;
            end;
         end loop;

         Template_Transfer (Session_Name);

         --  Generate a table containing a text string that describes every
         --  rule in the rule set of the grammar.  This information is used
         --  when tracing REDUCE actions.
         J  := 0;
         Rule := Session.Rule;

         while Rule /= null loop
            pragma Assert (Rule.Rule = J);
            --  fprintf(out," /* %3d */ \"", i);
            Put (" /* ");
            Put_Int (J);
            Put (" */ """);
            Write_Rule_Text (Rule);
            --  fprintf(out,"\",\n"); lineno++;
            Put_Line (""",");
            Rule := Rule.Next;
         end loop;
      end;

      Template_Transfer (Session_Name);

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
         use Symbols, Extras;
      begin
         I := 0;
         loop
            exit when I >= Integer (Extras.Symbol_Count);
            exit when Element_At (Symbol_Index (I)).Kind = Terminal;
            I := I + 1;
         end loop;

         --  I : Symbols.Symbol_Index;
         --      if( i<lemp->nsymbol ){
         Emit_Destructor_Code (Element_At (Symbol_Index (I)), Session);
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

--    for(i=0, rp=lemp->rule; rp; rp=rp->next, i++){
--      lime_put ("  ");
--      lime_put_int (rp->lhs->index);
--      lime_put (", /* (");
--      lime_put_int (i);
--      lime_put (" ");
      Rule_Print (Ada.Text_IO.Standard_Output, Rule);
--      lime_put_line (" */");
--    }
--
      Template_Transfer (Session_Name);
--
--    for(i=0, rp=lemp->rule; rp; rp=rp->next, i++){
--      lime_put ("  ");
--      lime_put_int (-rp->nrhs);
--      lime_put (",  /* (");
--      lime_put_int (i);
--      lime_put (") ");
--      rule_print (rp);
--      lime_put_line (" */");
--    }
--
      Template_Transfer (Session_Name);

      --  Generate code which execution during each REDUCE action
      I  := 0;
      Rule := Session.Rule;
      while Rule /= null loop
         --  I  := I + Translate_Code (Session, Rule);
         Rule := Rule.Next;
      end loop;

      if I /= 0 then
         Text_Out.Put_Line ("        YYMINORTYPE yylhsminor;");
      end if;

      --  First output rules other than the default: rule
      Rule := Session.Rule;
      while Rule /= null loop
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
         Emit_Code (Rule, Session);
--      lime_put_line ("        break;");
--      rp->codeEmitted = 1;
         Rule := Rule.Next;
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

      Close_In;
      Close_Out;
--    printf ("### 2-58\n");
   end Report_Table;


   procedure Compress_Tables (Session : in Sessions.Session_Type)
   is
   begin
      null;
--  void
--  lemon_compress_tables
--  (struct lemon *lemp)
--  {
--    struct state *stp;
--    struct action *ap, *ap2, *nextap;
--    struct rule *rp, *rp2, *rbest;
--    int nbest, n;
--    int i;
--    int usesWildcard;
--
--    for(i=0; i<lemp->nstate; i++){
--      stp = lemp->sorted[i];
--      nbest = 0;
--      rbest = 0;
--      usesWildcard = 0;
--
--      for(ap=stp->ap; ap; ap=ap->next){
--        if( ap->type==SHIFT && ap->sp==lemp->wildcard ){
--          usesWildcard = 1;
--        }
--        if( ap->type!=REDUCE ) continue;
--        rp = ap->x.rp;
--        if( rp->lhsStart ) continue;
--        if( rp==rbest ) continue;
--        n = 1;
--        for(ap2=ap->next; ap2; ap2=ap2->next){
--          if( ap2->type!=REDUCE ) continue;
--          rp2 = ap2->x.rp;
--          if( rp2==rbest ) continue;
--          if( rp2==rp ) n++;
--        }
--        if( n>nbest ){
--          nbest = n;
--          rbest = rp;
--        }
--      }
--
--      /* Do not make a default if the number of rules to default
--      ** is not at least 1 or if the wildcard token is a possible
--      ** lookahead.
--      */
--      if( nbest<1 || usesWildcard ) continue;
--
--
--      /* Combine matching REDUCE actions into a single default */
--      for(ap=stp->ap; ap; ap=ap->next){
--        if( ap->type==REDUCE && ap->x.rp==rbest ) break;
--      }
--      assert( ap );
--      ap->sp = lime_symbol_new("{default}");
--      for(ap=ap->next; ap; ap=ap->next){
--        if( ap->type==REDUCE && ap->x.rp==rbest ) ap->type = NOT_USED;
--      }
--      stp->ap = Action_sort(stp->ap);
--
--      for(ap=stp->ap; ap; ap=ap->next){
--        if( ap->type==SHIFT ) break;
--        if( ap->type==REDUCE && ap->x.rp!=rbest ) break;
--      }
--      if( ap==0 ){
--        stp->autoReduce = 1;
--        stp->pDfltReduce = rbest;
--      }
--    }
--
--    /* Make a second pass over all states and actions.  Convert
--    ** every action that is a SHIFT to an autoReduce state into
--    ** a SHIFTREDUCE action.
--    */
--    for(i=0; i<lemp->nstate; i++){
--      stp = lemp->sorted[i];
--      for(ap=stp->ap; ap; ap=ap->next){
--        struct state *pNextState;
--        if( ap->type!=SHIFT ) continue;
--        pNextState = ap->x.stp;
--        if( pNextState->autoReduce && pNextState->pDfltReduce!=0 ){
--          ap->type = SHIFTREDUCE;
--          ap->x.rp = pNextState->pDfltReduce;
--        }
--      }
--    }
--
--    /* If a SHIFTREDUCE action specifies a rule that has a single RHS term
--    ** (meaning that the SHIFTREDUCE will land back in the state where it
--    ** started) and if there is no C-code associated with the reduce action,
--    ** then we can go ahead and convert the action to be the same as the
--    ** action for the RHS of the rule.
--    */
--    for(i=0; i<lemp->nstate; i++){
--      stp = lemp->sorted[i];
--      for(ap=stp->ap; ap; ap=nextap){
--        nextap = ap->next;
--        if( ap->type!=SHIFTREDUCE ) continue;
--        rp = ap->x.rp;
--        if( rp->noCode==0 ) continue;
--        if( rp->nrhs!=1 ) continue;
--  #if 1
--        /* Only apply this optimization to non-terminals.  It would be OK to
--        ** apply it to terminal symbols too, but that makes the parser tables
--        ** larger. */
--        if( ap->sp->index<lemp->nterminal ) continue;
--  #endif
--        /* If we reach this point, it means the optimization can be applied */
--        nextap = ap;
--        for(ap2=stp->ap; ap2 && (ap2==ap || ap2->sp!=rp->lhs); ap2=ap2->next){}
--        assert( ap2!=0 );
--        ap->spOpt = ap2->sp;
--        ap->type = ap2->type;
--        ap->x = ap2->x;
--      }
--    }
--  }
   end  Compress_Tables;


   procedure Resort_States (Session : in Sessions.Session_Type)
   is
   begin
      null;
--  void lemon_resort_states (struct lemon *lemp)
--  {
--    int i;
--    struct state *stp;
--    struct action *ap;
--
--    printf ("ResortStates\n");
--    for(i=0; i<lemp->nstate; i++){
--      stp = lemp->sorted[i];
--      stp->nTknAct = stp->nNtAct = 0;
--      stp->iDfltReduce = -1; /* Init dflt action to "syntax error" */
--      stp->iTknOfst = NO_OFFSET;
--      stp->iNtOfst = NO_OFFSET;
--      for(ap=stp->ap; ap; ap=ap->next){
--        int iAction = compute_action(lemp,ap);
--        if( iAction>=0 ){
--          if( ap->sp->index<lemp->nterminal ){
--            stp->nTknAct++;
--          }else if( ap->sp->index<lemp->nsymbol ){
--            stp->nNtAct++;
--          }else{
--            assert( stp->autoReduce==0 || stp->pDfltReduce==ap->x.rp );
--            stp->iDfltReduce = iAction;
--          }
--        }
--      }
--    }
--    qsort(&lemp->sorted[1], lemp->nstate-1, sizeof(lemp->sorted[0]),
--          stateResortCompare);
--    for(i=0; i<lemp->nstate; i++){
--      lemp->sorted[i]->statenum = i;
--    }
--    lemp->nxstate = lemp->nstate;
--    while( lemp->nxstate>1 && lemp->sorted[lemp->nxstate-1]->autoReduce ){
--      lemp->nxstate--;
--    }
--  }
   end Resort_States;


   procedure Dummy is
   begin
      null;
   end Dummy;


   procedure Rule_Print (File : in Ada.Text_IO.File_Type;
                         Rule : in Rules.Rule_Access)
   is
      use Ada.Text_IO;
      use Ada.Strings.Unbounded;
--      use type Symbols.Symbol_Kind;
      use Symbols;
   begin
      --  Print LHS
      Put (File, To_String (Rule.LHS.Name));

      --  Print LHS alias
      --  This part is uncommented in lemon.c
      if False then
         if Rule.LHS_Alias /= Null_Unbounded_String then
            Put (File, "(");
            Put (File, To_String (Rule.LHS_Alias));
            Put (File, ")");
         end if;
      end if;

      Put (File, " ::=");

      --  Print RHS
      for I in Rule.RHS.all'Range loop
         declare
            Symbol : constant Symbols.Symbol_Access := Rule.RHS (I);
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

            if Rule.RHS_Alias.Element (I) /= Null_Unbounded_String then
               Put (File, "(");
               Put (File, To_String (Rule.RHS_Alias.Element (I)));
               Put (File, ")");
            end if;
         end;
      end loop;
   end Rule_Print;


   procedure Print_Action
     (Action : in     Actions.Action_Record;
      File   : in     Ada.Text_IO.File_Type;
      Indent : in     Integer;
      Result :    out Boolean)
   is
   begin
--    struct action *ap,          /* The action to print */
--    FILE *fp,                   /* Print the action here */
--    int indent                  /* Indent by this amount */
--  ){
--    int result = 1;
--    switch( ap->type ){
--      case SHIFT: {
--        struct state *stp = ap->x.stp;
--        fprintf(fp,"%*s shift        %-7d",indent,ap->sp->name,stp->statenum);
--        break;
--      }
--      case REDUCE: {
--        struct rule *rp = ap->x.rp;
--        fprintf(fp,"%*s reduce       %-7d",indent,ap->sp->name,rp->iRule);
--        RulePrint(fp, rp, -1);
--        break;
--      }
--      case SHIFTREDUCE: {
--        struct rule *rp = ap->x.rp;
--        fprintf(fp,"%*s shift-reduce %-7d",indent,ap->sp->name,rp->iRule);
--        RulePrint(fp, rp, -1);
--        break;
--      }
--      case ACCEPT:
--        fprintf(fp,"%*s accept",indent,ap->sp->name);
--        break;
--      case ERROR:
--        fprintf(fp,"%*s error",indent,ap->sp->name);
--        break;
--      case SRCONFLICT:
--      case RRCONFLICT:
--        fprintf(fp,"%*s reduce       %-7d ** Parsing conflict **",
--          indent,ap->sp->name,ap->x.rp->iRule);
--        break;
--      case SSCONFLICT:
--        fprintf(fp,"%*s shift        %-7d ** Parsing conflict **",
--          indent,ap->sp->name,ap->x.stp->statenum);
--        break;
--      case SH_RESOLVED:
--        if( lemon_show_conflict ){
--          fprintf(fp,"%*s shift        %-7d -- dropped by precedence",
--                  indent,ap->sp->name,ap->x.stp->statenum);
--        }else{
--          result = 0;
--        }
--        break;
--      case RD_RESOLVED:
--        if( lemon_show_conflict ){
--          fprintf(fp,"%*s reduce %-7d -- dropped by precedence",
--                  indent,ap->sp->name,ap->x.rp->iRule);
--        }else{
--          result = 0;
--        }
--        break;
--      case NOT_USED:
--        result = 0;
--        break;
--    }
--    if( result && ap->spOpt ){
--      fprintf(fp,"  /* because %s==%s */", ap->sp->name, ap->spOpt->name);
--    }
--    return result;
      null;
   end Print_Action;


   procedure Config_Print (File : in Ada.Text_IO.File_Type;
                           CFP  : in Configs.Config_Access)
   is
   begin
      null;
      --  RulePrint(fp, cfp->rp, cfp->dot);
   end Config_Print;


   function Minimum_Size_Type (LWS     : in     Integer;
                               UPR     : in     Integer;
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


   procedure Write_Rule_Text (Rule : in Rules.Rule_Access)
   is
      use Text_Out;
      use Symbols;

      Symbol : Symbol_Access;
   begin
      Put (Name_Of (Symbol_Access (Rule.LHS)));
      Put (" ::=");
      for J in 0 .. Rule.RHS'Length - 1 loop

         Symbol := Rule.RHS (J);

         if Symbol.Kind /= Multi_Terminal then
            Put (" ");
            Put (Name_Of (Symbol));

         else
            Put (" ");
            Put (Name_Of (Symbol.Sub_Symbol.First_Element));

            for K in 1 .. Symbol.Sub_Symbol.Last_Index loop
               Put ("|");
               Put (Name_Of (Symbol.Sub_Symbol.Element (K)));
            end loop;
         end if;

      end loop;
   end Write_Rule_Text;


   procedure Emit_Destructor_Code
     (Symbol : in Symbols.Symbol_Access;
      Session   : in Sessions.Session_Type)
   is
      --  char *cp = 0;
   begin
      null;
--   if( sp->type==TERMINAL ){
--     cp = lemp->tokendest;
--     if( cp==0 ) return;

--     lime_put_line ("{");
--   }else if( sp->destructor ){
--     cp = sp->destructor;
--     lime_put_line ("{");
--     if( !lemp->nolinenosflag ){
--       //(*lineno)++;   // XXX
--       lime_write_line_directive (sp->destLineno, lemp->filename);
--     }
--   }else if( lemp->vardest ){
--     cp = lemp->vardest;
--     if( cp==0 ) return;

--     lime_put_line ("{");
--   }else{
--   assert( 0 );  // Cannot happen
--   }
--   for(; *cp; cp++){
--     if( *cp=='$' && cp[1]=='$' ){
--       lime_put ("(yypminor->yy");
--       lime_put_int (sp->dtnum);
--       lime_put (")");
--       cp++;
--       continue;
--     }
--     //   if( *cp=='\n' ) (*lineno)++;  //  XXX
--     //fputc(*cp,out);
--     char lime_string[2];
--     lime_string[0] = *cp;
--     lime_string[1] = 0;
--     lime_put (lime_string);
--     //lime_put ("" & *cp);  // XXX
--   }
--   lime_put_line ("");
--   if (!lemp->nolinenosflag) {
--     lime_write_line_directive (0, lemp->outname);
--     //lime_write_line_directive (0, lime_get_out_name ());
--   }
--   lime_put_line ("}");
--   return;
   end Emit_Destructor_Code;


   procedure Emit_Code
     (Rule   : in Rules.Rule_Access;
      Session : in Sessions.Session_Type)
   is
--         const char *cp;
   begin
      null;
--   // Setup code prior to the #line directive
--   if( rp->codePrefix && rp->codePrefix[0] ){
--     lime_put ("{");
--     lime_put (rp->codePrefix);
--     for(cp=rp->codePrefix; *cp; cp++)
--       {
--         //if( *cp=='\n' ) //  XXX
--         //  (*lineno)++;
--       }
--   }

--  // Generate code to do the reduce action
--   if( rp->code ){
--     if( !lemp->nolinenosflag ){
--       lime_write_line_directive (rp->line, lemp->filename);
--     }
--     lime_put ("{");
--     lime_put (rp->code);
--     for(cp=rp->code; *cp; cp++){
--       //if( *cp=='\n' ) (*lineno)++;
--     }
--     lime_put_line ("}");
--     if( !lemp->nolinenosflag ){
--       //(*lineno)++;
--       //tplt_linedir(out,*lineno,lemp->outname);  //  XXX
--       //lime_write_line_directive (*lineno, lemp->outname);
--       lime_write_line_directive (0, lemp->outname);
--       //lime_write_line_directive (0, lime_get_out_name ());
--     }
--   }

--  // Generate breakdown code that occurs after the #line directive
--   if( rp->codeSuffix && rp->codeSuffix[0] ){
--     lime_put (rp->codeSuffix);
--     for(cp=rp->codeSuffix; *cp; cp++); //{ if( *cp=='\n' ) (*lineno)++; }
--   }

--   if( rp->codePrefix ){
--     lime_put_line ("}");
--   }

--   return;
   end Emit_Code;


   procedure Print_Stack_Union
     (Session : Sessions.Session_Type)  --  The main info structure for this parser
   is
--    char **types;             /* A hash table of datatypes */
--    int arraysize;            /* Size of the "types" array */
--    int maxdtlength;          /* Maximum length of any ".datatype" field. */
--    char *stddt;              /* Standardized name for a datatype */
--    int i,j;                  /* Loop counters */
--    unsigned hash;            /* For hashing the name of a type */
--    const char *name;         /* Name of the parser */
   begin
      null;
--    /* Allocate and initialize types[] and allocate stddt[] */
--    arraysize = lemp->nsymbol * 2;
--    types = (char**)calloc( arraysize, sizeof(char*) );
--    if( types==0 ){
--      fprintf(stderr,"Out of memory.\n");
--      exit(1);
--    }
--    for(i=0; i<arraysize; i++) types[i] = 0;
--    maxdtlength = 0;
--    if( lemp->vartype ){
--      maxdtlength = lemonStrlen(lemp->vartype);
--    }
--    for(i=0; i<lemp->nsymbol; i++){
--      int len;
--      struct symbol *sp = lemp->symbols[i];
--      if( sp->datatype==0 ) continue;
--      len = lemonStrlen(sp->datatype);
--      if( len>maxdtlength ) maxdtlength = len;
--    }
--    stddt = (char*)malloc( maxdtlength*2 + 1 );
--    if( stddt==0 ){
--      fprintf(stderr,"Out of memory.\n");
--      exit(1);
--    }

--    /* Build a hash table of datatypes. The ".dtnum" field of each symbol
--    ** is filled in with the hash index plus 1.  A ".dtnum" value of 0 is
--    ** used for terminal symbols.  If there is no %default_type defined then
--    ** 0 is also used as the .dtnum value for nonterminals which do not specify
--    ** a datatype using the %type directive.
--    */
--    for(i=0; i<lemp->nsymbol; i++){
--      struct symbol *sp = lemp->symbols[i];
--      char *cp;
--      if( sp==lemp->errsym ){
--        sp->dtnum = arraysize+1;
--        continue;
--      }
--      if( sp->type!=NONTERMINAL || (sp->datatype==0 && lemp->vartype==0) ){
--        sp->dtnum = 0;
--        continue;
--      }
--      cp = sp->datatype;
--      if( cp==0 ) cp = lemp->vartype;
--      j = 0;
--      while( ISSPACE(*cp) ) cp++;
--      while( *cp ) stddt[j++] = *cp++;
--      while( j>0 && ISSPACE(stddt[j-1]) ) j--;
--      stddt[j] = 0;
--      if( lemp->tokentype && strcmp(stddt, lemp->tokentype)==0 ){
--        sp->dtnum = 0;
--        continue;
--      }
--      hash = 0;
--      for(j=0; stddt[j]; j++){
--        hash = hash*53 + stddt[j];
--      }
--      hash = (hash & 0x7fffffff)%arraysize;
--      while( types[hash] ){
--        if( strcmp(types[hash],stddt)==0 ){
--          sp->dtnum = hash + 1;
--          break;
--        }
--        hash++;
--        if( hash>=(unsigned)arraysize ) hash = 0;
--      }
--      if( types[hash]==0 ){
--        sp->dtnum = hash + 1;
--        types[hash] = (char*)malloc( lemonStrlen(stddt)+1 );
--        if( types[hash]==0 ){
--          fprintf(stderr,"Out of memory.\n");
--          exit(1);
--        }
--        lemon_strcpy(types[hash],stddt);
--      }
--    }

--    /* Print out the definition of YYTOKENTYPE and YYMINORTYPE */
--    const char*  tokentype;

--    name      = (lemp->name      ? lemp->name      : "Parse");
--    tokentype = (lemp->tokentype ? lemp->tokentype : "void*");
--    lime_write_interface (name, tokentype);
--  /*   name = lemp->name ? lemp->name : "Parse"; */
--  /*   if( mhflag ) */
--  /*     { */
--  /*       lime_put_line ("#if INTERFACE"); */
--  /*     } */
--  /*   lime_put ("#define "); */
--  /*   lime_put (name); */
--  /*   lime_put ("TOKENTYPE "); */
--  /*   lime_put_line ((lemp->tokentype ? lemp->tokentype : "void*")); */

--  /*   if( mhflag ) */
--  /*     { */
--  /*       lime_put_line ("#endif"); */
--  /*     } */

--    lime_put_line ("typedef union {");
--    lime_put_line ("  int yyinit;");
--    lime_put ("  ");
--    lime_put (name);
--    lime_put_line ("TOKENTYPE yy0;");

--    for(i=0; i<arraysize; i++){
--      if( types[i]==0 ) continue;
--      lime_put ("  ");
--      lime_put (types[i]);
--      lime_put (" yy");
--      lime_put_int (i+1);
--      lime_put_line (";");
--      free(types[i]);
--    }
--    if( lemp->errsym && lemp->errsym->useCnt ){
--      lime_put ("  int yy");
--      lime_put_int (lemp->errsym->dtnum);
--      lime_put_line (";");
--    }
--    free(stddt);
--    free(types);
--    lime_put_line ("} YYMINORTYPE;");
   end Print_Stack_Union;


 --  procedure Rule_Print (Rule : in Rules.Rule_Access)
 --  is
      --  int i, j;
--   begin
--      null;
--    lime_put (rp->lhs->name);
--    /*    if( rp->lhsalias ) fprintf(out,"(%s)",rp->lhsalias); */ // XXX
--    lime_put (" ::=");
--    for(i=0; i<rp->nrhs; i++){
--      struct symbol *sp = rp->rhs[i];
--      if( sp->type==MULTITERMINAL ){
--        lime_put (" ");
--        lime_put (sp->subsym[0]->name);
--        for(j=1; j<sp->nsubsym; j++){
--          lime_put ("|");
--          lime_put (sp->subsym[j]->name);
--        }
--      }else{
--        lime_put (" ");
--        lime_put (sp->name);
--      }
--      /* if( rp->rhsalias[i] ) fprintf(out,"(%s)",rp->rhsalias[i]); */  //  XXX
--    }
--   end Rule_Print;


--  /* Print a single rule.
--  */
--  void RulePrint(FILE *fp, struct rule *rp, int iCursor){
--    struct symbol *sp;
--    int i, j;
--    fprintf(fp,"%s ::=",rp->lhs->name);
--    for(i=0; i<=rp->nrhs; i++){
--      if( i==iCursor ) fprintf(fp," *");
--      if( i==rp->nrhs ) break;
--      sp = rp->rhs[i];
--      if( sp->type==MULTITERMINAL ){
--        fprintf(fp," %s", sp->subsym[0]->name);
--        for(j=1; j<sp->nsubsym; j++){
--          fprintf(fp,"|%s",sp->subsym[j]->name);
--        }
--      }else{
--        fprintf(fp," %s", sp->name);
--      }
--    }
--  }

   procedure Generate_Tokens
     (Session      : in Sessions.Session_Type;
      Token_Prefix : in String;
      First        : in Integer;
      Last         : in Integer)
   is
      pragma Unreferenced (Session);

      function Get_Prefix return String;

      function Get_Prefix return String is
      begin
         if Token_Prefix /= "" then
            return Token_Prefix;
         else
            return "";
         end if;
      end Get_Prefix;

      use Text_Out;
      use Symbols, Extras;

      Prefix : constant String := Get_Prefix;
   begin
      if Options.MH_Flag then
         --  const char *prefix; */
         Put_Line ("#if INTERFACE");
--         Line_Number := Line_Number + 1;

         for I in First .. Last loop
--              Put_Line (Context.File_Implementation,
--                        "#define " &
--                          Value (Prefix)   &
--                          Value (Get_Token_Callback (I)) &
--                          " " & Integer'Image (I));
--              Line_Number := Line_Number + 1;
            Put ("#define ");
            Put (Prefix);
            --  Put_CP (Get_Token_Callback (I));
            --  return lime_lemp_copy->symbols[index]->name;
            Put (Name_Of (Element_At (Symbol_Index (I))));
            Put (" ");
            Put_Int (I);
            New_Line;
         end loop;
         Put_Line ("#endif");
      end if;
   end Generate_Tokens;




   procedure Render_Constants
     (Render : in Render_Record)
   is
      procedure Put (Item  : in String;
                     Value : in Integer);

      procedure Put (Item  : in String;
                     Value : in Integer)
      is
         use Text_Out;
      begin
         Put (Item);
         Put_Int (Value);
         New_Line;
      end Put;

      I : Integer;
   begin
      Put ("#define YYNSTATE             ", Render.Nx_State);
      Put ("#define YYNRULE              ", Render.N_Rule);
      Put ("#define YYNTOKEN             ", Render.N_Terminal);
      Put ("#define YY_MAX_SHIFT         ", Render.Nx_State - 1);
      I := Render.Min_Shift_Reduce;
      Put ("#define YY_MIN_SHIFTREDUCE   ", I);
      I := I + Render.N_Rule;
      Put ("#define YY_MAX_SHIFTREDUCE   ", I - 1);
      Put ("#define YY_ERROR_ACTION      ", Render.Err_Action);
      Put ("#define YY_ACCEPT_ACTION     ", Render.Acc_Action);
      Put ("#define YY_NO_ACTION         ", Render.No_Action);
      Put ("#define YY_MIN_REDUCE        ", Render.Min_Reduce);
      I := Render.Min_Reduce + Render.N_Rule;
      Put ("#define YY_MAX_REDUCE        ", I - 1);
   end Render_Constants;


   --  lemon.c:4377
   procedure Output_Action_Table
     (Action_Table : in Actions.A_Action_Table;
      N            : in Integer;
      No_Action    : in Integer)
   is
      use Text_Out;
      use DK8543.Auxiliary;
      J : Integer;
      Action : Integer;
   begin
      Put_Line ("#define YY_ACTTAB_COUNT (" & Image (N) & ")");
      Put_Line ("static const YYACTIONTYPE yy_action[] = {");
      J := 0;
      for I in 0 .. N - 1 loop
         --  #define acttab_yyaction(X,N)  ((X)->aAction[N].action)
         --  return acttab_yyaction (lime_pActtab, i);
         --  struct acttab *lime_pActtab;
         --  Action := Get_Acttab_YY_Action (I);
         Action := Action_Table.Action (I).Action;
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

   end Output_Action_Table;


   procedure Output_YY_Lookahead
     (Action_Table : in Actions.A_Action_Table;
      N            : in Integer;
      Nsymbol      : in Integer)
   is
      use Text_Out;
      use DK8543.Auxiliary;
      LA : Integer;
      J  : Integer := 0;
   begin
      Put_Line ("static const YYCODETYPE yy_lookahead[] = {");
      for I in 0 .. N - 1 loop
         --  LA := Get_Acttab_YY_Lookahead (I);
         LA := Action_Table.Lookahead (I).Action;
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
   end Output_YY_Lookahead;


   --  lemon.c:4414
   procedure Output_YY_Shift_Offsets
     (Session       : in Sessions.Session_Type;
      N             : in Integer;
      MnTknOfst     : in Integer;
      MxTknOfst     : in Integer;
      Min_Size_Type : in String;
      Nactiontab    : in Integer;
      NO_OFFSET     : in Integer)
   is
      use Text_Out;
      use DK8543.Auxiliary;
      Ofst : Integer;
      J    : Integer := 0;
   begin
      Put_Line ("#define YY_SHIFT_COUNT    (" & Image (N - 1) & ")");
      Put_Line ("#define YY_SHIFT_MIN      (" & Image (MnTknOfst) & ")");
      Put_Line ("#define YY_SHIFT_MAX      (" & Image (MxTknOfst) & ")");
      Put ("static const ");
      Put (Min_Size_Type);
      Put (" yy_shift_ofst[] = {");
      New_Line;
--  lemp->tablesize += n*sz;
      for I in 0 .. N - 1 loop
         declare
            use Symbols;
            use Extras;
            use Sessions;

            State : access States.State_Record;  --  States.State_Access;
         begin
            --  stp := lemp->sorted[i];
            State := Session.Sorted (State_Index (I));
            --  ofst := stp->iTknOfst;
            --  Ofst := Get_Token_Offset (I);
            Ofst := State.Token_Offset;
         end;
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
   end Output_YY_Shift_Offsets;


   --  lemon.c:4440
   procedure Output_YY_Reduce_Offsets
     (Session          : in Sessions.Session_Type;
      N             : in Integer;
      MnNtOfst      : in Integer;
      MxNtOfst      : in Integer;
      Min_Size_Type : in String;
      NO_OFFSET     : in Integer)
   is
      use Text_Out;
      use DK8543.Auxiliary;
      J : Integer := 0;
      Ofst : Integer;
   begin
      Put_Line ("#define YY_REDUCE_COUNT (" & Image (N - 1) & ")");
      Put_Line ("#define YY_REDUCE_MIN   (" & Image (MnNtOfst) & ")");
      Put_Line ("#define YY_REDUCE_MAX   (" & Image (MxNtOfst) & ")");
      Put_Line ("static const " & Min_Size_Type & " yy_reduce_ofst[] = {");

--  lemp->tablesize += n*sz;
      for I in 0 .. N - 1 loop
         declare
            use Symbols;
            use Extras;

            State : access States.State_Record;
         begin
            State := Session.Sorted (Sessions.State_Index (I));
            Ofst := State.iNtOfst;
         end;
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
   end Output_YY_Reduce_Offsets;


   --  lemon.c:4465
   procedure Output_Default_Action_Table
     (Session         : in Sessions.Session_Type;
      N            : in Integer;
      Error_Action : in Integer;
      Min_Reduce   : in Integer)
   is
      use Text_Out;
      use DK8543.Auxiliary;
      J : Integer := 0;
--      IDfltReduce : Integer;
   begin
      Put_Line ("static const YYACTIONTYPE yy_default[] = {");
      for I in 0 .. N - 1 loop
         declare
            use Symbols;
            use Extras;

            State : constant access States.State_Record :=
              Session.Sorted (Sessions.State_Index (I));
         begin
--         IDfltReduce := Get_Default_Reduce (I);
--         stp := lemp->sorted[i];
            if J = 0 then
               Put (" /* " & Image (I) & " */ ");
            end if;
            if State.iDfltReduce then
               Put (" " & Image (Error_Action) & ",");
            else
               Put (" " & Image (Boolean'Pos (State.iDfltReduce) + Min_Reduce) & ",");
            end if;
         end;
         if J = 9 or I = N - 1 then
            Put_Line ("");
            J := 0;
         else
            J := J + 1;
         end if;
      end loop;
      Put_Line ("};");
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


   procedure Write_Arg_Defines
     (Name    : in String;
      Arg_Ctx : in String;
      Extend  : in Boolean;
      Arg     : in String;
      Arg_I   : in String)
   is

      procedure Write (Decl : in String);

      procedure Write (Decl : in String) is
         use Text_Out;
      begin
         Put_Line ("#define " & Name & Arg_Ctx & Decl & Arg & ";");
      end Write;

      use Text_Out;
   begin
      Write ("_SDECL ");
      Write ("_PDECL ,");
      Write ("_PARAM ,");
      if Extend = False then
         Put_Line ("#define " & Name & "_FETCH " &
                     Arg   & "=yypParser->" & Arg_I & ";");
         Put_Line ("#define " & Name & "_STORE " &
                     Arg_I & "=yypParser->" & Arg_I & ";");
      else
         Write ("_FETCH ");
         Write ("_STORE ");
      end if;
   end Write_Arg_Defines;


   procedure Write_Interface
     (Name      : in String;
      Tokentype : in String)
   is
      use Text_Out;
   begin
      if Options.MH_Flag then
         Put_Line ("#if INTERFACE");
      end if;

      Put ("#define ");
      Put (Name);
      Put ("TOKENTYPE ");
      Put (Tokentype);
      New_Line;

      if Options.MH_Flag then
         Put_Line ("#endif");
      end if;
   end Write_Interface;


   procedure Write_Interface_Begin
   is
      use Text_Out;
   begin
      if Options.MH_Flag then
         Put_Line ("#if INTERFACE");
      end if;
   end Write_Interface_Begin;


   procedure Write_Interface_End
   is
      use Text_Out;
   begin
      if Options.MH_Flag then
         Put_Line ("#endif");
      end if;
   end Write_Interface_End;


   procedure Report_Header
     (Session          : in Sessions.Session_Type;
      Token_Prefix  : in String;
      Base_Name     : in String;
      Module_Name   : in String;
      Terminal_Last : in Natural)
   is
      Prefix : constant String := Token_Prefix;
   begin

      if not Options.MH_Flag then
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


   procedure Generate_Spec
     (Session      : in Sessions.Session_Type;
      Base_Name : in String;
      Prefix    : in String;
      Module    : in String;
      First     : in Integer;
      Last      : in Integer)
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
      Symbol_Count   : in Symbols.Symbol_Index;
      YY_Action_Type : in String;
      Is_Wildcard    : in Boolean;
      Wildcard_Index : in Symbols.Symbol_Index)
   is
      use Text_Out;
   begin
      Put ("#define YYCODETYPE ");
      Put (YY_Code_Type);
      New_Line;

      Put ("#define YYNOCODE ");
      Put_Int (Integer (Symbol_Count));
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


end Reports;
