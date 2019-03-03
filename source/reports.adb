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

with Interfaces.C.Strings;

with Rules;
with Symbols;
with Parsers;
with Text_Out;
with Actions;
with Configs;
with States;
with Options;

package body Reports is

   procedure Reprint_C (Lemp : access Lime.Lemon_Record) is
   begin
      Reprint (Lemp.all);
   end Reprint_C;

   procedure Report_Output_C (Lemp : access Lime.Lemon_Record) is
   begin
      Reprint (Lemp.all);
   end Report_Output_C;

   procedure Report_Table_C (Lemp : access Lime.Lemon_Record) is
   begin
      Report_Table (Lemp.all);
   end Report_Table_C;

   procedure Compress_Tables_C (Lemp : access Lime.Lemon_Record) is
   begin
      Compress_Tables (Lemp.all);
   end Compress_Tables_C;

   procedure Resort_States_C (Lemp : access Lime.Lemon_Record) is
   begin
      Resort_States (Lemp.all);
   end Resort_States_C;

   procedure Rule_Print_2 (File : in Ada.Text_IO.File_Type;
                           RP   : in Rules.Rule_Access);

   procedure Print_Action
     (AP     : in     Actions.Action_Access;
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

   function File_Makename (Global    : in Lime.Lemon_Record;
                           Extension : in String) return String;


   procedure Write_Rule_Text (Rule : in Rules.Rule_Access);
   --  Write text on "out" that describes the rule "rp".

   procedure Emit_Destructor_Code
     (SP   : in Symbols.Symbol_Access;
      Lemp : in Lime.Lemon_Record);
   --  The following routine emits code for the destructor for the
   --  symbol sp

   procedure Emit_Code
     (
      --  FILE *out,
      RP   : in Rules.Rule_Access;
      Lemp : in Lime.Lemon_Record
      --  int *lineno
     );
   --  Generate code which executes when the rule "rp" is reduced.  Write
   --  the code to "out".  Make sure lineno stays up-to-date.


   procedure Print_Stack_Union
     (Lemp : Lime.Lemon_Record);  --  The main info structure for this parser

   --  struct lemon *lemp //,
   --  //  int mhflag                  /* True if generating makeheaders output */

   --  Print the definition of the union used for the parser's data stack.
   --  This union contains fields for every possible data type for tokens
   --  and nonterminals.  In the process of computing and printing this
   --  union, also set the ".dtnum" field of every terminal and nonterminal
   --  symbol.

   procedure Rule_Print (RP : in Rules.Rule_Access);
   --  Print the text of a rule

   procedure Generate_Tokens
     (Lemon        : in Lime.Lemon_Record;
      Token_Prefix : in String;
      First        : in Integer;
      Last         : in Integer);

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

   procedure Reprint (Lemp : in Lime.Lemon_Record)
   is
      use Ada.Text_IO;
      use Ada.Strings.Unbounded;
      use Interfaces.C.Strings;
      use Symbols;
      use Rules;

      RP   : Rules.Rule_Access;
      SP   : Symbol_Access;
      J    : Symbol_Index;
      Max_Len, Len, N_Columns, Skip : Integer;
   begin
      Put ("// Reprint of input file '");
      Put (To_String (Unbounded_String'(Lemp.File_Name)));
      Put_Line ("'.");
      Put_Line ("// Symbols:");
      Max_Len := 10;

      --  Determine Max_Len
      for I in 0 .. Lemp.N_Symbol loop
         SP  := Symbols.Element_At (Lemp.Extra, Index => I);
         Len := Symbols.Length (SP.Name);
         if Len > Max_Len then
            Max_Len := Len;
         end if;
      end loop;

      --  Determine N_Columns
      N_Columns := 76 / (Max_Len + 5);
      if N_Columns < 1 then
         N_Columns := 1;
      end if;

      Skip := (Integer (Lemp.N_Symbol) + N_Columns - 1) / N_Columns;
      for I in 0 .. Skip - 1 loop
         Put ("//");
         J := Symbols.Symbol_Index (I);
         loop
            exit when J >= Lemp.N_Symbol;
            SP := Element_At (Lemp.Extra, Index => J);
            pragma Assert (SP.Index = J);
            --  printf(" %3d %-*.*s",j,maxlen,maxlen,sp->name);
            --  XXX
            Put (" ");
            Put (Symbol_Index'Image (J));
            Put (" ");
            Put (Integer'Image (Max_Len));
            Put (Integer'Image (Max_Len));
            Put (Symbols.From_Key (SP.Name));
            J := J + Symbol_Index (Skip);
         end loop;
         New_Line;
      end loop;

      RP := Lemp.Rule;
      loop
         exit when RP = null;
         Rule_Print_2 (Standard_Output, RP);
         Put (".");
         if RP.Prec_Sym /= null then
            Put (" [" & From_Key (RP.Prec_Sym.Name) & "]");
         end if;
         --  /* if( rp->code ) printf("\n    %s",rp->code); */
         New_Line;
         RP := RP.Next;
      end loop;
   end Reprint;


   procedure Report_Output (Lemp : in Lime.Lemon_Record)
   is
      use Ada.Text_IO;
      use Symbols;
      use Rules;
      use Lime;
      use Configs;
      use Actions;

      File : File_Type;

      Action_Result : Boolean;

      N   : Integer;
      STP : States.State_Access;
      CFP : Config_Access;
      AP  : Action_Access;
      RP  : Rule_Access;
   begin
--  fp = file_open(lemp,".out","wb");
      Open (File, Out_File, "XXX.out");

      for I in Symbol_Index range 0 .. Symbol_Index (Lemp.Nx_State) - 1 loop
         STP := Sorted_At (Lemp.Extra, Index => I);
         Put (File, "State ");
         Put (File, Integer'Image (STP.State_Num));
         Put (File, ":");
         New_Line;

         if Lemp.Basis_Flag then
            CFP := Config_Access (STP.BP);
         else
            CFP := Config_Access (STP.CFP);
         end if;

         while CFP /= null loop
            if CFP.DOT = CFP.RP.RHS'Length then
               Put (File, "    (");
               Put (File, CFP.RP.Rule'Img);
               Put (File, ") ");
            else
               Put (File, "          ");
            end if;
            Config_Print (File, CFP);
            New_Line (File);

--      SetPrint(fp,cfp->fws,lemp);
--      PlinkPrint(fp,cfp->fplp,"To  ");
--      PlinkPrint(fp,cfp->bplp,"From");

            if Lemp.Basis_Flag then
               CFP := CFP.Basis;
            else
               CFP := CFP.Next;
            end if;
         end loop;
         New_Line (File);

         AP := Actions.Action_Access (STP.AP);
         while AP /= null loop
            Print_Action (AP, File, 30, Action_Result);
            if Action_Result then
               New_Line (File);
            end if;
            AP := AP.Next;
         end loop;
         New_Line (File);
      end loop;

      Put_Line (File, "----------------------------------------------------");
      Put_Line (File, "Symbols:");
      Put_Line (File, "The first-set of non-terminals is shown after the name.");
      New_Line (File);

      for I in 0 .. Lemp.N_Symbol - 1 loop
         declare
            SP : Symbol_Access;
         begin
            SP := Element_At (Lemp.Extra, Index => I);
            Put (File, "  " & I'Img & ": " & From_Key (SP.Name));
            if SP.Kind = Non_Terminal then
               Put (File, ":");
               if SP.Lambda then
                  Put (File, " <lambda>");
               end if;
               for J in 0 .. Lemp.N_Terminal - 1 loop
                  if Symbols."=" (SP.First_Set, Null_Set) then
                     --  XXX and SetFind (SP.First_Set, J) then
                     Put (File, " ");
                     Put (File, From_Key (Element_At (Lemp.Extra, Index => J).Name));
                  end if;
               end loop;
            end if;
            if SP.Prec >= 0 then
               Put (File, " (precedence=");
               Put (File, SP.Prec'Img);
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
      for I in 0 .. Lemp.N_Symbol loop
         declare
            W  : Integer;
            SP : constant Symbol_Access := Element_At (Lemp.Extra, Index => I);
         begin
            if not SP.Content then
               W := Length (SP.Name);
               if N > 0 and N + W > 75 then
                  New_Line (File);
                  N := 0;
               end if;
               if N > 0 then
                  Put (File, " ");
                  N := N + 1;
               end if;
               Put (File, From_Key (SP.Name));
               N := N + W;
            end if;
         end;
      end loop;
      if N > 0 then
         New_Line (File);
      end if;

      Put_Line (File, "----------------------------------------------------");
      Put_Line (File, "Rules:");

      RP := Lemp.Rule;
      loop
         exit when RP = null;
         Put (File, RP.Rule'Img); -- XXX "%4d: ", rp->iRule);
         Put (File, ": ");
         Rule_Print_2 (File, RP);
         Put (File, ".");
         if RP.Prec_Sym /= null then
            Put (File, " [");
            Put (File, From_Key (RP.Prec_Sym.Name));
            Put (File, " precedence=");
            Put (File, RP.Prec_Sym.Prec'Img);
            Put (File, "]");
         end if;
         New_Line (File);
         RP := RP.Next;
      end loop;

      Close (File);
   end Report_Output;


   procedure Report_Table (Lemp : in out Lime.Lemon_Record)
   is
      use Ada.Strings.Unbounded;
      use Interfaces.C.Strings;
      use Lime;
      use Rules;
      package Acttab renames Actions;

      Lemp_Name : constant String := To_String (Lemp.Names.Name);
--    char line[LINESIZE];
      STP : States.State_Access;
--    struct action *ap;
      RP  : Rules.Rule_Access;

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
      Lemp.Min_Shift_Reduce := Lemp.N_State;
      Lemp.Err_Action       := Lemp.Min_Shift_Reduce + Lemp.N_Rule;
      Lemp.Acc_Action       := Lemp.Err_Action + 1;
      Lemp.No_Action        := Lemp.Acc_Action + 1;
      Lemp.Min_Reduce       := Lemp.No_Action + 1;
      Lemp.Max_Action       := Lemp.Min_Reduce + Lemp.N_Rule;

      Template_Open (Value (Lemon_User_Template), Error_Count, Template_Open_Success);
      Implementation_Open (File_Makename (Lemp, ".c"));

      Template_Transfer (Lemp_Name);

      --  Generate the include code, if any
      --  Lime_Print (Lemp.Outname, Lemp.No_Linenos_Flag, Lemp.Include);
      Template_Print (Ada.Strings.Unbounded.To_String (Lemp.Out_Name),
                      Boolean'Pos (Lemp.No_Linenos_Flag),
                      New_String (To_String (Lemp.Names.Include)));
      --  lime_print (lime_get_ouüt_name (), lemp->nolinenosflag, lemap->include);
      --  lime_write_include (lime_get_mh_flag(), file_makename(lemp, ".h"));
      Write_Include (File_Makename (Lemp, ".h"));

      Template_Transfer (Lemp_Name);

      --  Generate #defines for all tokens
--  XXX    Lime_Lemp_Copy := Lemp;
      --  lime_generate_tokens (lime_get_mh_flag(), lemp->tokenprefix, 1, lemp->nterminal);
      Generate_Tokens (Lemp, To_String (Lemp.Names.Token_Prefix), 1, Integer (Lemp.N_Terminal));

      Template_Transfer (Lemp_Name);

      --  Generate the defines
      declare
         use Symbols;

         Code     : constant String := Minimum_Size_Type (0, Integer (Lemp.N_Symbol),
                                                          Size_Of_Code_Type);
         Action   : constant String := Minimum_Size_Type (0, Lemp.Max_Action,
                                                          Size_Of_Action_Type);
         Wildcard    : constant Symbol_Access := Get_Wildcard (Lemp.Extra);
         Is_Wildcard : constant Boolean       := (Wildcard /= null);
      begin
         if Is_Wildcard then
            Generate_The_Defines_1
              (Code,
               Integer (Lemp.N_Symbol),
               Action,
               Is_Wildcard    => True,
               Wildcard_Index => Wildcard.Index);
         else
            Generate_The_Defines_1
            (Code,
             Integer (Lemp.N_Symbol),
             Action,
             Is_Wildcard    => False,
             Wildcard_Index => 0);
         end if;
      end;

      --  print_stack_union (lemp, lime_get_mh_flag());
      Print_Stack_Union (Lemp);
      Generate_The_Defines_2 (To_String (Lemp.Names.Stack_Size));

--    //if( lime_get_mh_flag() ){
--    //  lime_put_line ("#if INTERFACE");
--    //}
      Write_Interface_Begin;

      declare

         function Get_Name return String;

         function Get_Name return String is
         begin
            if Lemp.Names.Name /= "" then
               return To_String (Lemp.Names.Name);
            else
               return "Parse";
            end if;
         end Get_Name;


         Name : constant String := Get_Name;

         use Parsers;
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
      begin
         AX := new AX_Set_Array (0 .. Symbol_Index (Lemp.Nx_State) - 1);

      --  if( ax==0 ){
      --    fprintf(stderr,"malloc failed\n");
      --    exit(1);

         for I in Symbol_Index range 0 .. Symbol_Index (Lemp.Nx_State - 1) loop
            STP := Sorted_At (Lemp.Extra, Index => I);

            AX (I).Token := (STP      => STP,
                             Is_Token => True,
                             N_Action => STP.N_Tkn_Act,
                             Order    => <>);

            AX (I).Non_Terminal := (STP      => STP,
                                    Is_Token => False,
                                    N_Action => STP.N_Nt_Act,
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
      Act_Tab := Acttab.Alloc (Integer (Lemp.N_Symbol), Integer (Lemp.N_Terminal));
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
           (Nx_State         => Lemp.Nx_State,
            N_Rule           => Lemp.N_Rule,
            N_Terminal       => Integer (Lemp.N_Terminal),
            Min_Shift_Reduce => Lemp.Min_Shift_Reduce,
            Err_Action       => Lemp.Err_Action,
            Acc_Action       => Lemp.Acc_Action,
            No_Action        => Lemp.No_Action,
            Min_Reduce       => Lemp.Min_Reduce));

      Template_Transfer (Lemp_Name);


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
         Lemp.N_Action_Tab := Acttab.Action_Size (Act_Tab.all);
         N := Lemp.N_Action_Tab;
         Lemp.Table_Size := Lemp.Table_Size + N * Size_Of_Action_Type;

         Output_Action_Table (Act_Tab, N, Lemp.No_Action);

         --
         --  Output the yy_lookahead table
         --
         Lemp.N_Lookahead_Tab := Acttab.Lookahead_Size (Act_Tab.all);
         N := Lemp.N_Lookahead_Tab;
         Lemp.Table_Size := Lemp.Table_Size + N * Size_Of_Code_Type;
      end;

      Output_YY_Lookahead (Act_Tab, N, Integer (Lemp.N_Symbol));

      --
      --  Output the yy_shift_ofst[] table
      --

      N := Lemp.Nx_State;
--      while  N > 0 and Lemp.Sorted(N - 1).I_Tkn_Ofst = NO_Offset loop
--         N := N - 1;
--      end loop;
--
--    lime_lemp = lemp;
      Output_YY_Shift_Offsets
        (Lemp, N,
         Mn_Tkn_Ofst,
         Mx_Tkn_Ofst,
         Minimum_Size_Type (Mn_Tkn_Ofst, Integer (Lemp.N_Terminal) + Lemp.N_Action_Tab, SZ),
         Lemp.N_Action_Tab,
         No_Offset);

      Lemp.Table_Size := Lemp.Table_Size + N * SZ;

      --
      --  Output the yy_reduce_ofst[] table
      --
      N := Lemp.Nx_State;
--    while( n>0 && lemp->sorted[n-1]->iNtOfst==NO_OFFSET ) n--;
--
      Output_YY_Reduce_Offsets
        (Lemp, N,
         Mn_Nt_Ofst,
         Mx_Nt_Ofst,
         Minimum_Size_Type (Mn_Nt_Ofst - 1, Mx_Nt_Ofst, SZ),
         No_Offset);
      Lemp.Table_Size := Lemp.Table_Size + N * SZ;

      --
      --  Output the default action table
      --
      Output_Default_Action_Table
        (Lemp, Lemp.Nx_State,
         Lemp.Err_Action,
         Lemp.Min_Reduce);
      Lemp.Table_Size := Lemp.Table_Size + N * Size_Of_Action_Type;

      Template_Transfer (Lemp_Name);

      --
      --  Generate the table of fallback tokens.
      --
      if Lemp.Has_Fallback then
         declare
--            use Ada.Strings.Unbounded;
            use Symbols;
            MX : Symbol_Index := Lemp.N_Terminal - 1;
         begin
            --  while MX > 0 and Lemp.Symbols (MX).Fallback = 0 loop
            while
              MX > 0 and
              Element_At (Lemp.Extra, Index => MX).Fallback = null
            loop
               MX := MX - 1;
            end loop;
            Lemp.Table_Size := Lemp.Table_Size + Integer (MX + 1) * Size_Of_Code_Type;

            for I in 0 .. MX loop
               declare
                  use Text_Out;
                  P : constant Symbol_Access := Element_At (Lemp.Extra, I);
               begin
                  if P.Fallback = null then
                     Put ("    0,  /* ");
                     Put (From_Key (P.Name));
                     Put_Line (" => nothing */");
                  else
                     Put ("  ");
                     Put_Int (Integer (P.Fallback.Index));
                     Put (",  /* ");
                     Put (From_Key (P.Name));
                     Put (" => ");
                     Put (To_String (P.Fallback.Name));
                     Put_Line (" */");
                  end if;
               end;
            end loop;
         end;
      end if;

      Template_Transfer (Lemp_Name);

      --
      --  Generate A Table Containing the symbolic name of every symbol
      --
      declare
         use Text_Out;
         use Symbols;
         J : Integer;
         RP : Rules.Rule_Access;
      begin
         for I in Symbol_Index range 0 .. Symbol_Index (Lemp.N_Symbol - 1) loop
            declare
               Name : constant String := From_Key (Element_At (Lemp.Extra, I).Name);
            begin
               --  Lemon_Sprintf (Line, """" & Name & """,");
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

         Template_Transfer (Lemp_Name);

         --  Generate a table containing a text string that describes every
         --  rule in the rule set of the grammar.  This information is used
         --  when tracing REDUCE actions.
         J  := 0;
         RP := Lemp.Rule;

         while RP /= null loop
            pragma Assert (RP.Rule = J);
            --  fprintf(out," /* %3d */ \"", i);
            Put (" /* ");
            Put_Int (J);
            Put (" */ """);
            Write_Rule_Text (RP);
            --  fprintf(out,"\",\n"); lineno++;
            Put_Line (""",");
            RP := RP.Next;
         end loop;
      end;

      Template_Transfer (Lemp_Name);

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
      begin
         I := 0;
         loop
            exit when I >= Integer (Lemp.N_Symbol);
            exit when Element_At (Lemp.Extra, Symbol_Index (I)).Kind = Terminal;
            I := I + 1;
         end loop;

         --  I : Symbols.Symbol_Index;
         --      if( i<lemp->nsymbol ){
         Emit_Destructor_Code (Symbols.Element_At (Lemp.Extra, Symbol_Index (I)), Lemp);
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
      Rule_Print (RP);
--      lime_put_line (" */");
--    }
--
      Template_Transfer (Lemp_Name);
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
      Template_Transfer (Lemp_Name);

      --  Generate code which execution during each REDUCE action
      I  := 0;
      RP := Lemp.Rule;
      while RP /= null loop
         --  I  := I + Translate_Code (Lemp, RP);
         RP := RP.Next;
      end loop;

      if I /= 0 then
         Text_Out.Put_Line ("        YYMINORTYPE yylhsminor;");
      end if;

      --  First output rules other than the default: rule
      RP := Lemp.Rule;
      while RP /= null loop
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
         Emit_Code (RP, Lemp);
--      lime_put_line ("        break;");
--      rp->codeEmitted = 1;
         RP := RP.Next;
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


   procedure Compress_Tables (Lemp : in Lime.Lemon_Record)
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


   procedure Resort_States (Lemp : in Lime.Lemon_Record)
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


   procedure Rule_Print_2 (File : in Ada.Text_IO.File_Type;
                           RP   : in Rules.Rule_Access)
   is
   begin
--    int i, j;
--    fprintf(out, "%s",rp->lhs->name);
--    //lime_put (rp->lhs->name);
--    /*    if( rp->lhsalias ) fprintf(out,"(%s)",rp->lhsalias); */
--    fprintf(out," ::=");
--    //lime_put (" ::=");
--    for(i=0; i<rp->nrhs; i++){
--      struct symbol *sp = rp->rhs[i];
--      if( sp->type==MULTITERMINAL ){
--        fprintf(out," %s", sp->subsym[0]->name);
--        //lime_put (" ");
--        //lime_put (sp->subsym[0]->name);
--        for(j=1; j<sp->nsubsym; j++){
--          fprintf(out,"|%s", sp->subsym[j]->name);
--          //lime_put ("|");
--          //lime_put (sp->subsym[j]->name);
--        }
--      }else{
--        fprintf(out," %s", sp->name);
--        //lime_put (" ");
--        //lime_put (sp->name);
--      }
--      /* if( rp->rhsalias[i] ) fprintf(out,"(%s)",rp->rhsalias[i]); */
--    }
      null;
   end Rule_Print_2;


   procedure Print_Action
     (AP     : in     Actions.Action_Access;
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


   function File_Makename (Global    : in Lime.Lemon_Record;
                           Extension : in String) return String
   is
      use Ada.Strings;
      use Ada.Strings.Unbounded;
      use Interfaces.C.Strings;
      File_Name    : constant String  := To_String (Global.File_Name);
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
      Put (From_Key (Rule.LHS.Name));
      Put (" ::=");
      for J in 0 .. Rule.RHS'Length - 1 loop

         Symbol := Rule.RHS (J);

         if Symbol.Kind /= Multi_Terminal then
            Put (" ");
            Put (From_Key (Symbol.Name));

         else
            Put (" ");
            Put (From_Key (Symbol.Sub_Sym.First_Element.Name));

            for K in 1 .. Symbol.Sub_Sym.Last_Index loop
               Put ("|");
               Put (From_Key (Symbol.Sub_Sym.Element (K).Name));
            end loop;
         end if;

      end loop;
   end Write_Rule_Text;


   procedure Emit_Destructor_Code
     (SP   : in Symbols.Symbol_Access;
      Lemp : in Lime.Lemon_Record)
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
     (RP   : in Rules.Rule_Access;
      Lemp : in Lime.Lemon_Record)
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
     (Lemp : Lime.Lemon_Record)  --  The main info structure for this parser
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


   procedure Rule_Print (RP : in Rules.Rule_Access)
   is
      --  int i, j;
   begin
      null;
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
   end Rule_Print;


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
     (Lemon        : in Lime.Lemon_Record;
      Token_Prefix : in String;
      First        : in Integer;
      Last         : in Integer)
   is

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
      use Symbols;
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
            Put (From_Key
                   (Element_At
                      (Lemon.Extra,
                       Symbol_Index (I)).Name));
            Put (" ");
            Put_Int (I);
            New_Line;
         end loop;
         Put_Line ("#endif");
      end if;
   end Generate_Tokens;


   procedure Reprint_Of_Grammar
     (Lemon_Lemp    : in out Lime.Lemon_Record;
      Base_Name     : in     String;
      Token_Prefix  : in     String;
      Terminal_Last : in     Natural)
   is
      use Ada.Text_IO;
      use Lime;
   begin
--      if Options.RP_Flag then
--         Reports.Reprint (Lemon_Lemp);
--      else
         Put_Line ("### 2-1");
         --  Initialize the size for all follow and first sets
         Set_Size (Terminal_Last + 1);
         Put_Line ("### 2-2");
         --  Find the precedence for every production rule (that has one)
         Find_Rule_Precedences (Lemon_Lemp);
         Put_Line ("### 2-3");
         --  Compute the lambda-nonterminals and the first-sets for every
         --  nonterminal
         Find_First_Sets (Lemon_Lemp);
         Put_Line ("### 2-4");
         --  Compute all LR(0) states.  Also record follow-set propagation
         --  links so that the follow-set can be computed later
         Compute_LR_States (Lemon_Lemp);
         Put_Line ("### 2-5");
         --         Lemon_Lemp->nstate = 0;
--         FindStates (Lemon_lemp);
--         Lemon_Lemp->sorted = State_arrayof();

         --  Tie up loose ends on the propagation links
         Find_Links (Lemon_Lemp);
         Put_Line ("### 2-6");
         --  Compute the follow set of every reducible configuration
         Find_Follow_Sets (Lemon_Lemp);
         Put_Line ("### 2-7");
         --  Compute the action tables
         Find_Actions (Lemon_Lemp);
         Put_Line ("### 2-8");
         --  Compress the action tables
         if not Options.Compress then
            Reports.Compress_Tables (Lemon_Lemp);
         end if;
         Put_Line ("### 2-9");
         --  Reorder and renumber the states so that states with fewer choices
         --  occur at the end.  This is an optimization that helps make the
         --  generated parser tables smaller.
         if not Options.No_Resort then
            Reports.Resort_States (Lemon_Lemp);
         end if;
         Put_Line ("### 2-10");
         --   Generate a report of the parser generated.  (the "y.output" file)
         if not Options.Be_Quiet then
            Reports.Report_Output (Lemon_Lemp);
         end if;

         --  Generate the source code for the parser
         Reports.Report_Table (Lemon_Lemp);

         --  Produce a header file for use by the scanner.  (This step is
         --  omitted if the "-m" option is used because makeheaders will
         --  generate the file for us.)
         Report_Header
           (Lemon_Lemp,
            Token_Prefix,
            Base_Name, -- File_Makename (Lemon_Lemp, ""),
            "MODULE XXX",
            Terminal_Last);
--      end if;
   end Reprint_Of_Grammar;


end Reports;
