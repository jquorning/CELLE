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

with Lime;
with Database;
with Rules;
with Symbols;
with Parsers;
with Text_Out;
with Action_Tables;

package body Reports is

   procedure Rule_Print_2 (File : in Ada.Text_IO.File_Type;
                           RP   : in Rules.Rule_Access);

   procedure Print_Action
     (AP     : in     Lime.Action_Access;
      File   : in     Ada.Text_IO.File_Type;
      Indent : in     Integer;
      Result :    out Boolean);
  --  Print an action to the given file descriptor.  Return FALSE if
  --  nothing was actually printed.

   procedure Config_Print (File : in Ada.Text_IO.File_Type;
                           CFP  : in Lime.Config_Access);
   --  Print the rule for a configuration.

   function Minimum_Size_Type (LWS     : in     Integer;
                               UPR     : in     Integer;
                               PnBytes :    out Integer)
                              return Interfaces.C.Strings.chars_ptr;


   --  Return the name of a C datatype able to represent values between
   --  lwr and upr, inclusive.  If pnByte!=NULL then also write the sizeof
   --  for that type (1, 2, or 4) into *pnByte.

   function File_Makename (Global    : in Lime.Lemon_Record;
                           Extension : in String) return String;


   procedure Reprint
   is
      use Ada.Text_IO;
      use Interfaces.C.Strings;
      use Symbols;
      use Rules;

      Lemp : Lime.Lemon_Record renames Database.Lime_Lemp; -- Parameter
      RP   : Rules.Rule_Access;
      SP   : Symbol_Access;
      J    : Symbol_Index;
      Max_Len, Len, N_Columns, Skip : Integer;
   begin
      Put_Line ("// Reprint of input file '" & Value (Lemp.File_Name) & "'.");
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


   procedure Report_Output
   is
      use Ada.Text_IO;
      use Symbols;
      use Rules;
      use Lime;
      Lemp : Lemon_Record renames Database.Lime_Lemp;
      File : File_Type;

      Action_Result : Boolean;

      N   : Integer;
      STP : State_Access;
      CFP : Lime.Config_Access;
      AP  : Action_Access;
      RP  : Rule_Access;
   begin
--  fp = file_open(lemp,".out","wb");
      Open (File, Out_File, "XXX.out");

      for I in Symbol_Index range 0 .. Symbol_Index (Lemp.Nx_State) - 1 loop
         STP := Sorted_Element_At (Lemp.Extra, Index => I);
         Put (File, "State ");
         Put (File, Integer'Image (STP.State_Num));
         Put (File, ":");
         New_Line;

         if Lemp.Basis_Flag then
            CFP := STP.BP;
         else
            CFP := STP.CFP;
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

         AP := Action_Access (STP.AP);
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
            if not SP.B_Content then
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


   procedure Report_Table
   is
      use Interfaces.C.Strings;
      use Lime;
      use Rules;
      Lemp : Lemon_Record renames Database.Lime_Lemp;  --  Parameter
--    char line[LINESIZE];
--    struct state *stp;
--    struct action *ap;
--    struct rule *rp;
      Act_Tab : Action_Tables.A_Action_Table;
--    int i, j, n, sz;
      N  : Integer;
      SZ : Integer;
      Size_Of_Action_Type : Integer;
      Size_Of_Code_Type   : Integer;
--    const char *name;
      Mn_Tkn_Ofst, Mx_Tkn_Ofst : Integer;
      Mn_Nt_Ofst,  Mx_Nt_Ofst  : Integer;
--    struct axset *ax;
      Template_Open_Success : Integer;
      Error_Count : Natural;
   begin
      Lemp.Min_Shift_Reduce := Lemp.N_State;
      Lemp.Err_Action       := Lemp.Min_Shift_Reduce + Lemp.N_Rule;
      Lemp.Acc_Action       := Lemp.Err_Action + 1;
      Lemp.No_Action        := Lemp.Acc_Action + 1;
      Lemp.Min_Reduce       := Lemp.No_Action + 1;
      Lemp.Max_Action       := Lemp.Min_Reduce + Lemp.N_Rule;

      Template_Open (Lemon_User_Template, Error_Count, Template_Open_Success);
      Implementation_Open (New_String (File_Makename (Lemp, ".c")));

      Template_Transfer (Lemp.Name);

      --  Generate the include code, if any
      --  Lime_Print (Lemp.Outname, Lemp.No_Linenos_Flag, Lemp.Include);
      Template_Print (Lemp.Out_Name, Boolean'Pos (Lemp.No_Linenos_Flag), Lemp.Include);
      --  lime_print (lime_get_ouüt_name (), lemp->nolinenosflag, lemap->include);
      --  lime_write_include (lime_get_mh_flag(), file_makename(lemp, ".h"));
      Write_Include (New_String (File_Makename (Lemp, ".h")));

      Template_Transfer (Lemp.Name);

      --  Generate #defines for all tokens
--  XXX    Lime_Lemp_Copy := Lemp;
      --  lime_generate_tokens (lime_get_mh_flag(), lemp->tokenprefix, 1, lemp->nterminal);
      Generate_Tokens (Lemp.Token_Prefix, 1, Integer (Lemp.N_Terminal));

      Template_Transfer (Lemp.Name);

      --  Generate the defines
      declare
         use Interfaces.C.Strings;
         use Symbols;
         Code     : constant chars_ptr := Minimum_Size_Type (0, Integer (Lemp.N_Symbol),
                                                             Size_Of_Code_Type);
         Action   : constant chars_ptr := Minimum_Size_Type (0, Lemp.Max_Action,
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
      --  XXX  Print_Stack_Union (Lemp);
      Generate_The_Defines_2 (Lemp.Stack_Size);

--    //if( lime_get_mh_flag() ){
--    //  lime_put_line ("#if INTERFACE");
--    //}
      Write_Interface_Begin;

      declare

         function Get_Name return chars_ptr;

         function Get_Name return chars_ptr is
         begin
            if Lemp.Name /= Null_Ptr then
               return Lemp.Name;
            else
               return New_String ("Parse");
            end if;
         end Get_Name;


         Name : constant String := Value (Get_Name);

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
--
--  Compute the action table, but do not output it yet.  The action
--  table must be computed before generating the YYNSTATE macro because
--  we need to know how many states can be eliminated.
--
--    ax = (struct axset *) calloc(lemp->nxstate*2, sizeof(ax[0]));
--    if( ax==0 ){
--      fprintf(stderr,"malloc failed\n");
--      exit(1);
--    }
--    for(i=0; i<lemp->nxstate; i++){
--      stp = lemp->sorted[i];
--      ax[i*2].stp = stp;
--      ax[i*2].isTkn = 1;
--      ax[i*2].nAction = stp->nTknAct;
--      ax[i*2+1].stp = stp;
--      ax[i*2+1].isTkn = 0;
--      ax[i*2+1].nAction = stp->nNtAct;
--    }
--    mxTknOfst = mnTknOfst = 0;
--    mxNtOfst = mnNtOfst = 0;
--    /* In an effort to minimize the action table size, use the heuristic
--    ** of placing the largest action sets first */
--    for(i=0; i<lemp->nxstate*2; i++) ax[i].iOrder = i;
--    qsort(ax, lemp->nxstate*2, sizeof(ax[0]), axset_compare);
--    pActtab = acttab_alloc(lemp->nsymbol, lemp->nterminal);
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

      Template_Transfer (Lemp.Name);


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
         package Acttab renames Action_Tables;
      begin

         --
         --  Output the yy_action table
         --
         Lemp.N_Action_Tab := Acttab.Action_Size (Act_Tab.all);
         N := Lemp.N_Action_Tab;
         Lemp.Table_Size := Lemp.Table_Size + N * Size_Of_Action_Type;

         --  lime_apActtab = pActtab;
         Write_Action_Table (N, Lemp.No_Action);

         --
         --  Output the yy_lookahead table
         --
         Lemp.N_Lookahead_Tab := Acttab.Lookahead_Size (Act_Tab.all);
         N := Lemp.N_Lookahead_Tab;
         Lemp.Table_Size := Lemp.Table_Size + N * Size_Of_Code_Type;
      end;

      --      pActtab := pActtab;
      Write_YY_Lookahead (N, Integer (Lemp.N_Symbol));

      --
      --  Output the yy_shift_ofst[] table
      --

      N := Lemp.Nx_State;
--    while( n>0 && lemp->sorted[n-1]->iTknOfst==NO_OFFSET ) n--;
--
--    lime_lemp = lemp;
      Write_YY_Shift_Offsets
        (N,
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
--    lime_lemp = lemp;
      Write_YY_Reduce_Offsets
        (N,
         Mn_Nt_Ofst,
         Mx_Nt_Ofst,
         Minimum_Size_Type (Mn_Nt_Ofst - 1, Mx_Nt_Ofst, SZ),
         No_Offset);
      Lemp.Table_Size := Lemp.Table_Size + N * SZ;

      --
      --  Output the default action table
      --

      Write_Default_Action_Table
        (Lemp.Nx_State,
         Lemp.Err_Action,
         Lemp.Min_Reduce);
      Lemp.Table_Size := Lemp.Table_Size + N * Size_Of_Action_Type;

      Template_Transfer (Lemp.Name);

      --
      --  Generate the table of fallback tokens.
      --
      if Lemp.Has_Fallback then
         declare
            use Ada.Strings.Unbounded;
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
                  P : Symbol_Access := Element_At (Lemp.Extra, I);
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

      Template_Transfer (Lemp.Name);

--    /* Generate A Table Containing the symbolic name of every symbol
--    */
--    for(i=0; i<lemp->nsymbol; i++){
--      lemon_sprintf(line,"\"%s\",",lemp->symbols[i]->name);
--      lime_put ("  /* "); //%4d */ \"%s\",\n",i, lemp->symbols[i]->name); lineno++;
--      lime_put_int (i);
--      lime_put (" */ """); // %s\",\n",i, lemp->symbols[i]->name); lineno++;
--      lime_put (lemp->symbols[i]->name);  //
--      lime_put_line (""",");
--      //fprintf(out,"  /* %4d */ \"%s\",\n",i, lemp->symbols[i]->name); lineno++;
--    }
--    lime_template_transfer (lemp->name);
--
--    /* Generate a table containing a text string that describes every
--    ** rule in the rule set of the grammar.  This information is used
--    ** when tracing REDUCE actions.
--    */
--    for(i=0, rp=lemp->rule; rp; rp=rp->next, i++){
--      assert( rp->iRule==i );
--      //fprintf(out," /* %3d */ \"", i);
--      lime_put (" /* ");
--      lime_put_int (i);
--      lime_put (" */ """);
--      writeRuleText (rp);
--      //fprintf(out,"\",\n"); lineno++;
--      lime_put_line (""",");
--    }
--    lime_template_transfer (lemp->name);
--

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
--      if( i<lemp->nsymbol ){
--        emit_destructor_code (lemp->symbols[i], lemp);
--        lime_put_line ("      break;");
--      }
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
--      rule_print (rp);
--      lime_put_line (" */");
--    }
--
      Template_Transfer (Lemp.Name);
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
      Template_Transfer (Lemp.Name);
--
--    /* Generate code which execution during each REDUCE action */
--    i = 0;
--    for(rp=lemp->rule; rp; rp=rp->next){
--      i += translate_code(lemp, rp);
--    }
--    if( i ){
--      lime_put_line ("        YYMINORTYPE yylhsminor;");
--    }
--
--    /* First output rules other than the default: rule */
--    for(rp=lemp->rule; rp; rp=rp->next){
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
--      emit_code (rp, lemp);
--      lime_put_line ("        break;");
--      rp->codeEmitted = 1;
--    }
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


   procedure Compress_Tables
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


   procedure Resort_States
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
     (AP     : in     Lime.Action_Access;
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
                           CFP  : in Lime.Config_Access)
   is
   begin
      null;
      --  RulePrint(fp, cfp->rp, cfp->dot);
   end Config_Print;


   function Minimum_Size_Type (LWS     : in     Integer;
                               UPR     : in     Integer;
                               PnBytes :    out Integer)
                              return Interfaces.C.Strings.chars_ptr
   is
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
      return Interfaces.C.Strings.Null_Ptr;
   end Minimum_Size_Type;


   function File_Makename (Global    : in Lime.Lemon_Record;
                           Extension : in String) return String
   is
      use Ada.Strings;
      use Interfaces.C.Strings;
      File_Name    : constant String  := Value (Global.File_Name);
      Dot_Position : constant Natural := Fixed.Index (File_Name, ".", Backward);
   begin
      if Dot_Position = 0 then
         return File_Name & Extension;
      else
         return File_Name (File_Name'First .. Dot_Position) & Extension;
      end if;
   end File_Makename;


end Reports;
