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

with Auxiliary.Text.IO;
with Auxiliary.Text.Utility;
with Auxiliary.Errors;

with Rules;

package body Parser is

   --
   --  The state of the parser
   --
   type E_State is
     (INITIALIZE,
      WAITING_FOR_DECL_OR_RULE,
      WAITING_FOR_DECL_KEYWORD,
      WAITING_FOR_DECL_ARG,
      WAITING_FOR_PRECEDENCE_SYMBOL,
      WAITING_FOR_ARROW,
      IN_RHS,
      LHS_ALIAS_1,
      LHS_ALIAS_2,
      LHS_ALIAS_3,
      RHS_ALIAS_1,
      RHS_ALIAS_2,
      PRECEDENCE_MARK_1,
      PRECEDENCE_MARK_2,
      RESYNC_AFTER_RULE_ERROR,
      RESYNC_AFTER_DECL_ERROR,
      WAITING_FOR_DESTRUCTOR_SYMBOL,
      WAITING_FOR_DATATYPE_SYMBOL,
      WAITING_FOR_FALLBACK_ID,
      WAITING_FOR_WILDCARD_ID,
      WAITING_FOR_CLASS_ID,
      WAITING_FOR_CLASS_TOKEN,
      WAITING_FOR_TOKEN_NAME);

   use Ada.Strings.Unbounded;
   type Pstate_Record is
      record
         File_Name    : Unbounded_String; --  Name of the input file
         Token_Lineno : Natural;          --  Linenumber at which current token starts
         Error_Count  : Natural;          --  Number of errors so far
         Token_Start  : Natural;          --  Text of current token
         GP : access Lime.Lemon_Record;   --  Global state vector
         State : E_State;        --  The state of the parser
--    struct symbol *fallback;   --  The fallback token
--    struct symbol *tkclass;    --  Token class symbol
--    struct symbol *lhs;        --  Left-hand side of current rule
--    const char *lhsalias;      --  Alias for the LHS
--    int nrhs;                  --  Number of right-hand side symbols seen
--    struct symbol *rhs[MAXRHS];  --  RHS symbols
--    const char *alias[MAXRHS]; --  Aliases for each RHS symbol (or NULL)
         Prev_Rule : access Rules.Rule_Record;     --  Previous rule parsed
--    const char *declkeyword;   --  Keyword of a declaration
--    char **declargslot;        --  Where the declaration argument should be put
--    int insertLineMacro;       --  Add #line before declaration insert
--    int *decllinenoslot;       --  Where to write declaration line number
--    enum e_assoc declassoc;    --  Assign this association to decl arguments
         Prec_Counter : Integer;           --  Assign this precedence to decl arguments
         First_Rule   : access Rules.Rule_Record; --  Pointer to first rule in the grammar
         Last_Rule    : access Rules.Rule_Record; --  Pointer to the most recently parsed rule
      end record;

   procedure Preprocess_Input (File_Name : in     String;
                               Success   :    out Boolean);
   --  Run the preprocessor over the input file text.  The global
   --  variables azDefine[0] through azDefine[nDefine-1] contains the
   --  names of all defined macros.  This routine looks for "%ifdef"
   --  and "%ifndef" and "%endif" and comments them out.  Text in
   --  between is also commented out as appropriate.

   procedure Parse_One_Token (PSP : in out Pstate_Record);
   --  Parse a single Token.

   procedure Preprocess_Input (File_Name : in     String;
                               Success   :    out Boolean)
   is
      use Ada.Text_IO;
--        i, j, k, N : integer;
--        exclude : Integer := 0;
--        start : Integer := 0;
--        lineno : Integer := 1;
      Start_Lineno : Natural := 1;
--     begin
--        for i=0; z[i]; i++ loop
--        if z[i]=='\n' then lineno++; end if;
--        if z[i]!='%' or (i>0 && z[i-1]!='\n') then continue; end if;
--        if strncmp(&z[i],"%endif",6)==0 and ISSPACE(z[i+6]) then
--           if exclude then
--              Exclude := Exclude - 1;
--              if Exclude = 0 then
--                 for j=start; j<i; j++ loop if z[j]!='\n' then z[j] = ' ';
--                 end if; end loop;
--              end if;
--           end if;
--           for j=i; z[j] && z[j]!='\n'; j++ then z[j] = ' '; end if;
--        elsif (strncmp(&z[i],"%ifdef",6)==0 and ISSPACE(z[i+6]))
--          or (strncmp(&z[i],"%ifndef",7)==0 and ISSPACE(z[i+7]))
--        then
--           if exclude then
--              exclude++;
--           else
--              for j=i+7; ISSPACE(z[j]); j++ loop null; end loop;
--              for n=0; z[j+n] && !ISSPACE(z[j+n]); n++ loop null; end loop;
--              exclude := 1;
--              for k=0; k<nDefine; k++ loop
--              if strncmp(azDefine[k],&z[j],n)==0 and lemonStrlen(azDefine[k])==n then
--                 exclude := 0;
--                 exit;
--              end if;
--              end loop;
--              if z[i+3]=='n' then exclude = !exclude; end if;
--              if exclude then
--                 start := i;
--                 start_lineno := lineno;
--              end if;
--           end if;
--           for j=i; z[j] && z[j]!='\n'; j++ loop z[j] = ' '; end loop;
--        end if;
--     end loop;
--        if Exclude /= 0 then
--           Error ("XXX", Start_lineno, "unterminated %%ifdef starting on line.");
--           Success := False;
--        end if;
      Ifdef  : constant String := "%ifdef";
      Ifndef : constant String := "%ifndef";
      Endif  : constant String := "%endif";
      File : File_Type;
      Exclude : Natural := 0;
   begin
      Open (File, In_File, File_Name);
      loop
         declare
            Line : constant String := Get_Line (File);
         begin
            --  Skip comments
            --  Endif
            --  ifdef or ifndef

            null;
         exception when End_Error => exit;
         end;
      end loop;
      Close (File);
      Success := True;

      if Exclude /= 0 then
         Auxiliary.Errors.Error
           (File_Name, Start_Lineno,
            "unterminated %%ifdef starting on line.");
         Success := False;
      end if;
   end Preprocess_Input;


   procedure Parse (GP : access Lime.Lemon_Record)
   is
      use Ada.Text_IO;
      CPP_Comment     : constant String := "//";
      Comment_C_Begin : constant String := "/*";
      Comment_C_End   : constant String := "*/";

      File : File_Type;

      function Get_Line return String;
      function Get_Line return String is
         use Auxiliary.Text;
         Line : constant String := Get_Line (File);
         Last : Natural;
      begin
         Utility.Strip_End_Of_Line (From  => Line,
                                    Strip => "//",
                                    Last  => Last);
         return Line (Line'First .. Last);
      end Get_Line;

      PS : Pstate_Record;
      Start_Line : Integer := 0;

      use Ada.Strings.Unbounded;
      use Ada.Strings.Fixed;
      use Auxiliary.Text;
      Unbounded : Unbounded_String;
      First : Natural;
      Last  : Natural;
   begin
      --  memset(&ps, '\0', sizeof(ps));
      PS.GP          := GP;
      PS.File_Name   :=
        To_Unbounded_String (Interfaces.C.Strings.Value (GP.File_Name));
      PS.Error_Count := 0;
      PS.State       := INITIALIZE;

      --  Begin by opening the input file
      Open (File, In_File, To_String (PS.File_Name));

      --  Make an initial pass through the file to handle %ifdef and %ifndef.
      --  Preprocess_Input (filebuf);

      --  Now scan the text of the input file.
      loop
         declare
            Line_No_CPP     : constant String  := Get_Line;
            Comment_C_Start : constant Natural :=
              Index (Line_No_CPP, Comment_C_Begin);
         begin
            First := Line_No_CPP'First;
            Last  := Line_No_CPP'Last;

            if Comment_C_Start /= 0 then
               loop
                  declare
                     Line_No_C : constant String := Get_Line;
                     Pos_Stop : Natural;
                  begin
                     Pos_Stop := Index (Line_No_C, Comment_C_End);
                     if Pos_Stop /= 0 then
                        Unbounded := To_Unbounded_String
                          (Line_No_C (Line_No_C'First .. Pos_Stop +
                                        Comment_C_End'Length - 1));
                     end if;
                  end;
               end loop;
            else
               First := Line_No_CPP'First;
               Unbounded := To_Unbounded_String (Line_No_CPP);
            end if;
            First := Line_No_CPP'First;
            Auxiliary.Text.Trim (Line_No_CPP, First, Last, Side => Ada.Strings.Left);
--            while Is_Space (Line (Pos)) loop
--               Pos := Pos + 1;
--            end loop;
         end;
            --  for(cp=filebuf; (c= *cp)!=0; ){
--    if( c=='\n' ) lineno++;              /* Keep track of the line number */
--    if( ISSPACE(c) ){ cp++; continue; }  /* Skip all white space */
--      if( c=='/' && cp[1]=='/' ){          /* Skip C++ style comments */
--        cp+=2;
--        while( (c= *cp)!=0 && c!='\n' ) cp++;
--        continue;
--      }
--      if( c=='/' && cp[1]=='*' ){          /* Skip C style comments */
--        cp+=2;
--        while( (c= *cp)!=0 && (c!='/' || cp[-1]!='*') ){
--          if( c=='\n' ) lineno++;
--          cp++;
--        }
--        if( c ) cp++;
--        continue;
--      }
         PS.Token_Start  := First;            --  Mark the beginning of the token
         PS.Token_Lineno := IO.Line_Number;   --  Linenumber on which token begins

--      if( c=='\"' ){                     /* String literals */
--        cp++;
--        while( (c= *cp)!=0 && c!='\"' ){
--          if( c=='\n' ) lineno++;
--          cp++;
--        }
--        if( c==0 ){
--          ErrorMsg(ps.filename,startline,
--  "String starting on this line is not terminated before the end of the file.");
--          ps.errorcnt++;
--          nextcp = cp;
--        }else{
--          nextcp = cp+1;
--        }
--      }else if( c=='{' ){               /* A block of C code */
--        int level;
--        cp++;
--        for(level=1; (c= *cp)!=0 && (level>1 || c!='}'); cp++){
--          if( c=='\n' ) lineno++;
--          else if( c=='{' ) level++;
--          else if( c=='}' ) level--;
--          else if( c=='/' && cp[1]=='*' ){  /* Skip comments */
--            int prevc;
--            cp = &cp[2];
--            prevc = 0;
--            while( (c= *cp)!=0 && (c!='/' || prevc!='*') ){
--              if( c=='\n' ) lineno++;
--              prevc = c;
--              cp++;
--            }
--          }else if( c=='/' && cp[1]=='/' ){  /* Skip C++ style comments too */
--            cp = &cp[2];
--            while( (c= *cp)!=0 && c!='\n' ) cp++;
--            if( c ) lineno++;
--          }else if( c=='\'' || c=='\"' ){    /* String a character literals */
--            int startchar, prevc;
--            startchar = c;
--            prevc = 0;
--            for(cp++; (c= *cp)!=0 && (c!=startchar || prevc=='\\'); cp++){
--              if( c=='\n' ) lineno++;
--              if( prevc=='\\' ) prevc = 0;
--              else              prevc = c;
--            }
--          }
--        }
--        if( c==0 ){
--          ErrorMsg(ps.filename,ps.tokenlineno,
--  "C code starting on this line is not terminated before the end of the file.");
--          ps.errorcnt++;
--          nextcp = cp;
--        }else{
--          nextcp = cp+1;
--        }
--      }else if( ISALNUM(c) ){          /* Identifiers */
--        while( (c= *cp)!=0 && (ISALNUM(c) || c=='_') ) cp++;
--        nextcp = cp;
--      }else if( c==':' && cp[1]==':' && cp[2]=='=' ){ /* The operator "::=" */
--        cp += 3;
--        nextcp = cp;
--      }else if( (c=='/' || c=='|') && ISALPHA(cp[1]) ){
--        cp += 2;
--        while( (c = *cp)!=0 && (ISALNUM(c) || c=='_') ) cp++;
--        nextcp = cp;
--      }else{                          /* All other (one character) operators */
--        cp++;
--        nextcp = cp;
--      }
--      c = *cp;
--      *cp = 0;                        /* Null terminate the token */
--      Parse_One_Token (&ps);             --  Parse the token
--    *cp = (char)c;                  /* Restore the buffer */
--    cp = nextcp;
      end loop;

   exception

      when End_Error =>
         Close (File);
         GP.Rule      := PS.First_Rule;
         GP.Error_Cnt := PS.Error_Count;

      when others =>
         Auxiliary.Errors.Error
           (To_String (PS.File_Name), 0, "Can't open this file for reading.");
         GP.Error_Cnt := GP.Error_Cnt + 1;

   end Parse;

   procedure Parse_One_Token (PSP : in out Pstate_Record)
   is
      procedure Do_Initialize;

      procedure Do_Initialize is
      begin
         PSP.Prev_Rule    := null;
         PSP.Prec_Counter := 0;
         PSP.First_Rule   := null;
         PSP.Last_Rule    := null;
         PSP.GP.N_Rule    := 0;
      end Do_Initialize;

   begin

--  {
--    const char *x;
--    x = Strsafe(psp->tokenstart);     /* Save the token permanently */
--  #if 0
--    printf("%s:%d: Token=[%s] state=%d\n",psp->filename,psp->tokenlineno,
--      x,psp->state);
--  #endif
      case PSP.State is

         when INITIALIZE =>
            Do_Initialize;

         when WAITING_FOR_DECL_OR_RULE =>
            Do_Initialize;
--        if( x[0]=='%' ){
--          psp->state = WAITING_FOR_DECL_KEYWORD;
--        }else if( ISLOWER(x[0]) ){
--          psp->lhs = lime_symbol_new(x);
--          psp->nrhs = 0;
--          psp->lhsalias = 0;
--          psp->state = WAITING_FOR_ARROW;
--        }else if( x[0]=='{' ){
--          if( psp->prevrule==0 ){
--            ErrorMsg(psp->filename,psp->tokenlineno,
--  "There is no prior rule upon which to attach the code \
--  fragment which begins on this line.");
--            psp->errorcnt++;
--          }else if( psp->prevrule->code!=0 ){
--            ErrorMsg(psp->filename,psp->tokenlineno,
--  "Code fragment beginning on this line is not the first \
--  to follow the previous rule.");
--            psp->errorcnt++;
--          }else{
--            psp->prevrule->line = psp->tokenlineno;
--            psp->prevrule->code = &x[1];
--            psp->prevrule->noCode = 0;
--          }
--        }else if( x[0]=='[' ){
--          psp->state = PRECEDENCE_MARK_1;
--        }else{
--          ErrorMsg(psp->filename,psp->tokenlineno,
--            "Token \"%s\" should be either \"%%\" or a nonterminal name.",
--            x);
--          psp->errorcnt++;
--        }
            null;

         when PRECEDENCE_MARK_1 =>
--        if( !ISUPPER(x[0]) ){
--          ErrorMsg(psp->filename,psp->tokenlineno,
--            "The precedence symbol must be a terminal.");
--          psp->errorcnt++;
--        }else if( psp->prevrule==0 ){
--          ErrorMsg(psp->filename,psp->tokenlineno,
--            "There is no prior rule to assign precedence \"[%s]\".",x);
--          psp->errorcnt++;
--        }else if( psp->prevrule->precsym!=0 ){
--          ErrorMsg(psp->filename,psp->tokenlineno,
--  "Precedence mark on this line is not the first \
--  to follow the previous rule.");
--          psp->errorcnt++;
--        }else{
--          psp->prevrule->precsym = lime_symbol_new(x);
--        }
--        psp->state = PRECEDENCE_MARK_2;
            null;

         when PRECEDENCE_MARK_2 =>
--        if( x[0]!=']' ){
--          ErrorMsg(psp->filename,psp->tokenlineno,
--            "Missing \"]\" on precedence mark.");
--          psp->errorcnt++;
--        }
--        psp->state = WAITING_FOR_DECL_OR_RULE;
            null;

         when WAITING_FOR_ARROW =>
--        if( x[0]==':' && x[1]==':' && x[2]=='=' ){
--          psp->state = IN_RHS;
--        }else if( x[0]=='(' ){
--          psp->state = LHS_ALIAS_1;
--        }else{
--          ErrorMsg(psp->filename,psp->tokenlineno,
--            "Expected to see a \":\" following the LHS symbol \"%s\".",
--            psp->lhs->name);
--          psp->errorcnt++;
--          psp->state = RESYNC_AFTER_RULE_ERROR;
--        }
            null;

         when LHS_ALIAS_1 =>
--        if( ISALPHA(x[0]) ){
--          psp->lhsalias = x;
--          psp->state = LHS_ALIAS_2;
--        }else{
--          ErrorMsg(psp->filename,psp->tokenlineno,
--            "\"%s\" is not a valid alias for the LHS \"%s\"\n",
--            x,psp->lhs->name);
--          psp->errorcnt++;
--          psp->state = RESYNC_AFTER_RULE_ERROR;
--        }
            null;

         when LHS_ALIAS_2 =>
--        if( x[0]==')' ){
--          psp->state = LHS_ALIAS_3;
--        }else{
--          ErrorMsg(psp->filename,psp->tokenlineno,
--            "Missing \")\" following LHS alias name \"%s\".",psp->lhsalias);
--          psp->errorcnt++;
--          psp->state = RESYNC_AFTER_RULE_ERROR;
--        }
            null;

         when LHS_ALIAS_3 =>
--        if( x[0]==':' && x[1]==':' && x[2]=='=' ){
--          psp->state = IN_RHS;
--        }else{
--          ErrorMsg(psp->filename,psp->tokenlineno,
--            "Missing \"->\" following: \"%s(%s)\".",
--             psp->lhs->name,psp->lhsalias);
--          psp->errorcnt++;
--          psp->state = RESYNC_AFTER_RULE_ERROR;
--        }
            null;
         when IN_RHS =>
--        if( x[0]=='.' ){
--          struct rule *rp;
--          rp = (struct rule *)calloc( sizeof(struct rule) +
--               sizeof(struct symbol*)*psp->nrhs + sizeof(char*)*psp->nrhs, 1);
--          if( rp==0 ){
--            ErrorMsg(psp->filename,psp->tokenlineno,
--              "Can't allocate enough memory for this rule.");
--            psp->errorcnt++;
--            psp->prevrule = 0;
--          }else{
--            int i;
--            rp->ruleline = psp->tokenlineno;
--            rp->rhs = (struct symbol**)&rp[1];
--            rp->rhsalias = (const char**)&(rp->rhs[psp->nrhs]);
--            for(i=0; i<psp->nrhs; i++){
--              rp->rhs[i] = psp->rhs[i];
--              rp->rhsalias[i] = psp->alias[i];
--              if( rp->rhsalias[i]!=0 ){ rp->rhs[i]->bContent = 1; }
--            }
--            rp->lhs = psp->lhs;
--            rp->lhsalias = psp->lhsalias;
--            rp->nrhs = psp->nrhs;
--            rp->code = 0;
--            rp->noCode = 1;
--            rp->precsym = 0;
--            rp->index = psp->gp->nrule++;
--            rp->nextlhs = rp->lhs->rule;
--            rp->lhs->rule = rp;
--            rp->next = 0;
--            if( psp->firstrule==0 ){
--              psp->firstrule = psp->lastrule = rp;
--            }else{
--              psp->lastrule->next = rp;
--              psp->lastrule = rp;
--            }
--            psp->prevrule = rp;
--          }
--          psp->state = WAITING_FOR_DECL_OR_RULE;
--        }else if( ISALPHA(x[0]) ){
--          if( psp->nrhs>=MAXRHS ){
--            ErrorMsg(psp->filename,psp->tokenlineno,
--              "Too many symbols on RHS of rule beginning at \"%s\".",
--              x);
--            psp->errorcnt++;
--            psp->state = RESYNC_AFTER_RULE_ERROR;
--          }else{
--            psp->rhs[psp->nrhs] = lime_symbol_new(x);
--            psp->alias[psp->nrhs] = 0;
--            psp->nrhs++;
--          }
--        }else if( (x[0]=='|' || x[0]=='/') && psp->nrhs>0 ){
--          struct symbol *msp = psp->rhs[psp->nrhs-1];
--          if( msp->type!=MULTITERMINAL ){
--            struct symbol *origsp = msp;
--            msp = (struct symbol *) calloc(1,sizeof(*msp));
--            memset(msp, 0, sizeof(*msp));
--            msp->type = MULTITERMINAL;
--            msp->nsubsym = 1;
--            msp->subsym = (struct symbol **) calloc(1,sizeof(struct symbol*));
--            msp->subsym[0] = origsp;
--            msp->name = origsp->name;
--            psp->rhs[psp->nrhs-1] = msp;
--          }
--          msp->nsubsym++;
--          msp->subsym = (struct symbol **) realloc(msp->subsym,
--            sizeof(struct symbol*)*msp->nsubsym);
--          msp->subsym[msp->nsubsym-1] = lime_symbol_new(&x[1]);
--          if( ISLOWER(x[1]) || ISLOWER(msp->subsym[0]->name[0]) ){
--            ErrorMsg(psp->filename,psp->tokenlineno,
--              "Cannot form a compound containing a non-terminal");
--            psp->errorcnt++;
--          }
--        }else if( x[0]=='(' && psp->nrhs>0 ){
--          psp->state = RHS_ALIAS_1;
--        }else{
--          ErrorMsg(psp->filename,psp->tokenlineno,
--            "Illegal character on RHS of rule: \"%s\".",x);
--          psp->errorcnt++;
--          psp->state = RESYNC_AFTER_RULE_ERROR;
--        }
            null;
         when RHS_ALIAS_1 =>
--        if( ISALPHA(x[0]) ){
--          psp->alias[psp->nrhs-1] = x;
--          psp->state = RHS_ALIAS_2;
--        }else{
--          ErrorMsg(psp->filename,psp->tokenlineno,
--            "\"%s\" is not a valid alias for the RHS symbol \"%s\"\n",
--            x,psp->rhs[psp->nrhs-1]->name);
--          psp->errorcnt++;
--          psp->state = RESYNC_AFTER_RULE_ERROR;
--        }
            null;
         when RHS_ALIAS_2 =>
--        if( x[0]==')' ){
--          psp->state = IN_RHS;
--        }else{
--          ErrorMsg(psp->filename,psp->tokenlineno,
--            "Missing \")\" following LHS alias name \"%s\".",psp->lhsalias);
--          psp->errorcnt++;
--          psp->state = RESYNC_AFTER_RULE_ERROR;
--        }
            null;
         when WAITING_FOR_DECL_KEYWORD =>
--        if( ISALPHA(x[0]) ){
--          psp->declkeyword = x;
--          psp->declargslot = 0;
--          psp->decllinenoslot = 0;
--          psp->insertLineMacro = 1;
--          psp->state = WAITING_FOR_DECL_ARG;
--          if( strcmp(x,"name")==0 ){
--            psp->declargslot = &(psp->gp->name);
--            psp->insertLineMacro = 0;
--          }else if( strcmp(x,"include")==0 ){
--            psp->declargslot = &(psp->gp->include);
--          }else if( strcmp(x,"code")==0 ){
--            psp->declargslot = &(psp->gp->extracode);
--          }else if( strcmp(x,"token_destructor")==0 ){
--            psp->declargslot = &psp->gp->tokendest;
--          }else if( strcmp(x,"default_destructor")==0 ){
--            psp->declargslot = &psp->gp->vardest;
--          }else if( strcmp(x,"token_prefix")==0 ){
--            psp->declargslot = &psp->gp->tokenprefix;
--            psp->insertLineMacro = 0;
--          }else if( strcmp(x,"syntax_error")==0 ){
--            psp->declargslot = &(psp->gp->error);
--          }else if( strcmp(x,"parse_accept")==0 ){
--            psp->declargslot = &(psp->gp->accept);
--          }else if( strcmp(x,"parse_failure")==0 ){
--            psp->declargslot = &(psp->gp->failure);
--          }else if( strcmp(x,"stack_overflow")==0 ){
--            psp->declargslot = &(psp->gp->overflow);
--          }else if( strcmp(x,"extra_argument")==0 ){
--            psp->declargslot = &(psp->gp->arg);
--            psp->insertLineMacro = 0;
--          }else if( strcmp(x,"extra_context")==0 ){
--            psp->declargslot = &(psp->gp->ctx);
--            psp->insertLineMacro = 0;
--          }else if( strcmp(x,"token_type")==0 ){
--            psp->declargslot = &(psp->gp->tokentype);
--            psp->insertLineMacro = 0;
--          }else if( strcmp(x,"default_type")==0 ){
--            psp->declargslot = &(psp->gp->vartype);
--            psp->insertLineMacro = 0;
--          }else if( strcmp(x,"stack_size")==0 ){
--            psp->declargslot = &(psp->gp->stacksize);
--            psp->insertLineMacro = 0;
--          }else if( strcmp(x,"start_symbol")==0 ){
--            psp->declargslot = &(psp->gp->start);
--            psp->insertLineMacro = 0;
--          }else if( strcmp(x,"left")==0 ){
--            psp->preccounter++;
--            psp->declassoc = LEFT;
--            psp->state = WAITING_FOR_PRECEDENCE_SYMBOL;
--          }else if( strcmp(x,"right")==0 ){
--            psp->preccounter++;
--            psp->declassoc = RIGHT;
--            psp->state = WAITING_FOR_PRECEDENCE_SYMBOL;
--          }else if( strcmp(x,"nonassoc")==0 ){
--            psp->preccounter++;
--            psp->declassoc = NONE;
--            psp->state = WAITING_FOR_PRECEDENCE_SYMBOL;
--          }else if( strcmp(x,"destructor")==0 ){
--            psp->state = WAITING_FOR_DESTRUCTOR_SYMBOL;
--          }else if( strcmp(x,"type")==0 ){
--            psp->state = WAITING_FOR_DATATYPE_SYMBOL;
--          }else if( strcmp(x,"fallback")==0 ){
--            psp->fallback = 0;
--            psp->state = WAITING_FOR_FALLBACK_ID;
--          }else if( strcmp(x,"token")==0 ){
--            psp->state = WAITING_FOR_TOKEN_NAME;
--          }else if( strcmp(x,"wildcard")==0 ){
--            psp->state = WAITING_FOR_WILDCARD_ID;
--          }else if( strcmp(x,"token_class")==0 ){
--            psp->state = WAITING_FOR_CLASS_ID;
--          }else{
--            ErrorMsg(psp->filename,psp->tokenlineno,
--              "Unknown declaration keyword: \"%%%s\".",x);
--            psp->errorcnt++;
--            psp->state = RESYNC_AFTER_DECL_ERROR;
--          }
--        }else{
--          ErrorMsg(psp->filename,psp->tokenlineno,
--            "Illegal declaration keyword: \"%s\".",x);
--          psp->errorcnt++;
--          psp->state = RESYNC_AFTER_DECL_ERROR;
--        }
            null;

         when WAITING_FOR_DESTRUCTOR_SYMBOL =>
--        if( !ISALPHA(x[0]) ){
--          ErrorMsg(psp->filename,psp->tokenlineno,
--            "Symbol name missing after %%destructor keyword");
--          psp->errorcnt++;
--          psp->state = RESYNC_AFTER_DECL_ERROR;
--        }else{
--          struct symbol *sp = lime_symbol_new(x);
--          psp->declargslot = &sp->destructor;
--          psp->decllinenoslot = &sp->destLineno;
--          psp->insertLineMacro = 1;
--          psp->state = WAITING_FOR_DECL_ARG;
--        }
            null;
         when WAITING_FOR_DATATYPE_SYMBOL =>
--        if( !ISALPHA(x[0]) ){
--          ErrorMsg(psp->filename,psp->tokenlineno,
--            "Symbol name missing after %%type keyword");
--          psp->errorcnt++;
--          psp->state = RESYNC_AFTER_DECL_ERROR;
--        }else{
--          struct symbol *sp = lime_symbol_find(x);
--          if((sp) && (sp->datatype)){
--            ErrorMsg(psp->filename,psp->tokenlineno,
--              "Symbol %%type \"%s\" already defined", x);
--            psp->errorcnt++;
--            psp->state = RESYNC_AFTER_DECL_ERROR;
--          }else{
--            if (!sp){
--              sp = lime_symbol_new(x);
--            }
--            psp->declargslot = &sp->datatype;
--            psp->insertLineMacro = 0;
--            psp->state = WAITING_FOR_DECL_ARG;
--          }
--        }
            null;
         when WAITING_FOR_PRECEDENCE_SYMBOL =>
--        if( x[0]=='.' ){
--          psp->state = WAITING_FOR_DECL_OR_RULE;
--        }else if( ISUPPER(x[0]) ){
--          struct symbol *sp;
--          sp = lime_symbol_new(x);
--          if( sp->prec>=0 ){
--            ErrorMsg(psp->filename,psp->tokenlineno,
--              "Symbol \"%s\" has already be given a precedence.",x);
--            psp->errorcnt++;
--          }else{
--            sp->prec = psp->preccounter;
--            sp->assoc = psp->declassoc;
--          }
--        }else{
--          ErrorMsg(psp->filename,psp->tokenlineno,
--            "Can't assign a precedence to \"%s\".",x);
--          psp->errorcnt++;
--        }
            null;
         when WAITING_FOR_DECL_ARG =>
--        if( x[0]=='{' || x[0]=='\"' || ISALNUM(x[0]) ){
--          const char *zOld, *zNew;
--          char *zBuf, *z;
--          int nOld, n, nLine = 0, nNew, nBack;
--          int addLineMacro;
--          char zLine[50];
--          zNew = x;
--          if( zNew[0]=='"' || zNew[0]=='{' ) zNew++;
--          nNew = lemonStrlen(zNew);
--          if( *psp->declargslot ){
--            zOld = *psp->declargslot;
--          }else{
--            zOld = "";
--          }
--          nOld = lemonStrlen(zOld);
--          n = nOld + nNew + 20;
--          addLineMacro = !psp->gp->nolinenosflag && psp->insertLineMacro &&
--                          (psp->decllinenoslot==0 || psp->decllinenoslot[0]!=0);
--          if( addLineMacro ){
--            for(z=psp->filename, nBack=0; *z; z++){
--              if( *z=='\\' ) nBack++;
--            }
--            lemon_sprintf(zLine, "#line %d ", psp->tokenlineno);
--            nLine = lemonStrlen(zLine);
--            n += nLine + lemonStrlen(psp->filename) + nBack;
--          }
--          *psp->declargslot = (char *) realloc(*psp->declargslot, n);
--          zBuf = *psp->declargslot + nOld;
--          if( addLineMacro ){
--            if( nOld && zBuf[-1]!='\n' ){
--              *(zBuf++) = '\n';
--            }
--            memcpy(zBuf, zLine, nLine);
--            zBuf += nLine;
--            *(zBuf++) = '"';
--            for(z=psp->filename; *z; z++){
--              if( *z=='\\' ){
--                *(zBuf++) = '\\';
--              }
--              *(zBuf++) = *z;
--            }
--            *(zBuf++) = '"';
--            *(zBuf++) = '\n';
--          }
--          if( psp->decllinenoslot && psp->decllinenoslot[0]==0 ){
--            psp->decllinenoslot[0] = psp->tokenlineno;
--          }
--          memcpy(zBuf, zNew, nNew);
--          zBuf += nNew;
--          *zBuf = 0;
--          psp->state = WAITING_FOR_DECL_OR_RULE;
--        }else{
--          ErrorMsg(psp->filename,psp->tokenlineno,
--            "Illegal argument to %%%s: %s",psp->declkeyword,x);
--          psp->errorcnt++;
--          psp->state = RESYNC_AFTER_DECL_ERROR;
--        }
            null;
         when WAITING_FOR_FALLBACK_ID =>
--        if( x[0]=='.' ){
--          psp->state = WAITING_FOR_DECL_OR_RULE;
--        }else if( !ISUPPER(x[0]) ){
--          ErrorMsg(psp->filename, psp->tokenlineno,
--            "%%fallback argument \"%s\" should be a token", x);
--          psp->errorcnt++;
--        }else{
--          struct symbol *sp = lime_symbol_new(x);
--          if( psp->fallback==0 ){
--            psp->fallback = sp;
--          }else if( sp->fallback ){
--            ErrorMsg(psp->filename, psp->tokenlineno,
--              "More than one fallback assigned to token %s", x);
--            psp->errorcnt++;
--          }else{
--            sp->fallback = psp->fallback;
--            psp->gp->has_fallback = 1;
--          }
--        }
            null;
         when WAITING_FOR_TOKEN_NAME =>
--        /* Tokens do not have to be declared before use.  But they can be
--        ** in order to control their assigned integer number.  The number for
--        ** each token is assigned when it is first seen.  So by including
--        **
--        **     %token ONE TWO THREE
--        **
--        ** early in the grammar file, that assigns small consecutive values
--        ** to each of the tokens ONE TWO and THREE.
--        */
--        if( x[0]=='.' ){
--          psp->state = WAITING_FOR_DECL_OR_RULE;
--        }else if( !ISUPPER(x[0]) ){
--          ErrorMsg(psp->filename, psp->tokenlineno,
--            "%%token argument \"%s\" should be a token", x);
--          psp->errorcnt++;
--        }else{
--          (void)lime_symbol_new(x);
--        }
            null;
         when WAITING_FOR_WILDCARD_ID =>
--        if( x[0]=='.' ){
--          psp->state = WAITING_FOR_DECL_OR_RULE;
--        }else if( !ISUPPER(x[0]) ){
--          ErrorMsg(psp->filename, psp->tokenlineno,
--            "%%wildcard argument \"%s\" should be a token", x);
--          psp->errorcnt++;
--        }else{
--          struct symbol *sp = lime_symbol_new(x);
--          if( psp->gp->wildcard==0 ){
--            psp->gp->wildcard = sp;
--          }else{
--            ErrorMsg(psp->filename, psp->tokenlineno,
--              "Extra wildcard to token: %s", x);
--            psp->errorcnt++;
--          }
--        }
            null;
         when WAITING_FOR_CLASS_ID =>
--        if( !ISLOWER(x[0]) ){
--          ErrorMsg(psp->filename, psp->tokenlineno,
--            "%%token_class must be followed by an identifier: ", x);
--          psp->errorcnt++;
--          psp->state = RESYNC_AFTER_DECL_ERROR;
--       }else if( lime_symbol_find(x) ){
--          ErrorMsg(psp->filename, psp->tokenlineno,
--            "Symbol \"%s\" already used", x);
--          psp->errorcnt++;
--          psp->state = RESYNC_AFTER_DECL_ERROR;
--        }else{
--          psp->tkclass = lime_symbol_new(x);
--          psp->tkclass->type = MULTITERMINAL;
--          psp->state = WAITING_FOR_CLASS_TOKEN;
--        }
            null;
         when WAITING_FOR_CLASS_TOKEN =>
--        if( x[0]=='.' ){
--          psp->state = WAITING_FOR_DECL_OR_RULE;
--        }else if( ISUPPER(x[0]) || ((x[0]=='|' || x[0]=='/') && ISUPPER(x[1])) ){
--          struct symbol *msp = psp->tkclass;
--          msp->nsubsym++;
--          msp->subsym = (struct symbol **) realloc(msp->subsym,
--            sizeof(struct symbol*)*msp->nsubsym);
--          if( !ISUPPER(x[0]) ) x++;
--          msp->subsym[msp->nsubsym-1] = lime_symbol_new(x);
--        }else{
--          ErrorMsg(psp->filename, psp->tokenlineno,
--            "%%token_class argument \"%s\" should be a token", x);
--          psp->errorcnt++;
--          psp->state = RESYNC_AFTER_DECL_ERROR;
--        }
            null;
         when RESYNC_AFTER_RULE_ERROR =>
--  /*      if( x[0]=='.' ) psp->state = WAITING_FOR_DECL_OR_RULE;
--  **      break; */
            null;

         when RESYNC_AFTER_DECL_ERROR =>
--        if( x[0]=='.' ) psp->state = WAITING_FOR_DECL_OR_RULE;
--        if( x[0]=='%' ) psp->state = WAITING_FOR_DECL_KEYWORD;
            null;
      end case;

   end Parse_One_Token;


end Parser;
