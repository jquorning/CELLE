--
--  The author disclaims copyright to this source code.  In place of
--  a legal notice, here is a blessing:
--
--    May you do good and not evil.
--    May you find forgiveness for yourself and forgive others.
--    May you share freely, not taking more than you give.
--

with DK8543.Interfaces.C.Strings;

with Symbols;
with Errors;
with Scanner_Errors;

separate (Scanner)
procedure Parse_One_Token (PSP  : in out Scanner_Data.Scanner_Record;
                           Line : in     Scanner_Data.Line_Record)
is
   use Errors;
   use Ada.Strings.Unbounded;
   use Scanner_Data;
   use Scanner_Errors;

   procedure Do_Initialize;

   procedure Do_Initialize is
   begin
      PSP.Prev_Rule    := null;
      PSP.Prec_Counter := 0;
      PSP.First_Rule   := null;
      PSP.Last_Rule    := null;
      PSP.GP.N_Rule    := 0;
   end Do_Initialize;

   X : constant String := Line.Item (PSP.Token_Start .. Line.Last);
begin
--    const char *x;
--    x = Strsafe(psp->tokenstart);     /* Save the token permanently */
--  #if 0
--    printf("%s:%d: Token=[%s] state=%d\n",psp->filename,psp->tokenlineno,
--      x,psp->state);
--  #endif
   case PSP.Scan_State is

      when INITIALIZE =>
         Do_Initialize;

      when WAITING_FOR_DECL_OR_RULE =>
         Do_Initialize;

         if X (0) = '%' then
            PSP.Scan_State := WAITING_FOR_DECL_KEYWORD;

         elsif X (0) in 'a' .. 'z' then
            PSP.LHS        := Symbols.Lime_Symbol_New (Interfaces.C.Strings.New_String (X));
            PSP.N_RHS      := 0;
            PSP.LHS_Alias  := Interfaces.C.Strings.Null_Ptr;
            PSP.Scan_State := WAITING_FOR_ARROW;

         elsif X (0) = '{' then

            if PSP.Prev_Rule = null then
               Error (E001);
--                  Error ("There is no prior rule upon which to attach the code " &
--                           "fragment which begins on this line.");

            elsif PSP.Prev_Rule.Code /= null then
               Error (E002);
--                  Error ("Code fragment beginning on this line is not the first " &
--                           "to follow the previous rule.");

            else
               PSP.Prev_Rule.Line    := PSP.Token_Lineno;
               PSP.Prev_Rule.Code    :=
                 new Unbounded_String'(To_Unbounded_String (X (X'First + 1 .. X'Last)));
               PSP.Prev_Rule.No_Code := False;
            end if;

         elsif X (0) = '[' then
            PSP.Scan_State := PRECEDENCE_MARK_1;

         else
            Error (E003);
--               Error ("Token '" & X & "' should be either '%%' or a nonterminal name.");
         end if;


      when PRECEDENCE_MARK_1 =>

         if X (0) not in 'A' .. 'Z' then
            Error (E004);
--               Error ("The precedence symbol must be a terminal.");

         elsif PSP.Prev_Rule = null then
            Error (E005); --  , X
--               Error ("There is no prior rule to assign precedence '[" & X & "]'.");

         elsif PSP.Prev_Rule.Prec_Sym /= null then
            Error (E006);
--               Error ("Precedence mark on this line is not the first " &
--                        "to follow the previous rule.");

         else
            PSP.Prev_Rule.Prec_Sym :=
              Symbols.Lime_Symbol_New (Interfaces.C.Strings.New_String (X));
         end if;
         PSP.Scan_State := PRECEDENCE_MARK_2;


      when PRECEDENCE_MARK_2 =>
         if X (0) /= ']' then
            --  Error ("Missing ']' on precedence mark.");
            Error (E007);
         end if;
         PSP.Scan_State := WAITING_FOR_DECL_OR_RULE;


      when WAITING_FOR_ARROW =>

         if X (0 .. 2) = "::=" then
            PSP.Scan_State := IN_RHS;

         elsif X (0) = '(' then
            PSP.Scan_State := LHS_ALIAS_1;

         else
            declare
               use Symbols;
            begin
               Error (E008, (1 => To_Unbounded_String (From_Key (PSP.LHS.Name))));
               PSP.Scan_State := RESYNC_AFTER_RULE_ERROR;
            end;
         end if;


      when LHS_ALIAS_1 =>
         if DK8543.Interfaces.C.Strings.Is_Alpha (X (0)) then
            PSP.LHS_Alias  := Interfaces.C.Strings.New_String (X);
            PSP.Scan_State := LHS_ALIAS_2;
         else
            Error (E009, (1 => To_Unbounded_String (X),
                          2 => To_Unbounded_String (Symbols.From_Key (PSP.LHS.Name))));
            PSP.Scan_State := RESYNC_AFTER_RULE_ERROR;
         end if;

      when LHS_ALIAS_2 =>
         if X (0)  = ')' then
            PSP.Scan_State := LHS_ALIAS_3;
         else
            Error (E010, (1 => To_Unbounded_String
                            (Interfaces.C.Strings.Value (PSP.LHS_Alias))));
            PSP.Scan_State := RESYNC_AFTER_RULE_ERROR;
         end if;


      when LHS_ALIAS_3 =>
         if X (0 .. 2) = "::=" then
            PSP.Scan_State := IN_RHS;
         else
            Error (E011, (1 => To_Unbounded_String
                            (Symbols.From_Key (PSP.LHS.Name)),
                          2 => To_Unbounded_String
                            (Interfaces.C.Strings.Value (PSP.LHS_Alias))));
            PSP.Scan_State := RESYNC_AFTER_RULE_ERROR;
         end if;


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

         if DK8543.Interfaces.C.Strings.Is_Alpha (X (X'First)) then
            PSP.Alias (PSP.N_RHS - 1) := To_Unbounded_String (X);
            PSP.Scan_State := RHS_ALIAS_2;

         else
            Error (E012, (1 => To_Unbounded_String (X),
                          2 => To_Unbounded_String
                            (Symbols.From_Key (PSP.RHS (PSP.N_RHS - 1).Name))));
            PSP.Scan_State := RESYNC_AFTER_RULE_ERROR;

         end if;


      when RHS_ALIAS_2 =>

         if X (X'First) = ')' then
            PSP.Scan_State := IN_RHS;

         else
            Error (E013, (1 => To_Unbounded_String
                            (Interfaces.C.Strings.Value (PSP.LHS_Alias))));
            PSP.Scan_State := RESYNC_AFTER_RULE_ERROR;

         end if;


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
