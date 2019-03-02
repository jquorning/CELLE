--
--  The author disclaims copyright to this source code.  In place of
--  a legal notice, here is a blessing:
--
--    May you do good and not evil.
--    May you find forgiveness for yourself and forgive others.
--    May you share freely, not taking more than you give.
--

--  with DK8543.Interfaces.C.Strings;

with Symbols;
with Errors;
with Scanner_Errors;

separate (Scanners)
procedure Parse_One_Token (Lemon   : in out Lime.Lemon_Record;
                           Scanner : in out Scanner_Record)
is
   use Ada.Strings.Unbounded;

   use Errors;
   use Scanner_Errors;
   use Rules;

   procedure Do_State_Initialize;
   procedure Do_State_Waiting_For_Decl_Or_Rule;
   procedure Do_State_Precedence_Mark_1;
   procedure Do_State_Precedence_Mark_2;
   procedure Do_State_In_RHS;

   X : String    renames Scanner.Item (Scanner.Token_Start .. Scanner.Last);
   C : Character renames X (X'First);

   procedure Do_State_Initialize is
   begin
      Scanner.Prev_Rule    := null;
      Scanner.Prec_Counter := 0;
      Scanner.First_Rule   := null;
      Scanner.Last_Rule    := null;

      Lemon.N_Rule := 0;
   end Do_State_Initialize;


   procedure Do_State_Waiting_For_Decl_Or_Rule is
   begin
      Do_State_Initialize;

      if C = '%' then
         Scanner.Scan_State := WAITING_FOR_DECL_KEYWORD;

      elsif C in 'a' .. 'z' then
         Scanner.LHS.Append (Symbols.Lime_Symbol_New (Interfaces.C.Strings.New_String (X)));
         --            PSP.N_RHS      := 0;

         Scanner.RHS        := Symbols.Symbol_Vectors.Empty_Vector;
         --            PSP.LHS_Alias  := Interfaces.C.Strings.Null_Ptr;
         Scanner.LHS_Alias  := Scanner_Data.Alias_Vectors.Empty_Vector;
         Scanner.Scan_State := WAITING_FOR_ARROW;

      elsif C = '{' then

         if Scanner.Prev_Rule = null then
            Error (E001);
            --                  Error ("There is no prior rule upon which to attach the code " &
            --                           "fragment which begins on this line.");

         elsif Rules."/=" (Scanner.Prev_Rule.Code, Null_Code) then
            Error (E002);
            --                  Error ("Code fragment beginning on this line is not the first " &
            --                           "to follow the previous rule.");

         else
            Scanner.Prev_Rule.Line := Scanner.Token_Lineno;
            Scanner.Prev_Rule.Code :=
              Unbounded_String'(To_Unbounded_String (X (X'First + 1 .. X'Last)));
            --  new Unbounded_String'(To_Unbounded_String (X (X'First + 1 .. X'Last)));
            Scanner.Prev_Rule.No_Code := False;
         end if;

      elsif C = '[' then
         Scanner.Scan_State := PRECEDENCE_MARK_1;

      else
         Error (E003);
         --               Error ("Token '" & X & "' should be either '%%' or a nonterminal name.");
      end if;
   end Do_State_Waiting_For_Decl_Or_Rule;


   procedure Do_State_Precedence_Mark_1 is
   begin
      if C not in 'A' .. 'Z' then
         Error (E004);
         --  Error ("The precedence symbol must be a terminal.");

      elsif Scanner.Prev_Rule = null then
         Error (E005);
         --  Error ("There is no prior rule to assign precedence '[" & X & "]'.");

      elsif Scanner.Prev_Rule.Prec_Sym /= null then
         Error (E006);
         --  Error ("Precedence mark on this line is not the first " &
         --         "to follow the previous rule.");

      else
         Scanner.Prev_Rule.Prec_Sym :=
           Symbols.Lime_Symbol_New (Interfaces.C.Strings.New_String (X));
      end if;

      Scanner.Scan_State := PRECEDENCE_MARK_2;

   end Do_State_Precedence_Mark_1;


   procedure Do_State_Precedence_Mark_2 is
   begin
      if C /= ']' then
         --  Error ("Missing ']' on precedence mark.");
         Error (E007);
      end if;

      Scanner.Scan_State := WAITING_FOR_DECL_OR_RULE;

   end Do_State_Precedence_Mark_2;


   procedure Do_State_In_RHS is
   begin
      if C = '.' then
         declare
            use Symbols;
            Rule : constant access Rules.Rule_Record := new Rules.Rule_Record;
         begin
            --  Rp := (struct rule *)calloc( sizeof(struct rule) +
            --                               sizeof(struct symbol*)*psp->nrhs +
            --                               sizeof(char*)*psp->nrhs, 1);
            --               RP := new Rules.Rule_Record;
            --  if Rp = 0 then
            --   ErrorMsg(psp->filename,psp->tokenlineno,
            --            "Can't allocate enough memory for this rule.");
            --   psp->errorcnt++;
            --   Psp.Prev_Rule := 0;
            --  else
            Rule.Rule_Line := Scanner.Token_Lineno;
            --  Rp.rhs      := (struct symbol**)&rp[1];
            --  Rp.rhsalias := (const char**)&(rp->rhs[psp->nrhs]);

            --                    for I in 0 .. PSP.N_RHS - 1 loop
            --                       RP.RHS       (I) := PSP.RHS   (I);
            --                       RP.RHS_Alias (I) := PSP.Alias (I);
            --                       if RP.RHS_Alias (I) /= null then
            --                          RP.RHS (I).Content := True;
            --                       end if;
            --                    end loop;

            declare
               subtype Index_Range is Positive range
                 Scanner.RHS.First_Index .. Scanner.RHS.Last_Index;
            begin
               for I in Index_Range loop
                  Rule.RHS       (I) := Scanner.RHS   (I);
                  --  XXX                       RP.RHS_Alias (I) := PSP.Alias.Element (I);
                  --  if Symbols."/=" (RP.RHS_Alias (I), Null_Unbounded_String) then
                  --                        declare
                  --                           use
                  --                        begin
                  --  if RP.RHS_Alias (I) /= Null_Unbounded_String then
                  if Length (Rule.RHS_Alias (I)) /= 0 then
                     Rule.RHS (I).Content := True;
                  end if;
                  --                        end;
               end loop;
            end;

            Rule.LHS        := Scanner.LHS.First_Element;
            Rule.LHS_Alias  := Scanner.LHS_Alias.First_Element;
            Rule.Code       := Null_Code; --  New Unbounded_String'(Null_Unbounded_String);
            Rule.No_Code    := True;
            Rule.Prec_Sym   := null;

            Lemon.N_Rule  := Lemon.N_Rule + 1;

            Rule.Index      := Lemon.N_Rule;
            Rule.Next_LHS   := Rule.LHS.Rule;
            Rule.LHS.Rule   := Rule;
            Rule.Next       := null;
            if Scanner.First_Rule = null then
               Scanner.First_Rule := Rule;
               Scanner.Last_Rule  := Rule;
            else
               Scanner.Last_Rule.Next := Rule;
               Scanner.Last_Rule      := Rule;
            end if;
            Scanner.Prev_Rule := Rule;
            --  end if;
         end;
         Scanner.Scan_State := WAITING_FOR_DECL_OR_RULE;

      elsif
        C in 'a' .. 'z' or
        C in 'A' .. 'Z'
      then
         Scanner.RHS  .Append (Symbols.Lime_Symbol_New (Interfaces.C.Strings.New_String (X)));
         Scanner.Alias.Append (Null_Unbounded_String);
         --            end if;

      elsif
        (C = '|' or C = '/') and not
        Scanner.RHS.Is_Empty
      then
         declare
            use Symbols;
            Symbol : Symbols.Symbol_Access := Scanner.RHS.Last_Element;
         begin
            if Symbol.Kind /= Symbols.Multi_Terminal then
               declare
                  Orig_Symbol : constant Symbol_Access := Symbol;
               begin
                  Symbol := new Symbol_Record;
                  Symbol.Kind    := Symbols.Multi_Terminal;
                  Symbol.Sub_Sym := Symbol_Vectors.Empty_Vector;
                  Symbol.Sub_Sym.Append (Orig_Symbol);

                  Symbol.Name := Orig_Symbol.Name;

                  Scanner.RHS.Append (Symbol);
               end;
            end if;

            Symbol.Sub_Sym.Append
              (Symbols.Lime_Symbol_New
                 (Interfaces.C.Strings.New_String (X (X'First + 1 .. X'Last))));

            if
              X (X'First + 1) in 'a' .. 'z' or
              To_String (Symbol.Sub_Sym.First_Element.Name) (1) in 'a' .. 'z'
            then
               Errors.Error (E201, Line_Number => Scanner.Token_Lineno);
            end if;
         end;

      elsif C = '(' and not Scanner.RHS.Is_Empty then
         Scanner.Scan_State := RHS_ALIAS_1;

      else
         Error (E202, (1 => To_Unbounded_String (X)));
         Scanner.Scan_State := RESYNC_AFTER_RULE_ERROR;
      end if;
   end Do_State_In_RHS;

   PSP : Scanner_Record renames Scanner;
begin

   case Scanner.Scan_State is

      when INITIALIZE               =>  Do_State_Initialize;
      when WAITING_FOR_DECL_OR_RULE =>  Do_State_Waiting_For_Decl_Or_Rule;
      when PRECEDENCE_MARK_1        =>  Do_State_Precedence_Mark_1;
      when PRECEDENCE_MARK_2        =>  Do_State_Precedence_Mark_2;

      when WAITING_FOR_ARROW =>

         if X (1 .. 3) = "::=" then
            PSP.Scan_State := IN_RHS;

         elsif X (1) = '(' then
            PSP.Scan_State := LHS_ALIAS_1;

         else
            declare
               use Symbols;
            begin
--               Error (E008, (1 => To_Unbounded_String (From_Key (PSP.LHS.Name))));
               Error (E008, (1 => To_Unbounded_String
                               (From_Key (PSP.LHS.First_Element.Name))));
               PSP.Scan_State := RESYNC_AFTER_RULE_ERROR;
            end;
         end if;


      when LHS_ALIAS_1 =>
         if
           X (X'First) in 'a' .. 'z' or
           X (X'First) in 'A' .. 'Z'
         then
--            PSP.LHS_Alias  := Interfaces.C.Strings.New_String (X);
--            PSP.LHS_Alias.Append (Interfaces.C.Strings.New_String (X));
            PSP.LHS_Alias.Append (To_Alias (X));
            PSP.Scan_State := LHS_ALIAS_2;
         else
            Error (E009, (1 => To_Unbounded_String (X),
--                          2 => To_Unbounded_String (Symbols.From_Key (PSP.LHS.Name))));
                          2 => To_Unbounded_String
                            (Symbols.From_Key (PSP.LHS.First_Element.Name))));
            PSP.Scan_State := RESYNC_AFTER_RULE_ERROR;
         end if;

      when LHS_ALIAS_2 =>
         if X (X'First)  = ')' then
            PSP.Scan_State := LHS_ALIAS_3;
         else
--            Error (E010, (1 => To_Unbounded_String
--                            (Interfaces.C.Strings.Value (PSP.LHS_Alias))));
            Error (E010, (1 => PSP.LHS_Alias.First_Element));
            PSP.Scan_State := RESYNC_AFTER_RULE_ERROR;
         end if;


      when LHS_ALIAS_3 =>
         if X (X'First .. X'First + 2) = "::=" then
            PSP.Scan_State := IN_RHS;
         else
            Error (E011, (1 => To_Unbounded_String
--                            (Symbols.From_Key (PSP.LHS.Name)),
                            (Symbols.From_Key (PSP.LHS.First_Element.Name)),
--                          2 => To_Unbounded_String
--                            (Interfaces.C.Strings.Value (PSP.LHS_Alias))));
                          2 => PSP.LHS_Alias.First_Element));
            PSP.Scan_State := RESYNC_AFTER_RULE_ERROR;
         end if;


      when IN_RHS =>   Do_State_In_RHS;

      when RHS_ALIAS_1 =>

         if
           X (X'First) in 'a' .. 'z' or
           X (X'First) in 'A' .. 'Z'
         then
            PSP.Alias.Append (To_Alias (X));
            PSP.Scan_State   := RHS_ALIAS_2;

         else
            Error (E012, (1 => To_Unbounded_String (X),
                          2 => To_Unbounded_String
                            (Symbols.From_Key (PSP.RHS.Last_Element.Name))));
            PSP.Scan_State := RESYNC_AFTER_RULE_ERROR;

         end if;


      when RHS_ALIAS_2 =>

         if X (X'First) = ')' then
            PSP.Scan_State := IN_RHS;

         else
            Error (E013, (1 => PSP.LHS_Alias.First_Element));
            PSP.Scan_State := RESYNC_AFTER_RULE_ERROR;

         end if;


      when WAITING_FOR_DECL_KEYWORD =>
         if
           X (X'First) in 'a' .. 'z' or
           X (X'First) in 'A' .. 'Z'
         then
            PSP.Decl_Keyword      := To_Unbounded_String (X);
            PSP.Decl_Arg_Slot     := null;
            PSP.Decl_Lineno_Slot  := null;
            PSP.Insert_Line_Macro := True;
            PSP.Scan_State := WAITING_FOR_DECL_ARG;

            if X = "name" then
               PSP.Decl_Arg_Slot := Lemon.Names.Name'Access;
               PSP.Insert_Line_Macro := False;

            elsif X = "include" then
               PSP.Decl_Arg_Slot := Lemon.Names.Include'Access;

            elsif X = "code" then
               PSP.Decl_Arg_Slot := Lemon.Names.Extra_Code'Access;

            elsif X = "token_destructor" then
               PSP.Decl_Arg_Slot := Lemon.Names.Token_Dest'Access;

            elsif X = "default_destructor" then
               PSP.Decl_Arg_Slot := Lemon.Names.Var_Dest'Access;

            elsif X = "token_prefix" then
               PSP.Decl_Arg_Slot := Lemon.Names.Token_Prefix'Access;
               PSP.Insert_Line_Macro := False;

            elsif X = "syntax_error" then
               PSP.Decl_Arg_Slot := Lemon.Names.Error'Access;

            elsif X = "parse_accept" then
               PSP.Decl_Arg_Slot := Lemon.Names.C_Accept'Access;

            elsif X = "parse_failure" then
               PSP.Decl_Arg_Slot := Lemon.Names.Failure'Access;

            elsif X = "stack_overflow" then
               PSP.Decl_Arg_Slot := Lemon.Names.Overflow'Access;

            elsif X = "extra_argument" then
               PSP.Decl_Arg_Slot     := Lemon.Names.ARG2'Access;
               PSP.Insert_Line_Macro := False;

            elsif X = "extra_context" then
               PSP.Decl_Arg_Slot     := Lemon.Names.CTX2'Access;
               PSP.Insert_Line_Macro := False;

            elsif X = "token_type" then
               PSP.Decl_Arg_Slot     := Lemon.Names.Token_Type'Access;
               PSP.Insert_Line_Macro := False;

            elsif X = "default_type" then
               PSP.Decl_Arg_Slot     := Lemon.Names.Var_Type'Access;
               PSP.Insert_Line_Macro := False;

            elsif X = "stack_size" then
               PSP.Decl_Arg_Slot     := Lemon.Names.Stack_Size'Access;
               PSP.Insert_Line_Macro := False;

            elsif X = "start_symbol" then
               PSP.Decl_Arg_Slot     := Lemon.Names.Start'Access;
               PSP.Insert_Line_Macro := False;

            elsif X = "left" then
               PSP.Prec_Counter := PSP.Prec_Counter + 1;
               PSP.Decl_Assoc   := Symbols.Left;
               PSP.Scan_State   := WAITING_FOR_PRECEDENCE_SYMBOL;

            elsif X = "right" then
               PSP.Prec_Counter := PSP.Prec_Counter + 1;
               PSP.Decl_Assoc   := Symbols.Right;
               PSP.Scan_State   := WAITING_FOR_PRECEDENCE_SYMBOL;

            elsif X = "nonassoc" then
               PSP.Prec_Counter := PSP.Prec_Counter + 1;
               PSP.Decl_Assoc   := Symbols.None;
               PSP.Scan_State   := WAITING_FOR_PRECEDENCE_SYMBOL;

            elsif X = "destructor" then
               PSP.Scan_State := WAITING_FOR_DESTRUCTOR_SYMBOL;

            elsif X = "type" then
               PSP.Scan_State := WAITING_FOR_DATATYPE_SYMBOL;

            elsif X = "fallback" then
               PSP.Fallback := null;
               PSP.Scan_State := WAITING_FOR_FALLBACK_ID;

            elsif X = "token" then
               PSP.Scan_State := WAITING_FOR_TOKEN_NAME;

            elsif X = "wildcard" then
               PSP.Scan_State := WAITING_FOR_WILDCARD_ID;

            elsif X = "token_class" then
               PSP.Scan_State := WAITING_FOR_CLASS_ID;

            else
               Error (E203, (1 => To_Unbounded_String (X)), Line_Number => PSP.Token_Lineno);
               PSP.Scan_State := RESYNC_AFTER_DECL_ERROR;
            end if;
         else
            Error (E204, (1 => To_Unbounded_String (X)), Line_Number => PSP.Token_Lineno);
            PSP.Scan_State := RESYNC_AFTER_DECL_ERROR;
         end if;


      when WAITING_FOR_DESTRUCTOR_SYMBOL =>
         if
           X (X'First) not in 'a' .. 'z' and
           X (X'First) not in 'A' .. 'Z'
         then
            Errors.Error (E205, Line_Number => PSP.Token_Lineno);
            PSP.Scan_State := RESYNC_AFTER_DECL_ERROR;
         else
            declare
               use Interfaces.C.Strings;
               Symbol : Symbols.Symbol_Access := Symbols.Lime_Symbol_New (New_String (X));
            begin
               PSP.Decl_Arg_Slot     :=
                 new chars_ptr'(New_String (To_String (Symbol.Destructor))); -- XXX
               PSP.Decl_Lineno_Slot  := Symbol.Dest_Lineno'Access;
               PSP.Insert_Line_Macro := True;
            end;
            PSP.Scan_State := WAITING_FOR_DECL_ARG;
         end if;


      when WAITING_FOR_DATATYPE_SYMBOL =>
         if
           X (X'First) not in 'a' .. 'z' and
           X (X'First) not in 'A' .. 'Z'
         then
            Errors.Error (E206, Line_Number => PSP.Token_Lineno);
            PSP.Scan_State := RESYNC_AFTER_DECL_ERROR;
         else
            declare
               use Interfaces.C.Strings;
               use Symbols;
               Symbol : Symbols.Symbol_Access := Symbols.Lime_Symbol_Find (New_String (X));
            begin
               if
                 Symbol /= null and then
                 Symbols."/=" (Symbol.Data_Type, Null_Unbounded_String)
               then
                  Error (E207, (1 => To_Unbounded_String (X)),
                         Line_Number => PSP.Token_Lineno);
                  PSP.Scan_State := RESYNC_AFTER_DECL_ERROR;
               else
                  if Symbol = null then
                     Symbol := Lime_Symbol_New (New_String (X));
                  end if;
                  PSP.Decl_Arg_Slot     :=
                    new chars_ptr'(New_String (To_String (Symbol.Data_Type)));
                  PSP.Insert_Line_Macro := False;
                  PSP.Scan_State        := WAITING_FOR_DECL_ARG;
               end if;
            end;
         end if;


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
         if C = '.' then
            Scanner.Scan_State := WAITING_FOR_DECL_OR_RULE;

         elsif C not in 'A' .. 'Z' then
            Errors.Error
              (E211,
               Line_Number => Scanner.Token_Lineno,
               Arguments   => (1 => To_Unbounded_String (X)));
         else
            declare
               use Interfaces.C.Strings;
               use Symbols;

               Symbol : Symbol_Access := Lime_Symbol_New (New_String (X));
            begin
--               if Scanner.Gp.Wildcard = 0 then
--                  Scanner.Gp.wildcard := Symbol;
               if Get_Wildcard (Lemon.Extra) = null then
                  Set_Wildcard (Lemon.Extra, Symbol);
               else
                  Errors.Error
                    (E212,
                     Line_Number => Scanner.Token_Lineno,
                     Arguments   => (1 => To_Unbounded_String (X)));
               end if;
            end;
         end if;


      when WAITING_FOR_CLASS_ID =>
         declare
            use Interfaces.C.Strings;
            use Symbols;
         begin
            if C not in 'a' .. 'z' then
               Errors.Error
                 (E209,
                  Line_Number => Scanner.Token_Lineno,
                  Arguments   => (1 => To_Unbounded_String (X)));
               Scanner.Scan_State := RESYNC_AFTER_DECL_ERROR;

            elsif Lime_Symbol_Find (New_String (X)) /= null then
               Errors.Error
                 (E210,
                  Line_Number => Scanner.Token_Lineno,
                  Arguments   => (1 => To_Unbounded_String (X)));
               Scanner.Scan_State := RESYNC_AFTER_DECL_ERROR;

            else
               Scanner.Token_Class      := Lime_Symbol_New (New_String (X));
               Scanner.Token_Class.Kind := Multi_Terminal;
               Scanner.Scan_State       := WAITING_FOR_CLASS_TOKEN;

            end if;
         end;

      when WAITING_FOR_CLASS_TOKEN =>

         if C = '.' then
            Scanner.Scan_State := WAITING_FOR_DECL_OR_RULE;

         elsif
           (C in 'A' .. 'Z') or
           ((C = '|' or C = '/') and
              X (X'First + 1) in 'A' .. 'Z')
         then
            declare
               use Symbols;

               Symbol : Symbol_Access := Scanner.Token_Class; -- ???
               First  : Natural := X'First;
            begin
               --  Symbol.N_Sub_Sym := Symbol.N_Sub_Sym + 1;
               --  Symbol.Sub_Sym := (struct symbol **) realloc(msp->subsym,
               --                    sizeof(struct symbol*)*msp->nsubsym);
               if C not in 'A' .. 'Z' then
                  First := X'First + 1;
               end if;
               --  Symbol.Sub_Sym (symbol.N_Sub_Sym - 1) := Lime_Symbol_New (X (First .. X'Last));
               Symbol.Sub_Sym.Append
                 (Lime_Symbol_New
                    (Interfaces.C.Strings.New_String (X (First .. X'Last))));
            end;
         else
            Errors.Error
              (E208,
               Line_Number => Scanner.Token_Lineno,
               Arguments   => (1 => To_Unbounded_String (X)));

            Scanner.Scan_State := RESYNC_AFTER_DECL_ERROR;

         end if;

      when RESYNC_AFTER_RULE_ERROR =>
         --  // if( x[0]=='.' ) psp->state = WAITING_FOR_DECL_OR_RULE;
         --  // break;
         null;

      when RESYNC_AFTER_DECL_ERROR =>
         case C is
            when '.' =>  Scanner.Scan_State := WAITING_FOR_DECL_OR_RULE;
            when '%' =>  Scanner.Scan_State := WAITING_FOR_DECL_KEYWORD;
            when others => null;
         end case;

   end case;

end Parse_One_Token;
