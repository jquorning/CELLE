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

with DK8543.Text.IO;
with DK8543.Text.Utility;
with DK8543.Errors;
with DK8543.Interfaces.C.Strings;

with Rules;
with Symbols;

package body Scanner is

   --
   --  The state of the parser
   --
   type State_Scanner is
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

   Max_Line_Length : constant := 250;   --  Should do for the most
   subtype Extended_Pos is Natural      range 0 .. Max_Line_Length;
   subtype Line_Pos     is Extended_Pos range 1 .. Extended_Pos'Last;

   --
   --  Line_Record
   --

   type Mode_Identifier is
     (Root,             --  On outer level
      String_Literal,
      C_Code_Block,
      Identifier,       --  Identifier
      Quoted_Identifier --  Identifier in quortes
     );

   use Ada.Strings.Unbounded;
   type Line_Record is
      record
         Current : Line_Pos;          --  Position in Item of examined character
         First   : Line_Pos;          --  First position in Item
         Last    : Line_Pos;          --  Last position in Item
         Item    : String (Line_Pos); --  Full line read from input
         Mode    : Mode_Identifier;   --  Mode
         Buffer  : Unbounded_String;  --  Holder for identifiers etc.
      end record;

   --
   --  Scanner_Record
   --

   type State_Preproc is
     (Root,
      Ifdef,
      Ifndef);

   MAX_RHS : constant := 1000;

   type RHS_Array is array (0 .. MAX_RHS - 1) of Symbols.Symbol_Access;

   type Scanner_Record is
      record
         Token_Start  : Line_Pos;            --  Text of current token
         Token_Lineno : Natural;             --  Linenumber at which current token starts
         Error_Count  : Natural;             --  Number of errors so far
         GP : access Lime.Lemon_Record;      --  Global state vector
         Preproc_State : State_Preproc;
         Scan_State    : State_Scanner;      --  The state of the parser
--    struct symbol *fallback;   --  The fallback token
--    struct symbol *tkclass;    --  Token class symbol
         LHS       : Symbols.Symbol_Access;           --  Left-hand side of current rule
         LHS_Alias : Interfaces.C.Strings.chars_ptr;  --  Alias for the LHS
         N_RHS     : Natural;                         --  Number of right-hand side symbols seen

         --    struct symbol *rhs[MAXRHS];  --  RHS symbols
         RHS : RHS_Array;
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
         File_Name    : Unbounded_String;    --  Name of the input file
      end record;

   --  Run the preprocessor over the input file text.  The global
   --  variables azDefine[0] through azDefine[nDefine-1] contains the
   --  names of all defined macros.  This routine looks for "%ifdef"
   --  and "%ifndef" and "%endif" and comments them out.  Text in
   --  between is also commented out as appropriate.

   procedure Parse_One_Token (PSP  : in out Scanner_Record;
                              Line : in     Line_Record);
   --  Parse a single Token.


   procedure Get_Line_Without_EOL_Comment (Line : out Line_Record);

   procedure Error ( --  PS   : in out Scanner_Record;
                    Text : in     String);


   Comment_CPP     : constant String := "//";
   Comment_C_Begin : constant String := "/*";
   Comment_C_End   : constant String := "*/";

   Preproc_Ifdef   : constant String := "%ifdef";
   Preproc_Ifndef  : constant String := "%ifndef";
   Preproc_Endif   : constant String := "%endif";

   use Ada.Text_IO;
   File : File_Type;
   Line : Line_Record;
   PS   : Scanner_Record;
   Start_Line : Integer := 0;


   procedure Get_Line_Without_EOL_Comment (Line : out Line_Record)
   is
      use DK8543.Text;
   begin
      Ada.Text_IO.Get_Line (Line.Item, Line.Last);
      Line.First := Line.Item'First;
      Utility.Strip_End_Of_Line (From  => Line.Item,
                                 Strip => Comment_CPP,
                                 Last  => Line.Last);
   end Get_Line_Without_EOL_Comment;


   procedure Error (Text : in String)
   is
      use DK8543.Errors;
   begin
      Error (To_String (PS.File_Name), Start_Line, Text);
      PS.Error_Count := PS.Error_Count + 1;
   end Error;


   procedure Comment_C_Filter (Line : in out Line_Record);

   Comment_C_Start : Natural;
   Comment_C_Stop  : Natural;

   procedure Comment_C_Filter (Line : in out Line_Record)
   is
      use Ada.Strings.Fixed;
   begin
      loop
         Get_Line_Without_EOL_Comment (Line);
         Comment_C_Stop :=
           Index (Line.Item (Line.First .. Line.Last), Comment_C_End);
         if Comment_C_Stop /= 0 then
            Line.First := Comment_C_Stop + Comment_C_End'Length;
            exit;
         end if;
      end loop;
   end Comment_C_Filter;


   procedure Parse_Current_Character (Line : in out Line_Record);
   procedure Parse_Quoted_Identifier (Line : in out Line_Record);

   procedure Parse_Current_Character (Line : in out Line_Record)
   is
      Current : constant Character := Line.Item (Line.Current);
   begin
      case Current is

         when '"' =>                     --   String literals
            Line.Mode   := Quoted_Identifier;
            Line.Buffer := Null_Unbounded_String;

         when '{' =>
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
            null;

         when 'a' .. 'z' | 'A' .. 'Z' =>
--        while( (c= *cp)!=0 && (ISALNUM(c) || c=='_') ) cp++;
--        nextcp = cp;
--      }else if( c==':' && cp[1]==':' && cp[2]=='=' ){ /* The operator "::=" */
            null;

         when ':' =>
--        cp += 3;
--        nextcp = cp;
--      }else if( (c=='/' || c=='|') && ISALPHA(cp[1]) ){
            null;

         when '/' =>
      --        cp += 2;
--        while( (c = *cp)!=0 && (ISALNUM(c) || c=='_') ) cp++;
--        nextcp = cp;
--      }else{                          /* All other (one character) operators */
            null;

         when others =>
               --        cp++;
--        nextcp = cp;
--      }
            null;

      end case;
--      c = *cp;
--      *cp = 0;                        /* Null terminate the token */
      Parse_One_Token (PS, Line);       --  Parse the token
--    *cp = (char)c;                  /* Restore the buffer */
--    cp = nextcp;
   end Parse_Current_Character;


   procedure Parse_Quoted_Identifier (Line : in out Line_Record)
   is
      Current : Character renames Line.Item (Line.Current);
   begin
      if Current = '"' then
         Line.Mode := Root;
      else
         Line.Buffer := Line.Buffer & Current;
      end if;
   exception
      when Constraint_Error =>
         Error
           ("String starting on this line is not terminated before the end of the file.");
   end Parse_Quoted_Identifier;

   procedure Parse_On_Mode (Line  : in out Line_Record;
                            Break :    out Boolean);


   procedure Parse_On_Mode (Line  : in out Line_Record;
                            Break :    out Boolean)
   is
      Current : Character renames Line.Item (Line.Current);
   begin
      case Line.Mode is

         when String_Literal => null;

         when Identifier =>  null;
         when C_Code_Block =>  null;

         when Quoted_Identifier =>  Parse_Quoted_Identifier (Line);
         when Root              =>  Parse_Current_Character (Line);

      end case;

   exception

      when Constraint_Error =>
         case Line.Mode is

            when Quoted_Identifier =>
               Error ("String starting on this line is not " &
                        "terminated before the end of the " &
                        "file.");

            when others =>
               raise;

         end case;

   end Parse_On_Mode;


   procedure Parse (GP : access Lime.Lemon_Record)
   is

      use Ada.Strings.Fixed;
      use DK8543.Text;

      Break_Out : Boolean;
   begin
      PS.GP          := GP;
      PS.File_Name   :=
        To_Unbounded_String (Interfaces.C.Strings.Value (GP.File_Name));
      PS.Error_Count := 0;
      PS.Scan_State  := INITIALIZE;

      --  Begin by opening the input file
      Open (File, In_File, To_String (PS.File_Name));

      --  Make an initial pass through the file to handle %ifdef and %ifndef.
      --  Preprocess_Input (filebuf);

      --  Now scan the text of the input file.
      loop
         Get_Line_Without_EOL_Comment (Line);

         --  Preprocess
         if Line.First = Line.Item'First then
            if In_First_Part (Line.Item, Preproc_Ifdef) then
               null;
            elsif In_First_Part (Line.Item, Preproc_Ifndef) then
               null;
            elsif In_First_Part (Line.Item, Preproc_Endif) then
               null;
            else
               null;
            end if;
         end if;

         Comment_C_Start := Index (Line.Item (Line.First .. Line.Last), Comment_C_Begin);
         exit when Comment_C_Start = 0;

         --  Skip C comments
         Comment_C_Filter (Line);

         --  Trim leading spaces
         DK8543.Text.Trim (Line.Item, Line.First, Line.Last,
                           Side => Ada.Strings.Left);

         PS.Token_Start  := Line.First;       --  Mark the beginning of the token
         PS.Token_Lineno := IO.Line_Number;   --  Linenumber on which token begins

         loop
            Parse_On_Mode (Line, Break_Out);
            exit when Break_Out;
         end loop;

      end loop;

   exception

      when End_Error =>
         Close (File);
         GP.Rule      := Rules.Rule_Access (PS.First_Rule);
         GP.Error_Cnt := PS.Error_Count;

      when others =>
--           Auxiliary.Errors.Error
--             (To_String (PS.File_Name), 0, "Can't open this file for reading.");
--           GP.Error_Cnt := GP.Error_Cnt + 1;
         Error ("Can't open this file for reading.");

   end Parse;


   procedure Parse_One_Token (PSP  : in out Scanner_Record;
                              Line : in     Line_Record)
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
                  Error ("There is no prior rule upon which to attach the code " &
                           "fragment which begins on this line.");

               elsif PSP.Prev_Rule.Code /= null then
                  Error ("Code fragment beginning on this line is not the first " &
                           "to follow the previous rule.");

               else
                  PSP.Prev_Rule.Line    := PSP.Token_Lineno;
                  PSP.Prev_Rule.Code    :=
                    new Unbounded_String'(To_Unbounded_String (X (X'First + 1 .. X'Last)));
                  PSP.Prev_Rule.No_Code := False;
               end if;

            elsif X (0) = '[' then
               PSP.Scan_State := PRECEDENCE_MARK_1;

            else
               Error ("Token '" & X & "' should be either '%%' or a nonterminal name.");
            end if;


         when PRECEDENCE_MARK_1 =>

            if X (0) not in 'A' .. 'Z' then
               Error ("The precedence symbol must be a terminal.");

            elsif PSP.Prev_Rule = null then
               Error ("There is no prior rule to assign precedence '[" & X & "]'.");

            elsif PSP.Prev_Rule.Prec_Sym /= null then
               Error ("Precedence mark on this line is not the first " &
                        "to follow the previous rule.");

            else
               PSP.Prev_Rule.Prec_Sym :=
                 Symbols.Lime_Symbol_New (Interfaces.C.Strings.New_String (X));
            end if;
            PSP.Scan_State := PRECEDENCE_MARK_2;


         when PRECEDENCE_MARK_2 =>
            if X (0) /= ']' then
               Error ("Missing ']' on precedence mark.");
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
                  Error ("Expected to see a ':' following the LHS symbol '" &
                           From_Key (PSP.LHS.Name) & "'.");
                  PSP.Scan_State := RESYNC_AFTER_RULE_ERROR;
               end;
            end if;


         when LHS_ALIAS_1 =>
            if DK8543.Interfaces.C.Strings.Is_Alpha (X (0)) then
               PSP.LHS_Alias  := Interfaces.C.Strings.New_String (X);
               PSP.Scan_State := LHS_ALIAS_2;
            else
               Error ("'" & X & "' is not a valid alias for the LHS '" &
                        Symbols.From_Key (PSP.LHS.Name) & "'");
               PSP.Scan_State := RESYNC_AFTER_RULE_ERROR;
            end if;

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


end Scanner;
