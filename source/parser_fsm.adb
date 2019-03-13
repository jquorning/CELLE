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

with Symbols;
with Errors;
with Rules;
with Extras;

package body Parser_FSM is

   use Lime;
   use Parser_Data;

   procedure Do_State_Waiting_For_Decl_Or_Rule (Lemon   : in out Lemon_Record;
                                                Scanner : in out Scanner_Record);
   procedure Do_State_Precedence_Mark_1        (Scanner : in out Scanner_Record);
   procedure Do_State_Precedence_Mark_2        (Scanner : in out Scanner_Record);
   procedure Do_State_Waiting_For_Decl_Keyword (Lemon   : in out Lemon_Record;
                                                Scanner : in out Scanner_Record);
   procedure Do_State_Waiting_For_Decl_Arg (Lemon   : in     Lemon_Record;
                                            Scanner : in out Scanner_Record);


   procedure Do_State_In_RHS (Lemon   : in out Lemon_Record;
                              Scanner : in out Scanner_Record);


   procedure Debug (On   : in Boolean;
                    Text : in String);


   --
   --
   --

   use Errors;
   use Ada.Strings.Unbounded;


   procedure Do_State (Lemon   : in out Lemon_Record;
                       Scanner : in out Scanner_Record)
   is
      Debug_On : constant Boolean := True;
      X : constant String    := Current_Token_Line (Scanner);
      C : constant Character := Current_Token_Char (Scanner);
   begin
      Debug (Debug_On, "Do_State: STATE: " & Scanner.State'Img);
      --  Debug (Debug_On, "  X: " & X);
      --  Debug (Debug_On, "  C: " & C);

      case Scanner.State is

      when WAITING_FOR_DECL_OR_RULE =>  Do_State_Waiting_For_Decl_Or_Rule (Lemon, Scanner);
      when PRECEDENCE_MARK_1        =>  Do_State_Precedence_Mark_1 (Scanner);
      when PRECEDENCE_MARK_2        =>  Do_State_Precedence_Mark_2 (Scanner);

      when WAITING_FOR_ARROW =>

         if X (1 .. 3) = "::=" then
            Scanner.State := IN_RHS;

         elsif X (1) = '(' then
            Scanner.State := LHS_ALIAS_1;

         else
            declare
               use Symbols;
            begin
               Parser_Error
                 (E008, Scanner.Token_Lineno,
                  (1 => To_Unbounded_String
                     (From_Key (Scanner.LHS.First_Element.Name))));
               Scanner.State := RESYNC_AFTER_RULE_ERROR;
            end;
         end if;


      when LHS_ALIAS_1 =>
         if
           X (X'First) in 'a' .. 'z' or
           X (X'First) in 'A' .. 'Z'
         then
--            Scanner.LHS_Alias  := Interfaces.C.Strings.New_String (X);
--            Scanner.LHS_Alias.Append (Interfaces.C.Strings.New_String (X));
            Scanner.LHS_Alias.Append (To_Alias (X));
            Scanner.State := LHS_ALIAS_2;
         else
            Parser_Error
              (E009, Scanner.Token_Lineno,
               (1 => To_Unbounded_String (X),
                2 => To_Unbounded_String
                  (Symbols.From_Key (Scanner.LHS.First_Element.Name))));
            Scanner.State := RESYNC_AFTER_RULE_ERROR;
         end if;

      when LHS_ALIAS_2 =>
         if X (X'First)  = ')' then
            Scanner.State := LHS_ALIAS_3;
         else
            Parser_Error (E010, Scanner.Token_Lineno,
                          (1 => Scanner.LHS_Alias.First_Element));
            Scanner.State := RESYNC_AFTER_RULE_ERROR;
         end if;


      when LHS_ALIAS_3 =>
         if X (X'First .. X'First + 2) = "::=" then
            Scanner.State := IN_RHS;
         else
            Parser_Error (E011, Scanner.Token_Lineno,
                          (1 => To_Unbounded_String
                             (Symbols.From_Key (Scanner.LHS.First_Element.Name)),
                           2 => Scanner.LHS_Alias.First_Element));
            Scanner.State := RESYNC_AFTER_RULE_ERROR;
         end if;


      when IN_RHS =>   Do_State_In_RHS (Lemon, Scanner);

      when RHS_ALIAS_1 =>

         if
           X (X'First) in 'a' .. 'z' or
           X (X'First) in 'A' .. 'Z'
         then
            Scanner.Alias.Append (To_Alias (X));
            Scanner.State   := RHS_ALIAS_2;

         else
            Parser_Error
              (E012, Scanner.Token_Lineno,
               (1 => To_Unbounded_String (X),
                2 => To_Unbounded_String
                  (Symbols.From_Key (Scanner.RHS.Last_Element.Name))));
            Scanner.State := RESYNC_AFTER_RULE_ERROR;

         end if;


      when RHS_ALIAS_2 =>

         if X (X'First) = ')' then
            Scanner.State := IN_RHS;

         else
            Parser_Error (E013, Scanner.Token_Lineno,
                          (1 => Scanner.LHS_Alias.First_Element));
            Scanner.State := RESYNC_AFTER_RULE_ERROR;

         end if;


      when WAITING_FOR_DECL_KEYWORD => Do_State_Waiting_For_Decl_Keyword (Lemon, Scanner);


      when WAITING_FOR_DESTRUCTOR_SYMBOL =>
         if
           X (X'First) not in 'a' .. 'z' and
           X (X'First) not in 'A' .. 'Z'
         then
            Parser_Error (E205, Line_Number => Scanner.Token_Lineno);
            Scanner.State := RESYNC_AFTER_DECL_ERROR;
         else
            declare
               use Symbols;

               Symbol : Symbol_Access := Create (X);
            begin
               Scanner.Decl_Arg_Slot     := new Unbounded_String'(Symbol.Destructor);
--                 new chars_ptr'(New_String (To_String (Symbol.Destructor))); -- XXX
               Scanner.Decl_Lineno_Slot  := Symbol.Dest_Lineno'Access;
               Scanner.Insert_Line_Macro := True;
            end;
            Scanner.State := WAITING_FOR_DECL_ARG;
         end if;


      when WAITING_FOR_DATATYPE_SYMBOL =>
         if
           X (X'First) not in 'a' .. 'z' and
           X (X'First) not in 'A' .. 'Z'
         then
            Parser_Error (E206, Line_Number => Scanner.Token_Lineno);
            Scanner.State := RESYNC_AFTER_DECL_ERROR;
         else
            declare
               use Symbols;

               Symbol : Symbol_Access := Find (X);
            begin
               if
                 Symbol /= null and then
                 Symbols."/=" (Symbol.Data_Type, Null_Unbounded_String)
               then
                  Parser_Error (E207,
                                Arguments   => (1 => To_Unbounded_String (X)),
                                Line_Number => Scanner.Token_Lineno);
                  Scanner.State := RESYNC_AFTER_DECL_ERROR;
               else
                  if Symbol = null then
                     Symbol := Create (X);
                  end if;
                  Scanner.Decl_Arg_Slot := new Unbounded_String'(Symbol.Data_Type);
                  --  new chars_ptr'(New_String (To_String (Symbol.Data_Type)));
                  Scanner.Insert_Line_Macro := False;
                  Scanner.State        := WAITING_FOR_DECL_ARG;
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

      when WAITING_FOR_DECL_ARG => Do_State_Waiting_For_Decl_Arg (Lemon, Scanner);

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
            Scanner.State := WAITING_FOR_DECL_OR_RULE;

         elsif C not in 'A' .. 'Z' then
            Parser_Error
              (E211,
               Line_Number => Scanner.Token_Lineno,
               Arguments   => (1 => To_Unbounded_String (X)));
         else
            declare
--               use Interfaces.C.Strings;
               use Symbols, Extras;

               Symbol : constant Symbol_Access := Create (X);
            begin
--               if Scanner.Gp.Wildcard = 0 then
--                  Scanner.Gp.wildcard := Symbol;
               if Get_Wildcard (Lemon.Extra) = null then
                  Set_Wildcard (Lemon.Extra, Symbol);
               else
                  Parser_Error
                    (E212,
                     Line_Number => Scanner.Token_Lineno,
                     Arguments   => (1 => To_Unbounded_String (X)));
               end if;
            end;
         end if;


      when WAITING_FOR_CLASS_ID =>
         declare
--            use Interfaces.C.Strings;
            use Symbols;
         begin
            if C not in 'a' .. 'z' then
               Parser_Error
                 (E209,
                  Line_Number => Scanner.Token_Lineno,
                  Arguments   => (1 => To_Unbounded_String (X)));
               Scanner.State := RESYNC_AFTER_DECL_ERROR;

            elsif Find (X) /= null then
               Parser_Error
                 (E210,
                  Line_Number => Scanner.Token_Lineno,
                  Arguments   => (1 => To_Unbounded_String (X)));
               Scanner.State := RESYNC_AFTER_DECL_ERROR;

            else
               Scanner.Token_Class      := Create (X);
               Scanner.Token_Class.Kind := Multi_Terminal;
               Scanner.State       := WAITING_FOR_CLASS_TOKEN;

            end if;
         end;

      when WAITING_FOR_CLASS_TOKEN =>

         if C = '.' then
            Scanner.State := WAITING_FOR_DECL_OR_RULE;

         elsif
           (C in 'A' .. 'Z') or
           ((C = '|' or C = '/') and
              X (X'First + 1) in 'A' .. 'Z')
         then
            declare
               use Symbols;

               Symbol : constant Symbol_Access := Scanner.Token_Class; -- ???
               First  : Natural := X'First;
            begin
               --  Symbol.N_Sub_Sym := Symbol.N_Sub_Sym + 1;
               --  Symbol.Sub_Sym := (struct symbol **) realloc(msp->subsym,
               --                    sizeof(struct symbol*)*msp->nsubsym);
               if C not in 'A' .. 'Z' then
                  First := X'First + 1;
               end if;
               --  Symbol.Sub_Sym (symbol.N_Sub_Sym - 1) := Lime_Symbol_New (X (First .. X'Last));
               Symbol.Sub_Sym.Append (Create (X (First .. X'Last)));
            end;
         else
            Parser_Error
              (E208,
               Line_Number => Scanner.Token_Lineno,
               Arguments   => (1 => To_Unbounded_String (X)));

            Scanner.State := RESYNC_AFTER_DECL_ERROR;

         end if;

      when RESYNC_AFTER_RULE_ERROR =>
         --  // if( x[0]=='.' ) psp->state = WAITING_FOR_DECL_OR_RULE;
         --  // break;
         null;

      when RESYNC_AFTER_DECL_ERROR =>
         case C is
            when '.' =>  Scanner.State := WAITING_FOR_DECL_OR_RULE;
            when '%' =>  Scanner.State := WAITING_FOR_DECL_KEYWORD;
            when others => null;
         end case;

      end case;
   end Do_State;


   procedure Initialize_FSM (Lemon   : in out Lemon_Record;
                             Scanner : in out Scanner_Record)
   is
   begin
      Scanner.Prev_Rule    := null;
      Scanner.Prec_Counter := 0;
      Scanner.First_Rule   := null;
      Scanner.Last_Rule    := null;

      Lemon.N_Rule  := 0;
      Scanner.State := WAITING_FOR_DECL_OR_RULE;
   end Initialize_FSM;


   procedure Do_State_Waiting_For_Decl_Or_Rule (Lemon   : in out Lemon_Record;
                                                Scanner : in out Scanner_Record)
   is
      use Rules;

      Cur : constant Character := Current_Char (Scanner);
      X   : constant String    := Current_Line (Scanner);
   begin
      Debug (True, "Do_State_Waiting_For_Decl_Or_Rule");
      Debug (True, "  Cur: " & Cur);
      Debug (True, "  X  : " & X);

      if Cur = '%' then
--         Advance (Scanner, By => 1);
         Scanner.State := WAITING_FOR_DECL_KEYWORD;

      elsif Cur in 'a' .. 'z' then
         Scanner.LHS.Append (Symbols.Create (X));
         --            PSP.N_RHS      := 0;

         Scanner.RHS        := Symbols.Symbol_Vectors.Empty_Vector;
         --            PSP.LHS_Alias  := Interfaces.C.Strings.Null_Ptr;
         Scanner.LHS_Alias  := Parser_Data.Alias_Vectors.Empty_Vector;
         Scanner.State := WAITING_FOR_ARROW;

      elsif Cur = '{' then

         if Scanner.Prev_Rule = null then
            Parser_Error (E001, Scanner.Token_Lineno);

         elsif Rules."/=" (Scanner.Prev_Rule.Code, Null_Code) then
            Parser_Error (E002, Scanner.Token_Lineno);

         else
            Scanner.Prev_Rule.Line := Scanner.Token_Lineno;
            Scanner.Prev_Rule.Code :=
              Unbounded_String'(To_Unbounded_String (X (X'First + 1 .. X'Last)));
            --  new Unbounded_String'(To_Unbounded_String (X (X'First + 1 .. X'Last)));
            Scanner.Prev_Rule.No_Code := False;
         end if;

      elsif Cur = '[' then
         Scanner.State := PRECEDENCE_MARK_1;

      else
         Parser_Error
           (E003, Scanner.Token_Lineno,
            (1 => To_Unbounded_String (X)));
      end if;
   end Do_State_Waiting_For_Decl_Or_Rule;


   procedure Do_State_Precedence_Mark_1 (Scanner : in out Scanner_Record)
   is
      Cur : constant Character := Current_Char (Scanner);
      X   : constant String    := Current_Line (Scanner);
   begin
      if Cur not in 'A' .. 'Z' then
         Parser_Error (E004, Scanner.Token_Lineno);

      elsif Scanner.Prev_Rule = null then
         Parser_Error (E005, Scanner.Token_Lineno,
                       (1 => To_Unbounded_String (X)));

      elsif Scanner.Prev_Rule.Prec_Sym /= null then
         Parser_Error (E006, Scanner.Token_Lineno);

      else
         Scanner.Prev_Rule.Prec_Sym :=
           Symbols.Create (X);
      end if;

      Scanner.State := PRECEDENCE_MARK_2;

   end Do_State_Precedence_Mark_1;


   procedure Do_State_Precedence_Mark_2 (Scanner : in out Scanner_Record)
   is
      Cur : constant Character := Current_Char (Scanner);
   begin
      if Cur /= ']' then
         --  Error ("Missing ']' on precedence mark.");
         Parser_Error (E007, Scanner.Token_Lineno);
      end if;

      Scanner.State := WAITING_FOR_DECL_OR_RULE;

   end Do_State_Precedence_Mark_2;


   procedure Do_State_Waiting_For_Decl_Keyword (Lemon   : in out Lemon_Record;
                                                Scanner : in out Scanner_Record)
   is
      function Match (Item : in String) return Boolean;

      Cur : constant Character := Current_Token_Char (Scanner);
      X   : constant String    := Current_Token_Line (Scanner);

      function Match (Item : in String) return Boolean
      is
         use Ada.Strings.Fixed;

         Length     : constant Natural := Natural'Min (X'Length, Item'Length);
         Item_Last  : constant Natural := Item'First + Length - 1;
         Right_Pos  : constant Natural := Index (Item (Item'First .. Item_Last), " ");
         Right_Last : constant Natural := Natural'Max (Right_Pos, Item_Last - 1);
         Length_2   : constant Natural := Natural'Min (X'Length, Item_Last - Item'First + 1);
         Left       : String renames X    (X'First    .. X'First    + Length_2 - 1);
         Right      : String renames Item (Item'First .. Item'First + Length_2 - 1);
      begin
         Debug (False, "    Left : " & Left);
         Debug (False, "    Right: " & Right);
         return Left = Right;
      end Match;

      Debug_On : constant Boolean := True;
   begin
      Debug (Debug_On, "Do_State_Waiting_For_Decl_Keyword");
      Debug (Debug_On, "  Cur: " & Cur);
      Debug (Debug_On, "  X  : " & X);

      if
        Cur in 'a' .. 'z' or
        Cur in 'A' .. 'Z'
      then
         Scanner.Decl_Keyword      := To_Unbounded_String (X);
         Scanner.Decl_Arg_Slot     := null;
         Scanner.Insert_Line_Macro := True;

         Scanner.State := WAITING_FOR_DECL_ARG;

         if Match ("name") then
            Scanner.Decl_Arg_Slot := Lemon.Names.Name'Access;
            Scanner.Insert_Line_Macro := False;

         elsif Match ("include") then
            Scanner.Decl_Arg_Slot := Lemon.Names.Include'Access;

         elsif Match ("code") then
            Scanner.Decl_Arg_Slot := Lemon.Names.Extra_Code'Access;

         elsif Match ("token_destructor") then
            Scanner.Decl_Arg_Slot := Lemon.Names.Token_Dest'Access;

         elsif Match ("default_destructor") then
            Scanner.Decl_Arg_Slot := Lemon.Names.Var_Dest'Access;

         elsif Match ("token_prefix") then
            Debug (True, "  token_prefix");
            Scanner.Decl_Arg_Slot     := Lemon.Names.Token_Prefix'Access;
            Scanner.Insert_Line_Macro := False;
            --  Advance_Until_After_Space (Scanner);

         elsif Match ("syntax_error") then
            Scanner.Decl_Arg_Slot := Lemon.Names.Error'Access;

         elsif Match ("parse_accept") then
            Scanner.Decl_Arg_Slot := Lemon.Names.C_Accept'Access;

         elsif Match ("parse_failure") then
            Scanner.Decl_Arg_Slot := Lemon.Names.Failure'Access;

         elsif Match ("stack_overflow") then
            Scanner.Decl_Arg_Slot := Lemon.Names.Overflow'Access;

         elsif Match ("extra_argument") then
            Scanner.Decl_Arg_Slot     := Lemon.Names.ARG2'Access;
            Scanner.Insert_Line_Macro := False;

         elsif Match ("extra_context") then
            Debug (True, "  extra_context");
            Scanner.Decl_Arg_Slot     := Lemon.Names.CTX2'Access;
            Scanner.Insert_Line_Macro := False;

         elsif Match ("token_type") then
            Debug (True, "  token_type");
            Scanner.Decl_Arg_Slot     := Lemon.Names.Token_Type'Access;
            Scanner.Insert_Line_Macro := False;

         elsif Match ("default_type") then
            Debug (True, "  default_type");
            Scanner.Decl_Arg_Slot     := Lemon.Names.Var_Type'Access;
            Scanner.Insert_Line_Macro := False;

         elsif Match ("stack_size") then
            Scanner.Decl_Arg_Slot     := Lemon.Names.Stack_Size'Access;
            Scanner.Insert_Line_Macro := False;

         elsif Match ("start_symbol") then
            Scanner.Decl_Arg_Slot     := Lemon.Names.Start'Access;
            Scanner.Insert_Line_Macro := False;

         elsif Match ("left") then
            Scanner.Prec_Counter := Scanner.Prec_Counter + 1;
            Scanner.Decl_Assoc   := Symbols.Left;
            Scanner.State   := WAITING_FOR_PRECEDENCE_SYMBOL;

         elsif Match ("right") then
            Scanner.Prec_Counter := Scanner.Prec_Counter + 1;
            Scanner.Decl_Assoc   := Symbols.Right;
            Scanner.State   := WAITING_FOR_PRECEDENCE_SYMBOL;

         elsif Match ("nonassoc") then
            Scanner.Prec_Counter := Scanner.Prec_Counter + 1;
            Scanner.Decl_Assoc   := Symbols.None;
            Scanner.State   := WAITING_FOR_PRECEDENCE_SYMBOL;

         elsif Match ("destructor") then
            Scanner.State := WAITING_FOR_DESTRUCTOR_SYMBOL;

         elsif Match ("type") then
            Scanner.State := WAITING_FOR_DATATYPE_SYMBOL;

         elsif Match ("fallback") then
            Scanner.Fallback := null;
            Scanner.State := WAITING_FOR_FALLBACK_ID;

         elsif Match ("token") then
            Scanner.State := WAITING_FOR_TOKEN_NAME;

         elsif Match ("wildcard") then
            Scanner.State := WAITING_FOR_WILDCARD_ID;

         elsif Match ("token_class") then
            Scanner.State := WAITING_FOR_CLASS_ID;

         else
            Parser_Error
              (E203, Scanner.Token_Lineno,
               Arguments => (1 => To_Unbounded_String (X)));

            Scanner.State := RESYNC_AFTER_DECL_ERROR;
         end if;
      else
         Parser_Error
           (E204, Scanner.Token_Lineno,
            Arguments => (1 => To_Unbounded_String (X)));

         Scanner.State := RESYNC_AFTER_DECL_ERROR;
      end if;

   end Do_State_Waiting_For_Decl_Keyword;


   procedure Do_State_In_RHS (Lemon  : in out Lemon_Record;
                              Scanner : in out Scanner_Record)
   is
      Cur : constant Character := Current_Char (Scanner);
      X   : constant String    := Current_Line (Scanner);
   begin
      if Cur = '.' then
         declare
            use Symbols;
            use Rules;

            Rule : constant access Rule_Record := new Rule_Record;
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
--            Rule.Next_LHS   := Rule.LHS.Rule;
--            Rule.LHS.Rule   := Rule;
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
         Scanner.State := WAITING_FOR_DECL_OR_RULE;

      elsif
        Cur in 'a' .. 'z' or
        Cur in 'A' .. 'Z'
      then
         Scanner.RHS  .Append (Symbols.Create (X));
         Scanner.Alias.Append (Null_Unbounded_String);
         --            end if;

      elsif
        (Cur = '|' or Cur = '/') and not
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
              (Symbols.Create (X (X'First + 1 .. X'Last)));

            if
              X (X'First + 1) in 'a' .. 'z' or
              To_String (Symbol.Sub_Sym.First_Element.Name) (1) in 'a' .. 'z'
            then
               Parser_Error (E201, Line_Number => Scanner.Token_Lineno);
            end if;
         end;

      elsif Cur = '(' and not Scanner.RHS.Is_Empty then
         Scanner.State := RHS_ALIAS_1;

      else
         Parser_Error (E202, Scanner.Token_Lineno,
                       (1 => To_Unbounded_String (X)));
         Scanner.State := RESYNC_AFTER_RULE_ERROR;
      end if;
   end Do_State_In_RHS;


   procedure Do_State_Waiting_For_Decl_Arg (Lemon   : in     Lemon_Record;
                                            Scanner : in out Scanner_Record)
   is
      Cur : constant Character := Current_Token_Char (Scanner);
      X   : constant String    := Current_Token_Line (Scanner);

      Debug_On : constant Boolean := True;
   begin
      Debug (Debug_On, "##Cur: " & Cur);
      Debug (Debug_On, "##X  : " & X);
      if
        Cur = '{' or
        Cur = '"' or
        Cur in 'a' .. 'z' or
        Cur in 'A' .. 'Z' or
        Cur in '0' .. '9'
      then
         declare
--          const char *zOld, *zNew;
--          char *zBuf, *z;
--          int nOld, n, nLine = 0, nNew, nBack;
            N    : Integer;
            Back : Integer;
            New_String     : constant String := X;
            Old_String     : Unbounded_String;
            Buf_String     : Unbounded_String;
            Z              : Unbounded_String;
            New_First      : Positive := New_String'First;
            Old_Length     : Natural;
            Buf_Length     : Natural;
            Z_Pos          : Natural;
            Add_Line_Macro : Boolean;
            Line           : Unbounded_String;
         begin
            if
              New_String (New_First) = '"' or
              New_String (New_First) = '{'
            then
               New_First := New_First + 1;
            end if;
            --  nNew := LemonStrlen (zNew);

            if Scanner.Decl_Arg_Slot /= null then  --  A_Declaration (Null_Unbounded_String) then
               Old_String := Scanner.Decl_Arg_Slot.all;
            else
               Old_String := To_Unbounded_String ("");
            end if;
            --  nOld = lemonStrlen(zOld);
            N := Old_Length + New_First + 20;

            Add_Line_Macro :=
              --  not Scanner.Gp.No_Linenos_Flag and
              not Lemon.No_Linenos_Flag and
              Scanner.Insert_Line_Macro and
              (Scanner.Decl_Lineno_Slot = null or
                 Scanner.Decl_Lineno_Slot.all /= 0);

            --
            --  Add line macro
            --
            if Add_Line_Macro then
               Z := Scanner.File_Name;
               Z_Pos := 1;
               Back  := 0;
               while Z_Pos <= Length (Z) loop
                  if Element (Z, Z_Pos) = '\' then
                     Back := Back + 1;
                  end if;
                  Z_Pos := Z_Pos + 1;
               end loop;
               Line := To_Unbounded_String ("#line ");
               Append (Line, Positive'Image (Scanner.Token_Lineno));
               Append (Line, " ");
               N := N + Length (Line) + Length (Scanner.File_Name) + Back;
            end if;

            --  Scanner.Decl_Arg_Slot = (char *) realloc(Scanner.Decl_Arg_Slot, n);
            Buf_String := Scanner.Decl_Arg_Slot.all; --   + nOld;
            if Add_Line_Macro then
               --               if
--                 Old_Length /= 0 and
--                 Element (Buf_String, -1) /= ASCII.LF
--               then
--                  --  *(zBuf++) := ASCII.NL;
--                  Append (Buf_String, ASCII.LF);
--               end if;

               --  Append line feed to Buf is missing at end
               if
                 (Length (Old_String) /= 0 and Length (Buf_String) /= 0) and then
                 Element (Buf_String, Length (Buf_String)) /= ASCII.LF
               then
                  Append (Buf_String, ASCII.LF);
               end if;

               Append (Buf_String, Line);
--               Put_Line ("## XX");
--               Put_Line (To_String (Buf_String));
               --  Buf_String := Buf_String; -- + nLine;
               --  *(zBuf++) = '"';
               Append (Buf_String, '"');
               Append (Z, Scanner.File_Name);
               while Z_Pos <= Length (Z) loop
                  if Element (Z, Z_Pos) = '\' then
                     Append (Buf_String, '\');
                  end if;
                  --  *(zBuf++) = *z;
                  Append (Buf_String, Element (Z, Z_Pos));
                  Z_Pos := Z_Pos + 1;
               end loop;
               --  *(zBuf++) := '"';
               --  *(zBuf++) := ASCII.NL;
               Append (Buf_String, '"');
               Append (Buf_String, ASCII.LF);
            end if;

            if
              Scanner.Decl_Lineno_Slot /= null and
              Scanner.Decl_Lineno_Slot.all = 0
            then
               Scanner.Decl_Lineno_Slot.all := Scanner.Token_Lineno;
            end if;
            Buf_String := To_Unbounded_String (New_String);
            --  Buf_String := Buf_String; --   + nNew;
            --  *zBuf := 0;
            Debug (True, "QQQ" & To_String (Buf_String));
            Scanner.State := WAITING_FOR_DECL_OR_RULE;
            Scanner.Done  := True;
         end;
      else
         Parser_Error
           (E213,
            Line_Number => Scanner.Token_Lineno,
            Arguments   => (1 => Scanner.Decl_Keyword,
                            2 => To_Unbounded_String (X)));

         Scanner.State := RESYNC_AFTER_DECL_ERROR;
      end if;

   end  Do_State_Waiting_For_Decl_Arg;


   procedure Debug (On   : in Boolean;
                    Text : in String)
   is
      use Ada.Text_IO;
   begin
      if On then
         Put_Line (Text);
      end if;
   end Debug;


end Parser_FSM;
