--
--  The author disclaims copyright to this source code.  In place of
--  a legal notice, here is a blessing:
--
--    May you do good and not evil.
--    May you find forgiveness for yourself and forgive others.
--    May you share freely, not taking more than you give.
--

with Ada.Strings.Unbounded;

with Types;
with Symbols;
with Errors;
with Rules;

package body Parser_FSM is

   use Sessions;
   use Parser_Data;

   procedure Do_State_Waiting_For_Decl_Or_Rule (Session : in out Session_Type;
                                                Scanner : in out Scanner_Record;
                                                Token   : in     String);

   procedure Do_State_Precedence_Mark_1 (Scanner : in out Scanner_Record;
                                         Token   : in     String);

   procedure Do_State_Precedence_Mark_2 (Scanner : in out Scanner_Record;
                                         Token   : in     String);

   procedure Do_State_Waiting_For_Decl_Keyword (Session : in out Session_Type;
                                                Scanner : in out Scanner_Record;
                                                Token   : in     String);

   procedure Do_State_Waiting_For_Destructor_Symbol (Session : in out Session_Type;
                                                     Scanner : in out Scanner_Record;
                                                     Token   : in     String);

   procedure Do_State_Waiting_For_Decl_Arg (Session : in     Session_Type;
                                            Scanner : in out Scanner_Record;
                                            Token   : in     String);

   procedure Do_State_Waiting_For_Arrow (Scanner : in out Scanner_Record;
                                         Token   : in     String);

   procedure Do_State_LHS_Alias_1 (Scanner : in out Scanner_Record;
                                   Token   : in     String);

   procedure Do_State_LHS_Alias_2 (Scanner : in out Scanner_Record;
                                   Token   : in     String);

   procedure Do_State_LHS_Alias_3 (Scanner : in out Scanner_Record;
                                   Token   : in     String);

   procedure Do_State_RHS_Alias_1 (Scanner : in out Scanner_Record;
                                   Token   : in     String);

   procedure Do_State_RHS_Alias_2 (Scanner : in out Scanner_Record;
                                   Token   : in     String);


   procedure Do_State_In_RHS (Session : in out Session_Type;
                              Scanner : in out Scanner_Record;
                              Token   : in     String);

   procedure Do_State_Waiting_For_Datatype_Symbol (Session : in out Session_Type;
                                                   Scanner : in out Scanner_Record;
                                                   Token   : in     String);

   procedure Do_State_Waiting_For_Precedence_Symbol (Session : in out Session_Type;
                                                     Scanner : in out Scanner_Record;
                                                     Token   : in     String);

   procedure Do_State_Waiting_For_Fallback_Id (Session : in out Session_Type;
                                               Scanner : in out Scanner_Record;
                                               Token   : in     String);

   procedure Do_State_Waiting_For_Token_Name (Scanner : in out Scanner_Record;
                                              Token   : in     String);

   procedure Do_State_Waiting_For_Wildcard_Id (Session : in out Session_Type;
                                               Scanner : in out Scanner_Record;
                                               Token   : in     String);

   procedure Do_State_Waiting_For_Class_Id (Scanner : in out Scanner_Record;
                                            Token   : in     String);

   procedure Do_State_Waiting_For_Class_Token (Scanner : in out Scanner_Record;
                                               Token   : in     String);


   --
   --
   --

   use Errors;
   use Ada.Strings.Unbounded;


   procedure Do_State (Session : in out Session_Type;
                       Scanner : in out Scanner_Record;
                       Token   : in     String)
   is
      C : Character renames Token (Token'First);
   begin

      case Scanner.State is

         when DUMMY =>
            null;

         when WAITING_FOR_DECL_OR_RULE =>
            Do_State_Waiting_For_Decl_Or_Rule (Session, Scanner, Token);

         when PRECEDENCE_MARK_1 =>
            Do_State_Precedence_Mark_1 (Scanner, Token);

         when PRECEDENCE_MARK_2 =>
            Do_State_Precedence_Mark_2 (Scanner, Token);

         when WAITING_FOR_ARROW =>
            Do_State_Waiting_For_Arrow (Scanner, Token);

         when LHS_ALIAS_1 =>
            Do_State_LHS_Alias_1 (Scanner, Token);

         when LHS_ALIAS_2 =>
            Do_State_LHS_Alias_2 (Scanner, Token);

         when LHS_ALIAS_3 =>
            Do_State_LHS_Alias_3 (Scanner, Token);

         when IN_RHS =>
            Do_State_In_RHS (Session, Scanner, Token);

         when RHS_ALIAS_1 =>
            Do_State_RHS_Alias_1 (Scanner, Token);

         when RHS_ALIAS_2 =>
            Do_State_RHS_Alias_2 (Scanner, Token);

         when WAITING_FOR_DECL_KEYWORD =>
            Do_State_Waiting_For_Decl_Keyword (Session, Scanner, Token);

         when WAITING_FOR_DESTRUCTOR_SYMBOL =>
            Do_State_Waiting_For_Destructor_Symbol (Session, Scanner, Token);

         when WAITING_FOR_DATATYPE_SYMBOL =>
            Do_State_Waiting_For_Datatype_Symbol (Session, Scanner, Token);

         when WAITING_FOR_PRECEDENCE_SYMBOL =>
            Do_State_Waiting_For_Precedence_Symbol (Session, Scanner, Token);

         when WAITING_FOR_DECL_ARG =>
            Do_State_Waiting_For_Decl_Arg (Session, Scanner, Token);

         when WAITING_FOR_FALLBACK_ID =>
            Do_State_Waiting_For_Fallback_Id (Session, Scanner, Token);

         when WAITING_FOR_TOKEN_NAME =>
            Do_State_Waiting_For_Token_Name (Scanner, Token);

         when WAITING_FOR_WILDCARD_ID =>
            Do_State_Waiting_For_Wildcard_Id (Session, Scanner, Token);

         when WAITING_FOR_CLASS_ID =>
            Do_State_Waiting_For_Class_Id (Scanner, Token);

         when WAITING_FOR_CLASS_TOKEN =>
            Do_State_Waiting_For_Class_Token (Scanner, Token);

      when RESYNC_AFTER_RULE_ERROR =>
         --  // if( x[0]=='.' ) psp->state = WAITING_FOR_DECL_OR_RULE;
         --  // break;
         null;

      when RESYNC_AFTER_DECL_ERROR =>
         case C is
            when '.' =>
               Scanner.State := WAITING_FOR_DECL_OR_RULE;
            when '%' =>
               Scanner.State := WAITING_FOR_DECL_KEYWORD;
            when others =>
               null;
         end case;

      end case;
   end Do_State;


   procedure Initialize_FSM (Session : in out Session_Type;
                             Scanner : in out Scanner_Record)
   is
      pragma Unreferenced (Session);
      use Rules.Rule_Lists;
   begin
      Scanner.Previous_Rule := No_Element;
      Scanner.Prec_Counter  := 0;
      Scanner.Rule          := Empty_List;
      Scanner.State         := WAITING_FOR_DECL_OR_RULE;
   end Initialize_FSM;


   procedure Do_State_Waiting_For_Decl_Or_Rule (Session : in out Session_Type;
                                                Scanner : in out Scanner_Record;
                                                Token   : in     String)
   is
      pragma Unreferenced (Session);
      use Rules;
      use Rules.Rule_Lists;

      Cur : Character renames Token (Token'First);
   begin

      if Cur = '%' then
         Scanner.State := WAITING_FOR_DECL_KEYWORD;

      elsif Cur in 'a' .. 'z' then
         Scanner.LHS.Clear;
         Scanner.LHS.Append (Symbols.Create (Token));
         Scanner.RHS.Clear;
         Scanner.LHS_Alias.Clear;
         Scanner.State := WAITING_FOR_ARROW;

      elsif Cur = '{' then

         if Scanner.Previous_Rule = No_Element then
            Parser_Error (E001, Scanner.Token_Lineno);

         elsif Rules."/=" (Element (Scanner.Previous_Rule).Code, Null_Code) then
            Parser_Error (E002, Scanner.Token_Lineno);

         else
            Element (Scanner.Previous_Rule).Line := Scanner.Token_Lineno;
            Element (Scanner.Previous_Rule).Code :=
              Unbounded_String'(To_Unbounded_String (Token (Token'First + 1 .. Token'Last)));
            Element (Scanner.Previous_Rule).No_Code := False;
         end if;

      elsif Cur = '[' then
         Scanner.State := PRECEDENCE_MARK_1;

      else
         Parser_Error  (E003, Scanner.Token_Lineno, Token);
      end if;
   end Do_State_Waiting_For_Decl_Or_Rule;


   procedure Do_State_Precedence_Mark_1 (Scanner : in out Scanner_Record;
                                         Token   : in     String)
   is
      use Rules;
      use Rules.Rule_Lists;
   begin
      if Token (Token'First) not in 'A' .. 'Z' then
         Parser_Error (E004, Scanner.Token_Lineno);

      elsif Scanner.Previous_Rule = No_Element then
         Parser_Error (E005, Scanner.Token_Lineno, Token);

      elsif Element (Scanner.Previous_Rule).Prec_Symbol /= null then
         Parser_Error (E006, Scanner.Token_Lineno);

      else
         Element (Scanner.Previous_Rule).Prec_Symbol :=
           Rule_Symbol_Access (Symbols.Create (Token));
      end if;

      Scanner.State := PRECEDENCE_MARK_2;

   end Do_State_Precedence_Mark_1;


   procedure Do_State_Precedence_Mark_2 (Scanner : in out Scanner_Record;
                                         Token   : in     String)
   is
   begin
      if Token (Token'First) /= ']' then
         Parser_Error (E007, Scanner.Token_Lineno);
      end if;

      Scanner.State := WAITING_FOR_DECL_OR_RULE;

   end Do_State_Precedence_Mark_2;


   procedure Do_State_Waiting_For_Decl_Keyword (Session : in out Session_Type;
                                                Scanner : in out Scanner_Record;
                                                Token   : in     String)
   is
      Cur : Character renames Token (Token'First);
   begin

      if
        Cur in 'a' .. 'z' or
        Cur in 'A' .. 'Z'
      then
         Scanner.Decl_Keyword      := To_Unbounded_String (Token);
         Scanner.Decl_Arg_Slot     := null;
         Scanner.Insert_Line_Macro := True;

         Scanner.State := WAITING_FOR_DECL_ARG;

         if Token = "name" then
            Scanner.Decl_Arg_Slot := Session.Names.Name'Access;
            Scanner.Insert_Line_Macro := False;

         elsif Token = "include" then
            Scanner.Decl_Arg_Slot := Session.Names.Include'Access;

         elsif Token = "code" then
            Scanner.Decl_Arg_Slot := Session.Names.Extra_Code'Access;

         elsif Token = "token_destructor" then
            Scanner.Decl_Arg_Slot := Session.Names.Token_Dest'Access;

         elsif Token = "default_destructor" then
            Scanner.Decl_Arg_Slot := Session.Names.Var_Dest'Access;

         elsif Token = "token_prefix" then
            Scanner.Decl_Arg_Slot     := Session.Names.Token_Prefix'Access;
            Scanner.Insert_Line_Macro := False;

         elsif Token = "syntax_error" then
            Scanner.Decl_Arg_Slot := Session.Names.Error'Access;

         elsif Token = "parse_accept" then
            Scanner.Decl_Arg_Slot := Session.Names.C_Accept'Access;

         elsif Token = "parse_failure" then
            Scanner.Decl_Arg_Slot := Session.Names.Failure'Access;

         elsif Token = "stack_overflow" then
            Scanner.Decl_Arg_Slot := Session.Names.Overflow'Access;

         elsif Token = "extra_argument" then
            Scanner.Decl_Arg_Slot     := Session.Names.ARG2'Access;
            Scanner.Insert_Line_Macro := False;

         elsif Token = "extra_context" then
            Scanner.Decl_Arg_Slot     := Session.Names.CTX2'Access;
            Scanner.Insert_Line_Macro := False;

         elsif Token = "token_type" then
            Scanner.Decl_Arg_Slot     := Session.Names.Token_Type'Access;
            Scanner.Insert_Line_Macro := False;

         elsif Token = "default_type" then
            Scanner.Decl_Arg_Slot     := Session.Names.Var_Type'Access;
            Scanner.Insert_Line_Macro := False;

         elsif Token = "stack_size" then
            Scanner.Decl_Arg_Slot     := Session.Names.Stack_Size'Access;
            Scanner.Insert_Line_Macro := False;

         elsif Token = "start_symbol" then
            Scanner.Decl_Arg_Slot     := Session.Names.Start'Access;
            Scanner.Insert_Line_Macro := False;

         elsif Token = "left" then
            Scanner.Prec_Counter     := Scanner.Prec_Counter + 1;
            Scanner.Decl_Association := Symbols.Left_Association;
            Scanner.State   := WAITING_FOR_PRECEDENCE_SYMBOL;

         elsif Token = "right" then
            Scanner.Prec_Counter     := Scanner.Prec_Counter + 1;
            Scanner.Decl_Association := Symbols.Right_Association;
            Scanner.State   := WAITING_FOR_PRECEDENCE_SYMBOL;

         elsif Token = "nonassoc" then
            Scanner.Prec_Counter     := Scanner.Prec_Counter + 1;
            Scanner.Decl_Association := Symbols.No_Association;
            Scanner.State   := WAITING_FOR_PRECEDENCE_SYMBOL;

         elsif Token = "destructor" then
            Scanner.State := WAITING_FOR_DESTRUCTOR_SYMBOL;

         elsif Token = "type" then
            Scanner.State := WAITING_FOR_DATATYPE_SYMBOL;

         elsif Token = "fallback" then
            Scanner.Fallback := null;
            Scanner.State := WAITING_FOR_FALLBACK_ID;

         elsif Token = "token" then
            Scanner.State := WAITING_FOR_TOKEN_NAME;

         elsif Token = "wildcard" then
            Scanner.State := WAITING_FOR_WILDCARD_ID;

         elsif Token = "token_class" then
            Scanner.State := WAITING_FOR_CLASS_ID;

         else
            Parser_Error (E203, Scanner.Token_Lineno, Token);

            Scanner.State := RESYNC_AFTER_DECL_ERROR;
         end if;
      else
         Parser_Error (E204, Scanner.Token_Lineno, Token);

         Scanner.State := RESYNC_AFTER_DECL_ERROR;
      end if;

   end Do_State_Waiting_For_Decl_Keyword;


   procedure Do_State_Waiting_For_Destructor_Symbol (Session : in out Session_Type;
                                                     Scanner : in out Scanner_Record;
                                                     Token   : in     String)
   is
      pragma Unreferenced (Session);
   begin
      if
        Token (Token'First) not in 'a' .. 'z' and
        Token (Token'First) not in 'A' .. 'Z'
      then
         Parser_Error (E205, Scanner.Token_Lineno);
         Scanner.State := RESYNC_AFTER_DECL_ERROR;
      else
         declare
            use Symbols;

            Symbol : Symbol_Access := Create (Token);
         begin
            Scanner.Decl_Arg_Slot     := new Unbounded_String'(Symbol.Destructor);
            Scanner.Decl_Lineno_Slot  := Symbol.Dest_Lineno'Access;
            Scanner.Insert_Line_Macro := True;
         end;
         Scanner.State := WAITING_FOR_DECL_ARG;
      end if;

   end Do_State_Waiting_For_Destructor_Symbol;


   procedure Do_State_Waiting_For_Arrow (Scanner : in out Scanner_Record;
                                         Token   : in     String)
   is
   begin

      if
        Token'Length >= 3 and then
        Token (Token'First .. Token'First + 2) = "::="
      then
         Scanner.State := IN_RHS;

      elsif Token (Token'First) = '(' then
         Scanner.State := LHS_ALIAS_1;

      else
         declare
            use Symbols;
         begin
            Parser_Error (E008, Scanner.Token_Lineno,
                          Name_Of (Scanner.LHS.First_Element));
            Scanner.State := RESYNC_AFTER_RULE_ERROR;
         end;
      end if;
   end Do_State_Waiting_For_Arrow;


   procedure Do_State_LHS_Alias_1 (Scanner : in out Scanner_Record;
                                   Token   : in     String)
   is
   begin
      if
        Token (Token'First) in 'a' .. 'z' or
        Token (Token'First) in 'A' .. 'Z'
      then
         Scanner.LHS_Alias.Clear;
         Scanner.LHS_Alias.Append (To_Alias (Token));
         Scanner.State := LHS_ALIAS_2;
      else
         Parser_Error
           (E009, Scanner.Token_Lineno,
            Argument_1 => Token,
            Argument_2 => Symbols.Name_Of (Scanner.LHS.First_Element));
         Scanner.State := RESYNC_AFTER_RULE_ERROR;
      end if;
   end Do_State_LHS_Alias_1;


   procedure Do_State_LHS_Alias_2 (Scanner : in out Scanner_Record;
                                   Token   : in     String)
   is
   begin
      if Token (Token'First)  = ')' then
         Scanner.State := LHS_ALIAS_3;
      else
         Parser_Error (E010, Scanner.Token_Lineno,
                       To_String (Scanner.LHS_Alias.First_Element));
         Scanner.State := RESYNC_AFTER_RULE_ERROR;
      end if;
   end Do_State_LHS_Alias_2;


   procedure Do_State_LHS_Alias_3 (Scanner : in out Scanner_Record;
                                   Token   : in     String)
   is
   begin
      if Token (Token'First .. Token'First + 2) = "::=" then
         Scanner.State := IN_RHS;
      else
         Parser_Error
           (E011, Scanner.Token_Lineno,
            Argument_1 => Symbols.Name_Of (Scanner.LHS.First_Element),
            Argument_2 => To_String (Scanner.LHS_Alias.First_Element));
         Scanner.State := RESYNC_AFTER_RULE_ERROR;
      end if;
   end Do_State_LHS_Alias_3;


   procedure Do_State_RHS_Alias_1 (Scanner : in out Scanner_Record;
                                   Token   : in     String)
   is
   begin
      if
        Token (Token'First) in 'a' .. 'z' or
        Token (Token'First) in 'A' .. 'Z'
      then
         Scanner.Alias.Append (To_Alias (Token));
         Scanner.State   := RHS_ALIAS_2;

      else
         Parser_Error
           (E012, Scanner.Token_Lineno,
            Argument_1 => Token,
            Argument_2 => Symbols.Name_Of (Scanner.RHS.Last_Element));
         Scanner.State := RESYNC_AFTER_RULE_ERROR;

      end if;
   end Do_State_RHS_Alias_1;


   procedure Do_State_RHS_Alias_2 (Scanner : in out Scanner_Record;
                                   Token   : in     String)
   is
   begin
      if Token (Token'First) = ')' then
         Scanner.State := IN_RHS;

      else
         Parser_Error (E013, Scanner.Token_Lineno,
                       To_String (Scanner.LHS_Alias.First_Element));
         Scanner.State := RESYNC_AFTER_RULE_ERROR;
      end if;
   end Do_State_RHS_Alias_2;


   procedure Do_State_In_RHS (Session : in out Session_Type;
                              Scanner : in out Scanner_Record;
                              Token   : in     String)
   is
      pragma Unreferenced (Session);
      Cur : Character renames Token (Token'First);
   begin
      if Cur = '.' then
         declare
            use Rules;
            use Rules.Alias_Vectors;

            Rule : Rule_Access;
         begin
            Rule := new Rule_Record;
            Rule.Rule_Line := Scanner.Token_Lineno;

            for I in Scanner.RHS.First_Index .. Scanner.RHS.Last_Index loop
               Rule.RHS.Append (Rule_Symbol_Access (Scanner.RHS.Element (I)));
               if I in Scanner.Alias.First_Index .. Scanner.Alias.Last_Index then
                  Rule.RHS_Alias.Append (Scanner.Alias.Element (I));
               else
                  Rule.RHS_Alias.Append (Null_Unbounded_String);
               end if;

               if Length (Rule.RHS_Alias.Element (Dot_Type (I))) /= 0 then
                  declare
                     Symbol : constant Rule_Symbol_Access := Rule.RHS (Dot_Type (I));
                  begin
                     Symbol.Content := True;
                  end;
               end if;
            end loop;

            Rule.LHS := Rule_Symbol_Access (Scanner.LHS.First_Element);
            if Scanner.LHS_Alias.Is_Empty then
               Rule.LHS_Alias := Null_Unbounded_String;
            else
               Rule.LHS_Alias := Scanner.LHS_Alias.First_Element;
            end if;
            Rule.Code        := Null_Code;
            Rule.No_Code     := True;
            Rule.Prec_Symbol := null;
            Rule.Index       := Index_Number (Scanner.Rule.Length);

            declare
               use Symbols;
            begin
               Rule.Next_LHS := Rule_Access (Symbol_Access (Rule.LHS).Rule);
               Symbol_Access (Rule.LHS).Rule := Rule;
            end;

            Scanner.Rule.Append (Rule);
            Scanner.Previous_Rule := Scanner.Rule.Last;
         end;
         Scanner.State := WAITING_FOR_DECL_OR_RULE;

      elsif
        Cur in 'a' .. 'z' or
        Cur in 'A' .. 'Z'
      then
         Scanner.RHS  .Append (Symbols.Create (Token));
         Scanner.Alias.Append (Null_Unbounded_String);

      elsif
        (Cur = '|' or Cur = '/') and not
        Scanner.RHS.Is_Empty
      then
         declare
            use Symbols;
            Symbol : Symbol_Access := Scanner.RHS.Last_Element;
         begin
            if Symbol.Kind /= Multi_Terminal then
               declare
                  Orig_Symbol : constant Symbol_Access := Symbol;
               begin
                  Symbol := new Symbol_Record;
                  Symbol.Kind := Multi_Terminal;
                  Symbol.Sub_Symbol.Append (Orig_Symbol);

                  Symbol.Name := Orig_Symbol.Name;

                  Scanner.RHS.Delete_Last;
                  Scanner.RHS.Append (Symbol);
               end;
            end if;

            Symbol.Sub_Symbol.Append
              (Symbols.Create (Token (Token'First + 1 .. Token'Last)));

            if
              Token (Token'First + 1) in 'a' .. 'z' or
              To_String (Symbol.Sub_Symbol.First_Element.Name) (1) in 'a' .. 'z'
            then
               Parser_Error (E201, Scanner.Token_Lineno);
            end if;
         end;

      elsif Cur = '(' and not Scanner.RHS.Is_Empty then
         Scanner.State := RHS_ALIAS_1;

      else
         Parser_Error (E202, Scanner.Token_Lineno, Token);
         Scanner.State := RESYNC_AFTER_RULE_ERROR;
      end if;
   end Do_State_In_RHS;


   procedure Do_State_Waiting_For_Decl_Arg (Session : in     Session_Type;
                                            Scanner : in out Scanner_Record;
                                            Token   : in     String)
   is
      Cur : Character renames Token (Token'First);
   begin
      if
        Cur = '{' or
        Cur = '"' or
        Cur in 'a' .. 'z' or
        Cur in 'A' .. 'Z' or
        Cur in '0' .. '9'
      then
         declare
            use type Types.Line_Number;

            N    : Integer;
            Back : Integer;
            New_String     : constant String := Token;
            Old_String     : Unbounded_String;
            Buf_String     : Unbounded_String;
            Z              : Unbounded_String;
            New_First      : Positive := New_String'First;
            Old_Length     : Natural;
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

            if Scanner.Decl_Arg_Slot = null then
               Old_String := To_Unbounded_String ("");
            else
               Old_String := Scanner.Decl_Arg_Slot.all;
            end if;
            Old_Length := Length (Old_String);
            N := Old_Length + New_First + 20;

            Add_Line_Macro :=
              --  not Scanner.Gp.No_Linenos_Flag and
              not Session.No_Linenos_Flag and
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
               Append (Line, Types.Line_Number'Image (Scanner.Token_Lineno));
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
            Scanner.State := WAITING_FOR_DECL_OR_RULE;
--            Scanner.Done  := True;
         end;
      else
         Parser_Error (E213, Scanner.Token_Lineno,
                       Argument_1 => To_String (Scanner.Decl_Keyword),
                       Argument_2 => Token);
         Scanner.State := RESYNC_AFTER_DECL_ERROR;
      end if;

   end Do_State_Waiting_For_Decl_Arg;


   procedure Do_State_Waiting_For_Datatype_Symbol (Session : in out Session_Type;
                                                   Scanner : in out Scanner_Record;
                                                   Token   : in     String)
   is
      pragma Unreferenced (Session);
   begin
      if
        Token (Token'First) not in 'a' .. 'z' and
        Token (Token'First) not in 'A' .. 'Z'
      then
         Parser_Error (E206, Scanner.Token_Lineno);
         Scanner.State := RESYNC_AFTER_DECL_ERROR;
      else
         declare
            use Symbols;

            Symbol : Symbol_Access := Find (Token);
         begin
            if
              Symbol /= null and then
              Symbol.Data_Type /= Null_Unbounded_String
            then
               Parser_Error (E207, Scanner.Line, Token);
               Scanner.State := RESYNC_AFTER_DECL_ERROR;
            else
               if Symbol = null then
                  Symbol := Create (Token);
               end if;
               Scanner.Decl_Arg_Slot
                 := Symbol.Data_Type'Access;
               --  Scanner.Decl_Arg_Slot := new Unbounded_String'(Symbol.Data_Type);
               --  new chars_ptr'(New_String (To_String (Symbol.Data_Type)));
               Scanner.Insert_Line_Macro := False;
               Scanner.State        := WAITING_FOR_DECL_ARG;
            end if;
         end;
      end if;
   end Do_State_Waiting_For_Datatype_Symbol;


   procedure Do_State_Waiting_For_Precedence_Symbol (Session : in out Session_Type;
                                                     Scanner : in out Scanner_Record;
                                                     Token   : in     String)
   is
      pragma Unreferenced (Session);
   begin
      if Token (Token'First) = '.' then
         Scanner.State := WAITING_FOR_DECL_OR_RULE;

      elsif Token (Token'First) in 'A' .. 'Z' then
         declare
            use Symbols;

            Symbol : constant Symbol_Access := Create (Token);
         begin
            if Symbol.Precedence >= 0 then
               Parser_Error (E217, Scanner.Token_Lineno, Token);
            else
               Symbol.Precedence  := Scanner.Prec_Counter;
               Symbol.Association := Scanner.Decl_Association;
            end if;
         end;
      else
         Parser_Error (E218, Scanner.Token_Lineno, Token);
      end if;

   end Do_State_Waiting_For_Precedence_Symbol;


   procedure Do_State_Waiting_For_Fallback_Id (Session : in out Session_Type;
                                               Scanner : in out Scanner_Record;
                                               Token   : in     String)
   is
   begin
      if Token (Token'First) = '.' then
         Scanner.State := WAITING_FOR_DECL_OR_RULE;

      elsif Token (Token'First) not in 'A' .. 'Z' then
         Parser_Error (E215, Scanner.Token_Lineno, Token);
      else
         declare
            use Symbols;
            Symbol : constant Symbol_Access := Create (Token);
         begin

            if Scanner.Fallback = null then
               Scanner.Fallback := Symbol;
            elsif Symbol.Fallback /= null then
               Parser_Error (E216, Scanner.Token_Lineno, Token);
            else
               Symbol.Fallback := Scanner.Fallback;
               Session.Has_Fallback := True;
               --  Scanner.Gp.Has_Fallback := True;
            end if;
         end;
      end if;
   end Do_State_Waiting_For_Fallback_Id;


   procedure Do_State_Waiting_For_Token_Name (Scanner : in out Scanner_Record;
                                              Token   : in     String)
   is
   begin
      --  Tokens do not have to be declared before use.  But they can be
      --  in order to control their assigned integer number.  The number for
      --  each token is assigned when it is first seen.  So by including
      --
      --     %token ONE TWO THREE
      --
      --  early in the grammar file, that assigns small consecutive values
      --  to each of the tokens ONE TWO and THREE.

      if Token (Token'First) = '.' then
         Scanner.State := WAITING_FOR_DECL_OR_RULE;

      elsif Token (Token'First) not in 'A' .. 'Z' then
         Parser_Error (E214, Scanner.Token_Lineno, Token);
      else
         declare
            use Symbols;
            Dummy_Symbol : Symbol_Access;
         begin
            Dummy_Symbol := Create (Token);
         end;
      end if;
   end Do_State_Waiting_For_Token_Name;


   procedure Do_State_Waiting_For_Wildcard_Id (Session : in out Session_Type;
                                               Scanner : in out Scanner_Record;
                                               Token   : in     String)
   is
      C : Character renames Token (Token'First);
   begin

      if C = '.' then
         Scanner.State := WAITING_FOR_DECL_OR_RULE;

      elsif C not in 'A' .. 'Z' then
         Parser_Error (E211, Scanner.Token_Lineno, Token);
      else
         declare
            use Symbols;

            Symbol : constant Symbol_Access := Create (Token);
         begin
            if Session.Wildcard = null then
               Session.Wildcard := Symbol;
            else
               Parser_Error (E212, Scanner.Token_Lineno, Token);
            end if;
         end;
      end if;

   end Do_State_Waiting_For_Wildcard_Id;


   procedure Do_State_Waiting_For_Class_Id (Scanner : in out Scanner_Record;
                                            Token   : in     String)
   is
      use Symbols;

      C : Character renames Token (Token'First);
   begin
      if C not in 'a' .. 'z' then
         Parser_Error (E209, Scanner.Token_Lineno, Token);
         Scanner.State := RESYNC_AFTER_DECL_ERROR;

      elsif Find (Token) /= null then
         Parser_Error (E210, Scanner.Token_Lineno, Token);
         Scanner.State := RESYNC_AFTER_DECL_ERROR;

      else
         Scanner.Token_Class      := Create (Token);
         Scanner.Token_Class.Kind := Multi_Terminal;
         Scanner.State       := WAITING_FOR_CLASS_TOKEN;

      end if;
   end Do_State_Waiting_For_Class_Id;


   procedure Do_State_Waiting_For_Class_Token (Scanner : in out Scanner_Record;
                                               Token   : in     String)
   is
      C : Character renames Token (Token'First);
   begin

      if C = '.' then
         Scanner.State := WAITING_FOR_DECL_OR_RULE;

      elsif
        C in 'A' .. 'Z' or
        ((C = '|' or C = '/') and
           Token (Token'First + 1) in 'A' .. 'Z')
      then
         declare
            use Symbols;

            Symbol : constant Symbol_Access := Scanner.Token_Class;
            First  : Natural := Token'First;
         begin
            if C not in 'A' .. 'Z' then
               First := Token'First + 1;
            end if;
            Symbol.Sub_Symbol.Append (Create (Token (First .. Token'Last)));
         end;
      else
         Parser_Error (E208, Scanner.Token_Lineno, Token);
         Scanner.State := RESYNC_AFTER_DECL_ERROR;
      end if;
   end Do_State_Waiting_For_Class_Token;


end Parser_FSM;
