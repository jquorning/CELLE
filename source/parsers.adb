--
--  The author disclaims copyright to this source code.  In place of
--  a legal notice, here is a blessing:
--
--    May you do good and not evil.
--    May you find forgiveness for yourself and forgive others.
--    May you share freely, not taking more than you give.
--

with Ada.Text_IO;
with Ada.Strings.Unbounded;
with Ada.Characters.Latin_1;

with DK8543.Errors;

with Parser_Data;
with Parser_FSM;

with Macros;
with Errors;
with Rules;

package body Parsers is

   On_True : constant Boolean := False;

   use Parser_Data;

   --  Run the preprocessor over the input file text.  The global
   --  variables azDefine[0] through azDefine[nDefine-1] contains the
   --  names of all defined macros.  This routine looks for "%ifdef"
   --  and "%ifndef" and "%endif" and comments them out.  Text in
   --  between is also commented out as appropriate.


--   procedure Get_Line_Without_EOL_Comment (File    : in     Ada.Text_IO.File_Type;
--                                           Scanner : in out Scanner_Record);

--   procedure Parse_Current_Character (Lemon   : in out Lime.Lemon_Record;
--                                      Scanner : in out Scanner_Record);

--   procedure Parse_Current_Line (Lemon   : in out Lime.Lemon_Record;
--                                 Scanner : in out Scanner_Record);

--   procedure Parse_Quoted_Identifier (Scanner : in out Scanner_Record);

   procedure Parse_One_Token (Lemon   : in out Lime.Lemon_Record;
                              Scanner : in out Scanner_Record;
                              Token   : in     String);
   --  Parse a single Token.
   --  The token must be enclosed by Scanner.First and Scanner.Token

--   procedure Detect_Start_Of_C_Comment_Block (Scanner : in out Scanner_Record);


   procedure Debug (On    : in Boolean;
                    Image : in String);

   function Read_File (File_Name : in String) return String;
   --  Read file File_Name into a string with lines separated by Latin_1.LF


   function Read_File (File_Name : in String) return String is
      use Ada.Strings.Unbounded;
      use Ada.Text_IO;
      Buffer : Unbounded_String := Null_Unbounded_String;
      File   : File_Type;
   begin
      Open (File, In_File, File_Name);

      loop
         declare
            Line : constant String := Get_Line (File);
         begin
            Append (Buffer, Line);
            Append (Buffer, Ada.Characters.Latin_1.LF);
         end;
      end loop;

   exception
      when End_Error =>
         Close (File);
         Append (Buffer, Ada.Characters.Latin_1.NUL);
         return To_String (Buffer);

   end Read_File;

   --
   --
   --

--   Comment_CPP     : constant String := "//";
--   Comment_C_Begin : constant String := "/*";
--   Comment_C_End   : constant String := "*/";

   use Errors;

--     procedure Get_Line_Without_EOL_Comment (File    : in     Ada.Text_IO.File_Type;
--                                             Scanner : in out Scanner_Record)
--     is
--        use DK8543.Strings;
--     begin
--        Scanner.Token_First := 1;
--        Scanner.Token_Last := Scanner.Token_First - 1;
--        Ada.Text_IO.Get_Line (File, Scanner.Item, Last => Scanner.Last);
--        Scanner.Token_Lineno := Scanner.Token_Lineno + 1;
--        Utility.Strip_End_Of_Line (From  => Scanner.Item,
--                                   Strip => Comment_CPP,
--                                   Last  => Scanner.Last);
--     end Get_Line_Without_EOL_Comment;


--     procedure Parse_Buffer (Lemon   : in out Lime.Lemon_Record;
--                             Scanner : in out Scanner_Record;
--                             Buffer  : in     String)
--     is
--        use Ada.Strings.Unbounded;

--        Scanner.Line_Number := 1;
--        loop


--  --      Current : constant Character := Current_Token_Char (Scanner);
--  --      Line    : constant String    := Current_Token_Line (Scanner);

--        Head_On    : constant Boolean := True;
--        Advance_On : constant Boolean := True;
--     begin
--        Debug (Head_On, "Parse_Current_Character");
--  --      Debug (Head_On, "  Current: " & Current);
--  --      Debug (Head_On, "  Line (Line'First): " & Line (Line'First));

--        if Scanner.Token_Last > Scanner.Last then
--           Ada.Text_IO.Put_Line ("returning due to end of line");
--           return;
--        end if;

--  --      if Scanner.Token_Last < 1 then
--  --         Ada.Text_IO.Put_Line ("returning due to empty line");
--  --         return;
--  --      end if;

--        declare
--           Current : constant Character := Scanner.Item (Scanner.Token_Last + 1);
--           Line    : constant String    := Scanner.Item (Scanner.Token_Last + 1 .. Scanner.Last);
--        begin
--        --
--        --  Scan until end of token
--        --
--           case Scanner.Item (Scanner.Token_Last + 1) is
--  --      case Current is

--              when '"' =>                     --   String literals
--                 Scanner.Mode   := Quoted_Identifier;
--                 Scanner.Buffer := Null_Unbounded_String;

--              when '{' =>              --  A block of C code
--                 Debug (True, "  A block of C code");

--                 --  Build up the token in Scanner.Buffer. Use ASCII.LF as line separator.
--                 --  Passe it all to Parse_One_Token
--  --            Block_Of_C_Code := True;  --  Signal to Parse_One_Token
--                 Scanner.Mode    := C_Code_Block;
--                 Scanner.Buffer  := Null_Unbounded_String;
--                 Append (Scanner.Buffer, '{');
--                 --              declare
--  --                 Level : Natural := 1;
--  --                 Cur   : Character;
--  --              begin
--  --                 Scanner.Token_Last := Scanner.Token_Last + 1;
--  --                 Append (Scanner.Buffer, '{');
--  --                 loop
--  --                    if Scanner.Token_Last > Scanner.Last then
--  --                       null;  --  Read new line
--  --                    end if;
--  --                    Cur := Scanner.Item (Scanner.Token_Last + 1);
--  --                    exit when Level = 0 and Cur := '}';
--  --                    if Cur = '{' then
--  --                       Level := Level + 1;
--  --                    elsif Cur = '}' then
--  --                       Level := Level - 1;
--  --                    elsif Cur = '/' and Scanner.Item (Scanner.Token_Last + 2) = '*' then
--  --                       --  Skip comments
--  --                 end loop;
--              --              declare
--  --                 Level : Natural := 1;
--  --              begin
--  --                 Cp := CP + 1;
--  --                 C  := CP.all;
--  --                 loop
--  --                    exit when C = ASCII.NUL;
--  --                    exit when Level = 0 and C = '}';

--  --                    if C = ASCII.LF then
--  --                       Lineno := Lineno + 1;
--  --                    elsif C = '{' then
--  --                       Level := Level + 1;
--  --                    elsif C = '}' then
--  --                       Level := Level - 1;
--  --                    elsif C = '/' and Cp (1) = '*' then  --  Skip comments
--  --                       declare
--  --                          Prevc : Integer := 0;
--  --                       begin
--  --                          cp := Cp + 2;
--  --                          C  := CP.all;
--  --                          loop
--  --                             exit when C = ASCII.NUL;
--  --                             exit when C = '/' and Prevc = '*';
--  --                             if C = ASCII.LF then
--  --                                Lineno := Lineno + 1;
--  --                             end if;
--  --                             prevc := C;
--  --                             Cp    := Cp + 1;
--  --                          end loop;
--  --                       end;
--  --                    elsif C = '/' and Cp (1)  = '/' then  --  Skip C++ style comments too
--  --                       cp := CP + 2;
--  --                       loop
--  --                          C := Cp.all;
--  --                          exit when C = ASCII.NUL or C = ASCII.LF;
--  --                          Cp := Cp + 1;
--  --                       end loop;
--  --                       if c  then
--  --                          Lineno := Lineno + 1;
--  --                       end if;
--  --                    elsif C = ''' or C = '"' then    --  String a character literals
--  --                       declare
--  --                          startchar : Integer := c;
--  --                          prevc     : Integer := 0;
--  --                       begin
--  --                          Cp := Cp + 1;
--  --                          loop
--  --                             C := Cp.all;
--  --                             exit when C = ASCII.NUL;
--  --                             exit when (C = startchar and Prevc /= '\');
--  --                             if C = ASCII.LF then
--  --                                Lineno := Lineno + 1;
--  --                             end if;
--  --                             if Prevc = '\' then
--  --                                prevc := 0;
--  --                             else
--  --                                prevc := c;
--  --                             end if;
--  --                             Cp := Cp + 1;
--  --                          end loop;
--  --                       end;
--  --                    end if;
--  --                    Cp := Cp + 1;
--  --                 end loop;
--  --              end;
--  --              if C = 0 then
--  --                 Parser_Error (E102, Scanner.Token_Lineno);
--  --                 nextcp := cp;
--  --              else
--  --                 nextcp := Cp + 1;
--  --              end if;


--              when 'a' .. 'z' |          --  Identifiers
--                'A' .. 'Z' |
--                '0' .. '9' | '_' =>

--                 Debug (Advance_On,
--                        "Advance Identifier beginning with:"
--                          & Scanner.Item (Scanner.Token_Last + 1));
--                 declare
--                    Cur : Character;
--                 begin
--                    loop
--                       exit when Scanner.Token_Last + 1 > Scanner.Last;
--                       Cur := Scanner.Item (Scanner.Token_Last + 1);
--                       --  Cur := Current_Token_Char (Scanner);
--                       exit when not
--                         (Cur in 'a' .. 'z' or
--                            Cur in 'A' .. 'Z' or
--                            Cur in '0' .. '9' or
--                            Cur = '_');
--                       Advance (Scanner, By => 1);
--                    end loop;
--                 end;


--              when others =>       --  All other (one character) operators
--                 if
--                   Line'Length >= 3 and then
--                   Line (Line'First .. Line'First + 3 - 1) = "::="
--                 then
--                    Debug (Advance_On, "Advance 3");
--                    Advance (Scanner, By => 3);
--                 elsif
--                   Line'Length >= 2 and then
--                   ((Line (Line'First) = '/' or Line (Line'First) = '|') and
--                      (Line (Line'First + 1) in 'a' .. 'z' or
--                       Line (Line'First + 1) in 'A' .. 'Z'))
--                 then
--                    Debug (Advance_On, "Advance 2");
--                    Advance (Scanner, By => 2);
--                    declare
--                       Cur : Character;
--                    begin
--                       loop
--                          Cur := Current_Token_Char (Scanner);
--                          if
--                            Cur in 'a' .. 'z' or
--                            Cur in 'A' .. 'Z' or
--                            Cur in '1' .. '9' or
--                            Cur = '_'
--                          then
--                             Debug (Advance_On, "Advance Label");
--                             Advance (Scanner, By => 1);
--                          else
--                             exit;
--                          end if;
--                       end loop;
--                    end;
--                 else
--  --               Debug (Advance_On, "Advance Character:" & Current_Token_Char (Scanner));
--                    Debug (Advance_On, "Advance Character:" &
--                           Scanner.Item (Scanner.Token_Last + 1));
--                    Advance (Scanner, By => 1);
--                 end if;

--           end case;
--        end;

--        if Scanner.Mode = C_Code_Block then
--           return;
--        end if;

--        --  Debug
--        Debug (False, "Scanner");
--        Debug (False, "  First      :" & Scanner.Token_First'Img);
--        Debug (False, "  Last       :" & Scanner.Last'Img);
--        Debug (False, "  Item       :" & Scanner.Item (Scanner.Item'First .. 100));

--        --  Skip empty lines
--        if Scanner.Token_First > Scanner.Last then
--           return;
--        end if;

--        Ada.Text_IO.Put (Scanner.Item (Scanner.Token_First .. Scanner.Last));
--        Ada.Text_IO.New_Line;

--        --
--        --  Scanner.Item (Scanner.Token_First .. Scanner.Token_Last)
--        --  Should enclose token
--        --
--        Parse_One_Token (Lemon, Scanner,
--                         Token => Scanner.Item (Scanner.Token_First .. Scanner.Token_Last));

--        Scanner.Token_First := Scanner.Token_Last + 1;
--  --      Scanner.Token_Last  := Scanner.Token_First - 1;

--     end Parse_Current_Character;


--     procedure Parse_Current_Line (Lemon   : in out Lime.Lemon_Record;
--                                   Scanner : in out Scanner_Record)
--     is
--     begin
--        Debug (True, "Parse_On_Mode. Mode: " & Scanner.Mode'Img);

--        case Scanner.Mode is

--           when C_Comment_Block =>
--              declare
--                 use Ada.Strings.Fixed;
--                 Position_C_Comment_End : constant Natural :=
--                   Index (Scanner.Item (Scanner.Token_First .. Scanner.Last), Comment_C_End);
--              begin
--                 if Position_C_Comment_End /= 0 then
--                    Scanner.Mode  := Root;
--                    Scanner.Token_First := Position_C_Comment_End + Comment_C_End'Length;
--                 else
--                    Scanner.Last := Scanner.Token_First - 1;
--                    --  No end of comment found so Line is empty
--                 end if;
--              end;

--           when String_Literal =>
--              Ada.Text_IO.Put_Line ("##3-1");

--           when Identifier =>
--              Ada.Text_IO.Put_Line ("##3-2");

--           when C_Code_Block =>
--              Ada.Text_IO.Put_Line ("##3-3");

--           when Quoted_Identifier =>
--              Ada.Text_IO.Put_Line ("##3-4");
--              Parse_Quoted_Identifier (Scanner);

--           when Root =>
--              Detect_Start_Of_C_Comment_Block (Scanner);

--              --  Scanner.Done := False;
--              --  loop
--              if Scanner.Mode = Root then
--                 while Scanner.Token_Last < Scanner.Last loop
--                    if Scanner.Item (Scanner.Token_First) = ' ' then
--                       Scanner.Token_First := Scanner.Token_First + 1;
--                       Scanner.Token_Last  := Scanner.Token_Last  + 1;
--                    else
--                       Parse_Current_Character (Lemon, Scanner);
--                       if Scanner.Mode = C_Code_Block then
--                          exit;
--                       end if;
--                    end if;
--  --                  Debug (True, "Call Parse_Current_Character with : "
--  --                           & Current_Token_Char (Scanner));
--                    exit when Scanner.Token_Last < 1;  --  Empty line detected
--                 end loop;
--              end if;
--              --   exit when Scanner.Done;
--              --  end loop;

--        end case;

--     exception

--        when Constraint_Error =>
--           case Scanner.Mode is

--              when Quoted_Identifier =>
--                 Errors.Parser_Error (E102, Scanner.Token_Lineno);

--              when others =>
--                 raise;

--           end case;

--     end Parse_Current_Line;


--     procedure Parse_Quoted_Identifier (Scanner : in out Scanner_Record)
--     is
--        use Ada.Strings.Unbounded;
--        Current : Character renames Scanner.Item (Scanner.Current);
--     begin
--        if Current = '"' then
--           Scanner.Mode := Root;
--        else
--           Scanner.Buffer := Scanner.Buffer & Current;
--        end if;

--     exception

--        when Constraint_Error =>
--           Errors.Parser_Error (E102, Scanner.Token_Lineno);

--     end Parse_Quoted_Identifier;


--     procedure Detect_Start_Of_C_Comment_Block (Scanner : in out Scanner_Record)
--     is
--        use Ada.Strings.Fixed;

--        Comment_C_Start : constant Natural :=
--          Index (Scanner.Item (Scanner.Token_First .. Scanner.Last), Comment_C_Begin);
--     begin
--        if Comment_C_Start /= 0 then
--  --         Scanner.Token_Start  := Comment_C_Start; --  Mark the beginning of the token
--  --         Scanner.Token_Lineno := Text_IO.Line_Number;  --  Linenumber on which token begins
--  --         Scanner.First        := Comment_C_Start;
--           Scanner.Last         := Comment_C_Start - 1;
--           Scanner.Mode         := C_Comment_Block;
--        end if;
--     end Detect_Start_Of_C_Comment_Block;


   procedure Parse (Lemon : in out Lime.Lemon_Record)
   is
      use Ada.Text_IO;
      use Ada.Strings.Unbounded;
      use Ada.Characters;

      Success : Boolean;
      Scanner : Scanner_Record;
   begin
      Scanner.File_Name    := Lemon.File_Name;
      Scanner.Token_Lineno := 0;
      Scanner.Error_Count  := 0;

      --  Begin by opening the input file
      Parser_FSM.Initialize_FSM (Lemon, Scanner);
      Errors.Set_File_Name (Scanner.File_Name);

      declare
         Filebuf    : String := Read_File (To_String (Scanner.File_Name));
         Index      : Natural := Filebuf'First;
         Next_Index : Natural;
         C          : Character;
      begin

         --  Make an initial pass through the file to handle %ifdef and %ifndef.
         Macros.Preprocess (Filebuf, Success);

         Scanner.Line_Number := 1;
         loop
            C := Filebuf (Index);
            exit when C = Latin_1.NUL;

            --  Keep track of line number
            if C = Latin_1.LF then
               Scanner.Line_Number := Scanner.Line_Number + 1;
            end if;

            --  Skip all white space
            if C = ' ' or C = Latin_1.HT or C = Latin_1.LF then
               Index := Index + 1;
               goto Continue;
            end if;

            --  Skip C++ style comment
            if C = '/' and Filebuf (Index + 1) = '/' then
               Debug (On_True, "Skip C++ style commet");
               Index := Index + 2;
               loop
                  C := Filebuf (Index);
                  exit when C = Latin_1.NUL;
                  exit when C = Latin_1.LF;
                  Index := Index + 1;
               end loop;
               goto Continue;
            end if;

            --  Skip C style comment
            if C = '/' and Filebuf (Index + 1) = '*' then
               Index := Index + 2;
               loop
                  C := Filebuf (Index);
                  exit when C = Latin_1.NUL;
                  exit when C = '/' and Filebuf (Index - 1) = '*';
                  if C = Latin_1.LF then
                     Scanner.Line_Number := Scanner.Line_Number + 1;
                  end if;
                  Index := Index + 1;
               end loop;
               if C /= Latin_1.NUL then
                  Index := Index + 1;
               end if;
               goto Continue;
            end if;

            Scanner.Token_First  := Index;
            Scanner.Token_Lineno := Scanner.Line_Number;

            Debug (On_True, "Token_First: " & Scanner.Token_First'Img);

            case C is

               --  String literals
               when '"' =>
                  Index := Index + 1;
                  loop
                     C := Filebuf (Index);
                     exit when C = Latin_1.NUL;
                     exit when C = '"';
                     if C = Latin_1.LF then
                        Scanner.Line_Number := Scanner.Line_Number + 1;
                     end if;
                     Index := Index + 1;
                  end loop;
                  if C = Latin_1.NUL then
                     Parser_Error (E103, Scanner.Token_Lineno);
                     Next_Index := Index;
                  else
                     Next_Index := Index + 1;
                  end if;

                  --  A block of C code
               when '{' =>
                  declare
                     Level : Natural;
                  begin
                     Level := 1;
                     Index := Index + 1;
                     loop
                        C := Filebuf (Index);
                        Debug (On_True, "C code:" & C);
                        exit when C = Latin_1.NUL;
                        exit when Level = 1 and C = '}';

                        if C = Latin_1.LF then
                           Scanner.Line_Number := Scanner.Line_Number + 1;

                        elsif C = '{' then
                           Level := Level + 1;
                        elsif C = '}' then
                           Level := Level - 1;

                           --  Skip C comments
                        elsif C = '/' and Filebuf (Index + 1) = '*' then
                           declare
                              Prev_C : Character;
                           begin
                              Index  := Index + 2;
                              Prev_C := Latin_1.NUL;
                              loop
                                 C := Filebuf (Index);
                                 exit when C = Latin_1.NUL;
                                 exit when C = '/' and Prev_C = '*';
                                 if C = Latin_1.LF then
                                    Scanner.Line_Number := Scanner.Line_Number + 1;
                                 end if;
                                 Prev_C := C;
                                 Index  := Index + 1;
                              end loop;
                           end;

                           --  Skip C++ style comments too
                        elsif C = '/' and Filebuf (Index + 1) = '/' then
                           Index := Index + 2;
                           loop
                              C := Filebuf (Index);
                              exit when C = Latin_1.NUL;
                              exit when C = Latin_1.LF;
                              Index := Index + 1;
                           end loop;
                           if C /= Latin_1.NUL then
                              Scanner.Line_Number := Scanner.Line_Number + 1;
                           end if;

                           --  String is a character literals
                        elsif C = ''' or C = '"' then
                           declare
                              Start_Char : constant Character := C;
                              Prev_C     : Character := Latin_1.NUL;
                           begin
                              Index := Index + 1;
                              loop
                                 C := Filebuf (Index);
                                 Debug (On_True, "C code literal:" & C);
                                 exit when C = Latin_1.NUL;
                                 exit when (C = Start_Char and Prev_C /= '\');
                                 if C = Latin_1.LF then
                                    Scanner.Line_Number := Scanner.Line_Number + 1;
                                 end if;
                                 if Prev_C = '\' then
                                    Prev_C := Latin_1.NUL;
                                 else
                                    Prev_C := C;
                                 end if;
                                 Index := Index + 1;
                              end loop;
                           end;
                        end if;

                        Index := Index + 1;
                     end loop;
                  end;
                  if C = Latin_1.NUL then
                     Parser_Error (E102, Scanner.Token_Lineno);
                     Next_Index := Index;
                  else
                     Next_Index := Index + 1;
                  end if;

                  --  Identifiers
               when
                 'a' .. 'z' |
                 'A' .. 'Z' |
                 '0' .. '9' | '_' =>
                  Debug (On_True, "Identifier");
                  loop
                     C := Filebuf (Index);
                     exit when C = Latin_1.NUL;
                     exit when not
                       (C in 'a' .. 'z' or
                          C in 'A' .. 'Z' or
                          C in '0' .. '9' or
                          C = '_');
                     Debug (On_True, "Skipping:" & C);
                     Index := Index + 1;
                  end loop;
                  Next_Index := Index;

                  --  All other (one character) operators
               when others =>
                  if
                    Filebuf (Index .. Index + 2) = "::="
                  then
                     Index := Index + 3;
                     Next_Index := Index;
                  elsif
                    (C = '/' or C = '|') and
                    (Filebuf (Index + 1) in 'a' .. 'z' or
                       Filebuf (Index + 1) in 'A' .. 'Z')
                  then
                     Index := Index + 2;
                     loop
                        C := Filebuf (Index);
                        exit when C = Latin_1.NUL;
                        if
                          C in 'a' .. 'z' or
                          C in 'A' .. 'Z' or
                          C in '0' .. '9' or
                          C = '_'
                        then
                           Index := Index + 1;
                        else
                           exit;
                        end if;
                     end loop;
                     Next_Index := Index;

                     --  All other one character operators
                  else
                     Debug (On_True, "One character operator");
                     Index      := Index + 1;
                     Next_Index := Index;
                  end if;

            end case;

            C := Filebuf (Index);

            Parse_One_Token (Lemon, Scanner,
                             Token => Filebuf (Scanner.Token_First .. Index - 1));

            Index := Next_Index;

            <<Continue>>
            null;
         end loop;
      end;

      Lemon.Rule      := Rules.Rule_Access (Scanner.First_Rule);
      Lemon.Error_Cnt := Scanner.Error_Count;
   end Parse;


   procedure Parse_One_Token (Lemon   : in out Lime.Lemon_Record;
                              Scanner : in out Scanner_Record;
                              Token   : in     String)
   is
      use Parser_FSM;
   begin

      --  JQ
      --  For input "%token_prefix TK_" :
      --  Token=[%] state=1
      --  Token=[token_prefix] state=2
      --  Token=[TK_] state=3

      if True then
         declare
            use Ada.Text_IO;
            use DK8543.Errors;
            use Ada.Strings.Unbounded;
--            Token_Img : constant String :=
--              Scanner.Item (Scanner.Token_First .. Scanner.Token_Last);
            State_Num : constant String := State_Scanner'Pos (Scanner.State)'Img;
         begin
            Error (Standard_Output,
                   To_String (Lemon.File_Name),
                   Scanner.Token_Lineno,
                   "Token=[" & Token & --  Token_Img &
                     "] "
                     & "state=" & State_Num); --- Scanner.State'Img);
         end;
      end if;

--      Scanner.Done := False;
--      loop
      Do_State (Lemon, Scanner, Token);
--         exit when Scanner.Done;
--      end loop;
   end Parse_One_Token;


   procedure Debug (On    : in Boolean;
                    Image : in String)
   is
      use Ada.Text_IO;
   begin
      if On then
         Put_Line (Image);
      end if;
   end Debug;

end Parsers;
