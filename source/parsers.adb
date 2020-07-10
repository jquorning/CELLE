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
with Ada.Strings.Fixed;

with Parser_Data;
with Parser_FSM;

with Macros;
with Errors;

package body Parsers is

   On_True : constant Boolean := False;

   use Parser_Data;

   function Read_File (File_Name : in String) return String;
   --  Read file File_Name into a string with lines separated by Latin_1.LF

   procedure Parse_One_Token (Session : in out Sessions.Session_Type;
                              Scanner : in out Scanner_Record;
                              Token   : in     String);
   --  Parse a single Token.
   --  The token must be enclosed by Scanner.First and Scanner.Token

   procedure Debug (On    : in Boolean;
                    Image : in String);



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

      when Name_Error =>
         Errors.Parser_Error (Errors.E016, 0, File_Name);
         return "" & ASCII.NUL;

   end Read_File;


   use Errors;

   procedure Parse (Session : in out Sessions.Session_Type)
   is
      use Ada.Strings.Unbounded;
      use Ada.Characters;

      Success : Boolean;
      Scanner : Scanner_Record;
   begin
      Scanner.File_Name    := Session.File_Name;
      Scanner.Token_Lineno := 0;
      Scanner.Error_Count  := 0;

      --  Begin by opening the input file
      Parser_FSM.Initialize_FSM (Session, Scanner);
      Errors.Set_File_Name (Scanner.File_Name);

      declare
         Filebuf    : String := Read_File (To_String (Scanner.File_Name));
         Index      : Natural := Filebuf'First;
         Next_Index : Natural;
         C          : Character;
      begin

         --  Make an initial pass through the file to handle %ifdef and %ifndef.
         Macros.Preprocess (Filebuf, Success);

         Scanner.Line := 1;
         loop
            C := Filebuf (Index);
            exit when C = Latin_1.NUL;

            --  Keep track of line number
            if C = Latin_1.LF then
               Scanner.Line := Scanner.Line + 1;
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
                     Scanner.Line := Scanner.Line + 1;
                  end if;
                  Index := Index + 1;
               end loop;
               if C /= Latin_1.NUL then
                  Index := Index + 1;
               end if;
               goto Continue;
            end if;

            Scanner.Token_First  := Index;
            Scanner.Token_Lineno := Scanner.Line;

            Debug (On_True, "Token_First: " & Natural'Image (Scanner.Token_First));

            case C is

               --  String literals
               when '"' =>
                  Index := Index + 1;
                  loop
                     C := Filebuf (Index);
                     exit when C = Latin_1.NUL;
                     exit when C = '"';
                     if C = Latin_1.LF then
                        Scanner.Line := Scanner.Line + 1;
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
                           Scanner.Line := Scanner.Line + 1;

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
                                    Scanner.Line := Scanner.Line + 1;
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
                              Scanner.Line := Scanner.Line + 1;
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
                                 exit when C = Start_Char and Prev_C /= '\';
                                 if C = Latin_1.LF then
                                    Scanner.Line := Scanner.Line + 1;
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
                          not (C in 'a' .. 'z' or
                                 C in 'A' .. 'Z' or
                                 C in '0' .. '9' or
                                 C = '_')
                        then
                           exit;
                        end if;
                        Index := Index + 1;
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

            Parse_One_Token (Session, Scanner,
                             Token => Filebuf (Scanner.Token_First .. Index - 1));

            Index := Next_Index;

            <<Continue>>
            null;
         end loop;
      end;

      Session.Rule      := Scanner.Rule;
      Session.Error_Cnt := Scanner.Error_Count;
   end Parse;


   procedure Parse_One_Token (Session : in out Sessions.Session_Type;
                              Scanner : in out Scanner_Record;
                              Token   : in     String)
   is
      use Parser_FSM;
   begin

      --  For input "%token_prefix TK_" :
      --  Token=[%] state=1
      --  Token=[token_prefix] state=2
      --  Token=[TK_] state=3

      if False then
         declare
            use Ada.Text_IO;
            use Errors;
            use Ada.Strings.Unbounded;
            use Ada.Strings;

            State_Num : constant String
              := Fixed.Trim (State_Scanner'Pos (Scanner.State)'Img, Left);
         begin
            Emit_Error (Standard_Output,
                        To_String (Session.File_Name),
                        Scanner.Token_Lineno,
                        "Token=[" & Token &
                          "] "
                          & "state=" & State_Num);
         end;
      end if;

      Do_State (Session, Scanner, Token);
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
