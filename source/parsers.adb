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

with DK8543.Text_IO;
with DK8543.Strings.Utility;

with Parser_Data;
with Parser_FSM;

with Errors;
with Rules;
with Symbols;

package body Parsers is

   use Parser_Data;

   --  Run the preprocessor over the input file text.  The global
   --  variables azDefine[0] through azDefine[nDefine-1] contains the
   --  names of all defined macros.  This routine looks for "%ifdef"
   --  and "%ifndef" and "%endif" and comments them out.  Text in
   --  between is also commented out as appropriate.


   procedure Parse_On_Mode (Lemon   : in out Lime.Lemon_Record;
                            Scanner : in out Scanner_Record);

   procedure Get_Line_Without_EOL_Comment (File    : in     Ada.Text_IO.File_Type;
                                           Scanner : in out Scanner_Record);

   procedure Parse_Current_Character (Lemon   : in out Lime.Lemon_Record;
                                      Scanner : in out Scanner_Record);

   procedure Parse_Quoted_Identifier (Scanner : in out Scanner_Record);

   procedure Parse_One_Token (Lemon   : in out Lime.Lemon_Record;
                              Scanner : in out Scanner_Record);
   --  Parse a single Token.

   procedure Detect_Start_Of_C_Comment_Block (Scanner : in out Scanner_Record);


   procedure Debug (On    : in Boolean;
                    Image : in String);

   --
   --
   --

   Comment_CPP     : constant String := "//";
   Comment_C_Begin : constant String := "/*";
   Comment_C_End   : constant String := "*/";

   Preproc_Ifdef   : constant String := "%ifdef";
   Preproc_Ifndef  : constant String := "%ifndef";
   Preproc_Endif   : constant String := "%endif";

   use Errors;

   procedure Get_Line_Without_EOL_Comment (File    : in     Ada.Text_IO.File_Type;
                                           Scanner : in out Scanner_Record)
   is
      use DK8543.Strings;
   begin
      Scanner.First := 1;
      Scanner.Token := Scanner.First;
      Ada.Text_IO.Get_Line (File, Scanner.Item, Last => Scanner.Last);
      Scanner.Token_Lineno := Scanner.Token_Lineno + 1;
      Utility.Strip_End_Of_Line (From  => Scanner.Item,
                                 Strip => Comment_CPP,
                                 Last  => Scanner.Last);
   end Get_Line_Without_EOL_Comment;


   procedure Parse_Current_Character (Lemon   : in out Lime.Lemon_Record;
                                      Scanner : in out Scanner_Record)
   is
      use Ada.Text_IO;
      use Ada.Strings.Unbounded;

      Current : constant Character := Current_Token_Char (Scanner);
   begin
      Debug (False, "Parse_Current_Character");
      Debug (False, "  Current: " & Current);

      case Current is

         when '"' =>                     --   String literals
            Scanner.Mode   := Quoted_Identifier;
            Scanner.Buffer := Null_Unbounded_String;

         when '{' =>              --  A block of C code
            Debug (True, "  A block of C code");
--              declare
--                 Level : Natural := 1;
--              begin
--                 Cp := CP + 1;
--                 C  := CP.all;
--                 loop
--                    exit when C = ASCII.NUL;
--                    exit when Level = 0 and C = '}';

--                    if C = ASCII.LF then
--                       Lineno := Lineno + 1;
--                    elsif C = '{' then
--                       Level := Level + 1;
--                    elsif C = '}' then
--                       Level := Level - 1;
--                    elsif C = '/' and Cp (1) = '*' then  --  Skip comments
--                       declare
--                          Prevc : Integer := 0;
--                       begin
--                          cp := Cp + 2;
--                          C  := CP.all;
--                          loop
--                             exit when C = ASCII.NUL;
--                             exit when C = '/' and Prevc = '*';
--                             if C = ASCII.LF then
--                                Lineno := Lineno + 1;
--                             end if;
--                             prevc := C;
--                             Cp    := Cp + 1;
--                          end loop;
--                       end;
--                    elsif C = '/' and Cp (1)  = '/' then  --  Skip C++ style comments too
--                       cp := CP + 2;
--                       loop
--                          C := Cp.all;
--                          exit when C = ASCII.NUL or C = ASCII.LF;
--                          Cp := Cp + 1;
--                       end loop;
--                       if c  then
--                          Lineno := Lineno + 1;
--                       end if;
--                    elsif C = ''' or C = '"' then    --  String a character literals
--                       declare
--                          startchar : Integer := c;
--                          prevc     : Integer := 0;
--                       begin
--                          Cp := Cp + 1;
--                          loop
--                             C := Cp.all;
--                             exit when C = ASCII.NUL;
--                             exit when (C = startchar and Prevc /= '\');
--                             if C = ASCII.LF then
--                                Lineno := Lineno + 1;
--                             end if;
--                             if Prevc = '\' then
--                                prevc := 0;
--                             else
--                                prevc := c;
--                             end if;
--                             Cp := Cp + 1;
--                          end loop;
--                       end;
--                    end if;
--                    Cp := Cp + 1;
--                 end loop;
--              end;
--              if C = 0 then
--                 Parser_Error (E102, Scanner.Token_Lineno);
--                 nextcp := cp;
--              else
--                 nextcp := Cp + 1;
--              end if;


         when 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' =>  --  Identifiers
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

      --  Debug
      Debug (False, "Scanner");
      Debug (False, "  First      :" & Scanner.First'Img);
      Debug (False, "  Last       :" & Scanner.Last'Img);
      Debug (False, "  Item       :" & Scanner.Item (Scanner.Item'First .. 100));

      --  Skip empty lines
      if Scanner.First > Scanner.Last then
         return;
      end if;

      Ada.Text_IO.Put (Scanner.Item (Scanner.First .. Scanner.Last));
      Ada.Text_IO.New_Line;

      Parse_One_Token (Lemon, Scanner);

   end Parse_Current_Character;


   procedure Parse_Quoted_Identifier (Scanner : in out Scanner_Record)
   is
      use Ada.Strings.Unbounded;
      Current : Character renames Scanner.Item (Scanner.Current);
   begin
      if Current = '"' then
         Scanner.Mode := Root;
      else
         Scanner.Buffer := Scanner.Buffer & Current;
      end if;

   exception

      when Constraint_Error =>  Errors.Parser_Error (E102, Scanner.Token_Lineno);

   end Parse_Quoted_Identifier;


   procedure Parse_On_Mode (Lemon   : in out Lime.Lemon_Record;
                            Scanner : in out Scanner_Record)
   is
      use Ada.Text_IO;
   begin
      Put_Line ("Parse_On_Mode. Mode: " & Scanner.Mode'Img);
      case Scanner.Mode is

         when C_Comment_Block =>
            declare
               use Ada.Strings.Fixed;
               Position_C_Comment_End : constant Natural :=
                 Index (Scanner.Item (Scanner.First .. Scanner.Last), Comment_C_End);
            begin
               if Position_C_Comment_End /= 0 then
                  Scanner.Mode  := Root;
                  Scanner.First := Position_C_Comment_End + Comment_C_End'Length;
                  Put_Line ("  End");
               else
                  Scanner.Last := Scanner.First - 1;
                  --  No end of comment found so Line is empty
               end if;
            end;

         when String_Literal =>
            Ada.Text_IO.Put_Line ("##3-1");

         when Identifier =>
            Ada.Text_IO.Put_Line ("##3-2");

         when C_Code_Block =>
            Ada.Text_IO.Put_Line ("##3-3");

         when Quoted_Identifier =>
            Ada.Text_IO.Put_Line ("##3-4");
            Parse_Quoted_Identifier (Scanner);

         when Root =>
            Detect_Start_Of_C_Comment_Block (Scanner);

            Parse_Current_Character (Lemon, Scanner);

      end case;

   exception

      when Constraint_Error =>
         case Scanner.Mode is

            when Quoted_Identifier =>
               Errors.Parser_Error (E102, Scanner.Token_Lineno);

            when others =>
               raise;

         end case;

   end Parse_On_Mode;


   procedure Detect_Start_Of_C_Comment_Block (Scanner : in out Scanner_Record)
   is
      use Ada.Strings.Fixed;
      use DK8543;

      Comment_C_Start : constant Natural :=
        Index (Scanner.Item (Scanner.First .. Scanner.Last), Comment_C_Begin);
   begin
      if Comment_C_Start /= 0 then
--         Scanner.Token_Start  := Comment_C_Start; --  Mark the beginning of the token
--         Scanner.Token_Lineno := Text_IO.Line_Number;  --  Linenumber on which token begins
--         Scanner.First        := Comment_C_Start;
         Scanner.Last         := Comment_C_Start - 1;
         Scanner.Mode         := C_Comment_Block;
      end if;
   end Detect_Start_Of_C_Comment_Block;


   procedure Parse (Lemon : in out Lime.Lemon_Record)
   is
      use Ada.Text_IO;
      use Ada.Strings.Unbounded;
      use DK8543;

      Input_File : File_Type;
      Scanner    : Scanner_Record;
   begin
      Scanner.File_Name    := Lemon.File_Name;
      Scanner.Token_Lineno := 0;
      Scanner.Error_Count  := 0;
      Scanner.State        := INITIALIZE;

      --  Begin by opening the input file
      Errors.Set_File_Name (Scanner.File_Name);
      Open (Input_File, In_File, To_String (Scanner.File_Name));

      --  Make an initial pass through the file to handle %ifdef and %ifndef.
      --  Preprocess_Input (filebuf);

      --  Now scan the text of the input file.
      loop
         Get_Line_Without_EOL_Comment (Input_File, Scanner);

         --  Preprocess
--           if Line.First = Line.Item'First then
--              if In_First_Part (Line.Item, Preproc_Ifdef) then
--                 null;
--              elsif In_First_Part (Line.Item, Preproc_Ifndef) then
--                 null;
--              elsif In_First_Part (Line.Item, Preproc_Endif) then
--                 null;
--              else
--                 null;
--              end if;
--           end if;
         --  Skip C comments
         --  Comment_C_Filter (Input_File, Line);
--         Parse_On_Mode (Lemon, Scanner, Line, Break_Out);

         --  Detect start of C comment block
--         Detect_Start_Of_C_Comment_Block (Scanner);

         --  Trim leading spaces
         DK8543.Strings.Trim (Scanner.Item, Scanner.First, Scanner.Last,
                              Side => Ada.Strings.Left);

         --  Scanner.Token_Lineno := Text_IO.Line_Number; --  Linenumber on which token begins

         --  Debug

         Parse_On_Mode (Lemon, Scanner);

      end loop;

   exception

      when End_Error =>
         Close (Input_File);
         Lemon.Rule      := Rules.Rule_Access (Scanner.First_Rule);
         Lemon.Error_Cnt := Scanner.Error_Count;

      when Constraint_Error =>
         raise;

      when others =>
         Errors.Parser_Error (E101, Scanner.Token_Lineno);
         raise;

   end Parse;


   procedure Parse_One_Token (Lemon   : in out Lime.Lemon_Record;
                              Scanner : in out Scanner_Record)
   is
      use Parser_FSM;
   begin
      Scanner.Done := False;
      loop
         Debug (False, "Do_State: STATE: " & Scanner.State'Img);

         Do_State
           (Lemon   => Lemon,
            Scanner => Scanner);
         exit when Scanner.Done;
      end loop;
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
