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

with Scanner_Data;
with Scanner_Errors;
with Errors;
with Rules;
with Symbols;

package body Scanners is

   use Scanner_Data;

   --  Run the preprocessor over the input file text.  The global
   --  variables azDefine[0] through azDefine[nDefine-1] contains the
   --  names of all defined macros.  This routine looks for "%ifdef"
   --  and "%ifndef" and "%endif" and comments them out.  Text in
   --  between is also commented out as appropriate.

   procedure Parse_One_Token (Lemon   : in out Lime.Lemon_Record;
                              Scanner : in out Scanner_Record;
                              Line    : in     Line_Record);
   --  Parse a single Token.

   procedure Parse_On_Mode (Lemon   : in out Lime.Lemon_Record;
                            Scanner : in out Scanner_Record;
                            Line    : in out Line_Record;
                            Break   :    out Boolean);

   procedure Get_Line_Without_EOL_Comment (File : in     Ada.Text_IO.File_Type;
                                           Line :    out Line_Record);

   procedure Parse_Current_Character (Lemon   : in out Lime.Lemon_Record;
                                      Scanner : in out Scanner_Record;
                                      Line    : in out Line_Record);

   procedure Parse_Quoted_Identifier (Line : in out Line_Record);

   --
   --
   --

   procedure Parse_One_Token (Lemon   : in out Lime.Lemon_Record;
                              Scanner : in out Scanner_Record;
                              Line    : in     Line_Record)
   is separate;


   Comment_CPP     : constant String := "//";
   Comment_C_Begin : constant String := "/*";
   Comment_C_End   : constant String := "*/";

   Preproc_Ifdef   : constant String := "%ifdef";
   Preproc_Ifndef  : constant String := "%ifndef";
   Preproc_Endif   : constant String := "%endif";

   use Scanner_Errors;
   use Errors;
   use Ada.Text_IO;

   Line  : Line_Record;

   procedure Get_Line_Without_EOL_Comment (File : in     Ada.Text_IO.File_Type;
                                           Line :    out Line_Record)
   is
      use DK8543.Text;
   begin
      Line.First := 1;
      Ada.Text_IO.Get_Line (File, Line.Item, Last => Line.Last);
      Utility.Strip_End_Of_Line (From  => Line.Item,
                                 Strip => Comment_CPP,
                                 Last  => Line.Last);
   end Get_Line_Without_EOL_Comment;


   procedure Parse_Current_Character (Lemon   : in out Lime.Lemon_Record;
                                      Scanner : in out Scanner_Record;
                                      Line    : in out Line_Record)
   is
      use Ada.Strings.Unbounded;
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
      Parse_One_Token (Lemon, Scanner, Line); --  Parse the token
--    *cp = (char)c;                  /* Restore the buffer */
--    cp = nextcp;
   end Parse_Current_Character;


   procedure Parse_Quoted_Identifier (Line : in out Line_Record)
   is
      use Ada.Strings.Unbounded;
      Current : Character renames Line.Item (Line.Current);
   begin
      if Current = '"' then
         Line.Mode := Root;
      else
         Line.Buffer := Line.Buffer & Current;
      end if;
   exception
      when Constraint_Error =>
         Error (E101);
   end Parse_Quoted_Identifier;


   procedure Parse_On_Mode (Lemon   : in out Lime.Lemon_Record;
                            Scanner : in out Scanner_Record;
                            Line    : in out Line_Record;
                            Break   :    out Boolean)
   is
      Current : Character renames Line.Item (Line.Current);
   begin
      case Line.Mode is

         when C_Comment_Block =>
            declare
               use Ada.Strings.Fixed;
               Position_C_Comment_End : constant Natural :=
                 Index (Line.Item (Line.First .. Line.Last), Comment_C_End);
            begin
               if Position_C_Comment_End /= 0 then
                  Line.Mode := Root;
                  Break     := True;
--               else
--                  Line.Last := 0;
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
            Parse_Quoted_Identifier (Line);

         when Root =>
            --  Ada.Text_IO.Put_Line ("##3-5");
            Parse_Current_Character (Lemon, Scanner, Line);

      end case;

   exception

      when Constraint_Error =>
         case Line.Mode is

            when Quoted_Identifier =>
               Error (E102);

            when others =>
               raise;

         end case;

   end Parse_On_Mode;


   procedure Detect_Start_Of_C_Comment_Block (Line    : in out Line_Record;
                                              Scanner : in out Scanner_Record);

   procedure Detect_Start_Of_C_Comment_Block (Line    : in out Line_Record;
                                              Scanner : in out Scanner_Record)
   is
      use Ada.Strings.Fixed;
      use DK8543.Text;

      Comment_C_Start : constant Natural :=
        Index (Line.Item (Line.First .. Line.Last), Comment_C_Begin);
   begin
      if Comment_C_Start /= 0 then
         Scanner.Token_Start  := Comment_C_Start; --  Mark the beginning of the token
         Scanner.Token_Lineno := IO.Line_Number;  --  Linenumber on which token begins
         Line.First := Comment_C_Start;
         Line.Mode  := C_Comment_Block;
      end if;
   end Detect_Start_Of_C_Comment_Block;


   procedure Parse (Lemon : in out Lime.Lemon_Record)
   is
      use Ada.Strings.Unbounded;
      use Ada.Strings.Fixed;
      use DK8543.Text;

      Input_File : File_Type;
      Scanner    : Scanner_Record;
      Break_Out  : Boolean;
   begin
      Scanner.File_Name   := Lemon.File_Name;
      Scanner.Error_Count := 0;
      Scanner.Scan_State  := INITIALIZE;

      --  Begin by opening the input file
      Open (Input_File, In_File, To_String (Scanner.File_Name));

      --  Make an initial pass through the file to handle %ifdef and %ifndef.
      --  Preprocess_Input (filebuf);

      --  Now scan the text of the input file.
      loop
         Get_Line_Without_EOL_Comment (Input_File, Line);

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
         Detect_Start_Of_C_Comment_Block (Line, Scanner);

         --  Trim leading spaces
         DK8543.Text.Trim (Line.Item, Line.First, Line.Last,
                           Side => Ada.Strings.Left);

         --  Debug
         Ada.Text_IO.Put (Line.Item (Line.First .. Line.Last));
         Ada.Text_IO.New_Line;

         Parse_On_Mode (Lemon, Scanner, Line, Break_Out);

         Scanner.Token_Start  := Line.First;       --  Mark the beginning of the token
         Scanner.Token_Lineno := IO.Line_Number;   --  Linenumber on which token begins

      end loop;

   exception

      when End_Error =>
         Close (Input_File);
         Lemon.Rule      := Rules.Rule_Access (Scanner.First_Rule);
         Lemon.Error_Cnt := Scanner.Error_Count;

      when others =>
         Error (E103);
         raise;

   end Parse;


end Scanners;
