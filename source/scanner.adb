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


package body Scanner is




   --  Run the preprocessor over the input file text.  The global
   --  variables azDefine[0] through azDefine[nDefine-1] contains the
   --  names of all defined macros.  This routine looks for "%ifdef"
   --  and "%ifndef" and "%endif" and comments them out.  Text in
   --  between is also commented out as appropriate.

   procedure Parse_One_Token (PSP  : in out Scanner_Data.Scanner_Record;
                              Line : in     Scanner_Data.Line_Record);

   procedure Parse_One_Token (PSP  : in out Scanner_Data.Scanner_Record;
                              Line : in     Scanner_Data.Line_Record)
   is separate;
   --  Parse a single Token.


   use Scanner_Data;
   procedure Get_Line_Without_EOL_Comment (Line : out Line_Record);


   Comment_CPP     : constant String := "//";
   Comment_C_Begin : constant String := "/*";
   Comment_C_End   : constant String := "*/";

   Preproc_Ifdef   : constant String := "%ifdef";
   Preproc_Ifndef  : constant String := "%ifndef";
   Preproc_Endif   : constant String := "%endif";

   use Scanner_Errors;
   use Errors;
   use Ada.Text_IO;

   File : File_Type;
   Line : Line_Record;
   PS   : Scanner_Record;


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
      Parse_One_Token (PS, Line);       --  Parse the token
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
         --  ("String starting on this line is not terminated before the end of the file.");
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
               Error (E102);

            when others =>
               raise;

         end case;

   end Parse_On_Mode;


   procedure Parse (GP : access Lime.Lemon_Record)
   is
      use Ada.Strings.Unbounded;
      use Ada.Strings.Fixed;
      use DK8543.Text;

      Break_Out : Boolean;
   begin
      PS.GP          := GP;
      PS.File_Name   := GP.File_Name;
--        To_Unbounded_String (Interfaces.C.Strings.Value (GP.File_Name));
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
         Error (E103);

   end Parse;


end Scanner;
