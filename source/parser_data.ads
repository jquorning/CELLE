--
--  The author disclaims copyright to this source code.  In place of
--  a legal notice, here is a blessing:
--
--    May you do good and not evil.
--    May you find forgiveness for yourself and forgive others.
--    May you share freely, not taking more than you give.
--

with Ada.Strings.Unbounded;
with Ada.Containers.Vectors;

with Symbols;
with Rules;

package Parser_Data is

   Max_Line_Length : constant := 1000;   --  Should do for the most

--     type Mode_Identifier is
--       (Root,             --  On outer level
--        C_Comment_Block,
--        String_Literal,
--        C_Code_Block,
--        Identifier,       --  Identifier
--        Quoted_Identifier --  Identifier in quortes
--       );

--     type State_Preproc is
--       (Root,
--        Ifdef,
--        Ifndef);

   --
   --  The state of the parser
   --

   type State_Scanner is
     (DUMMY,                     --  Make state numbers match lemon
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


   MAX_RHS : constant := 1000;

   use Ada.Strings.Unbounded;
   subtype S_Alias is Unbounded_String;
   function To_Alias (Item : String) return S_Alias renames To_Unbounded_String;

   package Alias_Vectors is
      new Ada.Containers.Vectors
     (Positive,
      S_Alias);

   type A_Declaration is access all Unbounded_String;

   use Symbols;
   type Scanner_Record is
      record
--         Token_Start   : Natural;                      --  Text of current token
         Line_Number   : Natural;                      --  Upcounting line number
         Token_Lineno  : Natural;                      --  Linenumber at which current token starts
         Error_Count   : Natural;                      --  Number of errors so far
--         Preproc_State : State_Preproc;
         State         : State_Scanner;                --  The state of the parser
         Fallback      : access Symbols.Symbol_Record; --  The fallback token
         Token_Class   : access Symbols.Symbol_Record; --  Token class symbol
--         LHS       : Symbols.Symbol_Access;          --  Left-hand side of current rule
         LHS : Symbol_Vectors.Vector;
--         LHS_Alias : Interfaces.C.Strings.chars_ptr;  --  Alias for the LHS
         LHS_Alias : Alias_Vectors.Vector;
         --  Interfaces.C.Strings.chars_ptr;  --  Alias for the LHS
         --  N_RHS     : Natural;
         --  Number of right-hand side symbols seen

         --    struct symbol *rhs[MAXRHS];  --  RHS symbols
         --  RHS       : RHS_Array;
         RHS       : Symbol_Vectors.Vector;
         --  Alias     : Alias_Array;
         Alias     : Alias_Vectors.Vector;
         --    const char *alias[MAXRHS];
         --  Aliases for each RHS symbol (or NULL)
         Prev_Rule     : access Rules.Rule_Record;     --  Previous rule parsed
         Decl_Keyword  : Unbounded_String;   --  Keyword of a declaration

         Decl_Arg_Slot : A_Declaration;
         --  Decl_Arg_Slot : access Unbounded_String; -- Access Interfaces.C.Strings.chars_ptr;
         --  Decl_Arg_Slot : access Interfaces.C.Strings.chars_ptr;
         --    char **declargslot;                --  Where the declaration argument should be put
         Insert_Line_Macro : Boolean;             --  Add #line before declaration insert

         Decl_Lineno_Slot  : not null access Integer := new Integer'(0);
         --  Where to write declaration line number

         Decl_Assoc   : E_Assoc;                  --  Assign this association to decl arguments
         Prec_Counter : Integer;                  --  Assign this precedence to decl arguments
         First_Rule   : access Rules.Rule_Record; --  Pointer to first rule in the grammar
         Last_Rule    : access Rules.Rule_Record; --  Pointer to the most recently parsed rule
         File_Name    : Unbounded_String;         --  Name of the input file

         --  From Line_Record
--         Current : Positive;                      --  Position in Item of examined character
         Token_First : Positive := Positive'First;    --  First position in filebuf
--         Token_Last  : Natural  := Natural'First;
--         Last    : Natural  := Natural'First;     --  Last position in Item
--         Item    : String (1 .. Max_Line_Length); --  Full line read from input
--         Mode    : Mode_Identifier := Root;       --  Mode
--         Done    : Boolean         := False;
         Buffer  : Unbounded_String;              --  Holder for identifiers etc.
      end record;


end Parser_Data;
