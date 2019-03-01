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

with Interfaces.C.Strings;

with Lime;
with Symbols;
with Rules;

package Scanner_Data is

   --
   --  Line_Record
   --

   Max_Line_Length : constant := 250;   --  Should do for the most

   subtype Extended_Pos is Natural      range 0 .. Max_Line_Length;
   subtype Line_Pos     is Extended_Pos range 1 .. Extended_Pos'Last;

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


   MAX_RHS : constant := 1000;

   use Ada.Strings.Unbounded;
   --  type RHS_Array   is array (0 .. MAX_RHS - 1) of Symbols.Symbol_Access;
   subtype S_Alias is Unbounded_String;
   function To_Alias (Item : String) return S_Alias renames To_Unbounded_String;

   package Alias_Vectors is
      new Ada.Containers.Vectors
     (Positive,
      S_Alias);
   --  type Alias_Array is array (0 .. MAX_RHS - 1) of Unbounded_String;

   use Symbols;
   type Scanner_Record is
      record
         Token_Start   : Line_Pos;                  --  Text of current token
         Token_Lineno  : Natural;                   --  Linenumber at which current token starts
         Error_Count   : Natural;                   --  Number of errors so far
--         GP            : access Lime.Lemon_Record;  --  Global state vector
         Preproc_State : State_Preproc;
         Scan_State    : State_Scanner;             --  The state of the parser
         Fallback : access Symbols.Symbol_Record;   --  The fallback token
--    struct symbol *tkclass;    --  Token class symbol
--         LHS       : Symbols.Symbol_Access;           --  Left-hand side of current rule
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

         --  Decl_Arg_Slot : access Unbounded_String;
         Decl_Arg_Slot : access Interfaces.C.Strings.chars_ptr;
         --    char **declargslot;        --  Where the declaration argument should be put
         Insert_Line_Macro : Boolean;       --  Add #line before declaration insert
         Decl_Lineno_Slot : access Integer;       --  Where to write declaration line number
         Decl_Assoc   : E_Assoc;      --  Assign this association to decl arguments
         Prec_Counter : Integer;                  --  Assign this precedence to decl arguments
         First_Rule   : access Rules.Rule_Record; --  Pointer to first rule in the grammar
         Last_Rule    : access Rules.Rule_Record; --  Pointer to the most recently parsed rule
         File_Name    : Unbounded_String;         --  Name of the input file
      end record;


end Scanner_Data;
