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

with Types;
with Symbols;
with Rule_Lists;

package Parser_Data is

   Max_Line_Length : constant := 1000;   --  Should do for the most

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
   function To_Alias (Item : in String) return S_Alias renames To_Unbounded_String;

   package Alias_Vectors is
      new Ada.Containers.Vectors (Index_Type   => Positive,
                                  Element_Type => S_Alias);

   type A_Declaration      is access all Unbounded_String;
   subtype Line_Number     is Types.Line_Number;
   type Line_Number_Access is not null access all Line_Number;

   use Symbols;
   type Scanner_Record is
      record
         Line          : Line_Number;                  --  Upcounting line number
         Token_Lineno  : Line_Number;                  --  Line at which current token starts
         Error_Count   : Natural;                      --  Number of errors so far
--         Preproc_State : State_Preproc;
         State         : State_Scanner;                --  The state of the parser
         Fallback      : access Symbols.Symbol_Record; --  The fallback token
         Token_Class   : access Symbols.Symbol_Record; --  Token class symbol
         LHS           : Symbol_Vectors.Vector;        --  Left-hand side of current rule
         LHS_Alias     : Alias_Vectors.Vector;         --  Alias for the LHS

         RHS           : Symbol_Vectors.Vector;    --  RHS symbols
                                                   --  Number of right-hand side symbols seen
         Alias         : Alias_Vectors.Vector;     --  Aliases for each RHS symbol (or NULL)
         Previous_Rule : Rule_Lists.Lists.Cursor;  --  Previous rule parsed
         Decl_Keyword  : Unbounded_String;         --  Keyword of a declaration
         Decl_Arg_Slot : A_Declaration;            --  Where the declaration argument should be put
         Insert_Line_Macro : Boolean;              --  Add #line before declaration insert

         Decl_Lineno_Slot : Line_Number_Access := new Line_Number'(0);
         --  Where to write declaration line number

         Decl_Association : Association_Type;     --  Assign this association to decl arguments
         Prec_Counter : Integer;                  --  Assign this precedence to decl arguments
         Rule         : Rule_Lists.Lists.List;
         File_Name    : Unbounded_String;         --  Name of the input file

         Token_First : Positive := Positive'First;  --  First position in filebuf
         Buffer      : Unbounded_String;            --  Holder for identifiers etc.
      end record;


end Parser_Data;
