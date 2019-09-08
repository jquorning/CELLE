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

limited with Symbols;

package Rules is

   type Rule_Record;
   type Rule_1_Access is access all Rule_Record;

   --  Left-hand side of the rule

   type RHS_Array_Access   is access all Symbols.Symbol_Access_Array;
   type Alias_Array_Access is access all Symbols.Symbol_Access_Array;
--   package Symbol_Vectors is
--      new Ada.Containers.Vectors
--     (Positive,
--      Symbols.Symbol_Access);

   use Ada.Strings.Unbounded;
   package Alias_Vectors is
      new Ada.Containers.Vectors
     (Positive,
      Unbounded_String);


   subtype T_Code is Unbounded_String;

   Null_Code : T_Code renames Null_Unbounded_String;

   function "=" (Left, Right : T_Code) return Boolean
     renames Ada.Strings.Unbounded."=";

   --  A configuration is a production rule of the grammar together with
   --  a mark (dot) showing how much of that rule has been processed so far.
   --  Configurations also contain a follow-set which is a list of terminal
   --  symbols which are allowed to immediately follow the end of the rule.
   --  Every configuration is recorded as an instance of the following:

   --  Each production rule in the grammar is stored in the following
   --  structure.
   type Rule_Record is
      record
         LHS          : access Symbols.Symbol_Record := null;
         LHS_Alias    : Unbounded_String := Null_Unbounded_String;
         --  Alias for the LHS (NULL if none)

         LHS_Start    : Boolean          := False; -- True if left-hand side is the start symbol
         Rule_Line    : Integer          := 0;     -- Line number for the rule
         RHS          : RHS_Array_Access := null;  -- The RHS symbols

         RHS_Alias    : Alias_Vectors.Vector := Alias_Vectors.Empty_Vector;
         --  An alias for each RHS symbol (NULL if none)

         Line         : Integer := 0;                      -- Line number at which code begins

         Code         : T_Code  := Null_Unbounded_String;
         --  The code executed when this rule is reduced

         Code_Prefix  : T_Code  := Null_Unbounded_String;
         --  Setup code before code[] above

         Code_Suffix  : T_Code  := Null_Unbounded_String;
         --  Breakdown code after code[] above

         No_Code      : Boolean := False;
         --  True if this rule has no associated C code

         Code_Emitted : Boolean := False;
         --  True if the code has been emitted already

         Prec_Sym     : access Symbols.Symbol_Record := null;
         --  Precedence symbol for this rule

         Index        : Integer := 0;
         --  An index number for this rule

         Rule         : Integer := 0;
         --  Rule number as used in the generated tables

         Can_Reduce   : Boolean := False;             -- True if this rule is ever reduced
         Does_Reduce  : Boolean := False;             -- Reduce actions occur after optimization
         Next_LHS     : access Rule_Record := null;   -- Next rule with the same LHS
         Next         : access Rule_Record := null;   -- Next rule in the global list
      end record;

   type Rule_Access is access all Rule_Record;


   function Merge (Pa : in Rule_Access;
                   Pb : in Rule_Access)
                  return Rule_Access;
   --  Merge together to lists of rules ordered by rule.iRule


   function Rule_Sort (Rule : in Rule_Access) return Rule_Access;
   --  Sort a list of rules in order of increasing iRule Value


   procedure Assing_Sequential_Rule_Numbers
     (Lemon_Rule : in     Rule_Access;
      Start_Rule :    out Rule_Access);

end Rules;

