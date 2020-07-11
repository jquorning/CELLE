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
with Ada.Containers.Doubly_Linked_Lists;

with Types;
limited with Symbols;

package Rules is

   type Dot_Type is new Natural;
   type Rule_Symbol_Access is access all Symbols.Symbol_Record;
   subtype Line_Number is Types.Line_Number;

   package RHS_Vectors is
      new Ada.Containers.Vectors (Index_Type   => Dot_Type,
                                  Element_Type => Rule_Symbol_Access);

   use Ada.Strings.Unbounded;
   package Alias_Vectors is
      new Ada.Containers.Vectors (Index_Type   => Dot_Type,
                                  Element_Type => Unbounded_String);

   subtype T_Code is Unbounded_String;

   Null_Code : T_Code renames Null_Unbounded_String;

   function "=" (Left, Right : in T_Code) return Boolean
     renames Ada.Strings.Unbounded."=";

   --  A configuration is a production rule of the grammar together with
   --  a mark (dot) showing how much of that rule has been processed so far.
   --  Configurations also contain a follow-set which is a list of terminal
   --  symbols which are allowed to immediately follow the end of the rule.
   --  Every configuration is recorded as an instance of the following:

   --  Each production rule in the grammar is stored in the following
   --  structure.
   type Rule_Record;
   type Rule_Access is access all Rule_Record;

   type Index_Number is new Integer;
   type Rule_Number  is new Integer;

   type Rule_Record is
      record
         LHS          : Rule_Symbol_Access := null;
         LHS_Alias    : Unbounded_String   := Null_Unbounded_String;
         --  Alias for the LHS (NULL if none)

         LHS_Start    : Boolean            := False; -- True if left-hand side is the start symbol
         Rule_Line    : Line_Number        := 0;     -- Line number for the rule
         RHS          : RHS_Vectors.Vector := RHS_Vectors.Empty_Vector;
         --  The RHS symbols

         RHS_Alias    : Alias_Vectors.Vector := Alias_Vectors.Empty_Vector;
         --  An alias for each RHS symbol (NULL if none)

         Line         : Line_Number := 0;
         --  Line number at which code begins

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

         Prec_Symbol  : Rule_Symbol_Access := null;
         --  Precedence symbol for this rule

         Index : Index_Number := 0;
         --  An index number for this rule

         Number : Rule_Number := 0;
         --  Rule number as used in the generated tables

         Can_Reduce   : Boolean := False;             -- True if this rule is ever reduced
         Does_Reduce  : Boolean := False;             -- Reduce actions occur after optimization
         Next_LHS     : Rule_Access := null;   -- Next rule with the same LHS
      end record;

   package Rule_Lists is
      new Ada.Containers.Doubly_Linked_Lists (Element_Type => Rule_Access);

   procedure Rule_Sort (Rule_List : in out Rule_Lists.List);
   --  Sort a list of rules in order of increasing iRule Value

   procedure Assing_Sequential_Rule_Numbers (Rule_List : in out Rule_Lists.List);

end Rules;
