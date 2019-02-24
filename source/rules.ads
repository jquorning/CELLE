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

--  with System;
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

   --  use Symbols;
   use Ada.Strings.Unbounded;
   type Rule_Record is
      record
         LHS          : access Symbols.Symbol_Record;  -- lemon.h:97
         LHS_Alias    : Unbounded_String;
--         LHS_Alias    : Symbols.S_Alias;
         LHS_Start    : Integer;  -- lemon.h:99
         Rule_Line    : Integer;  -- lemon.h:100
--         N_RHS        : Integer;  -- lemon.h:101
--         RHS          : System.Address;  -- lemon.h:102
         RHS          : RHS_Array_Access;
--         RHS          : Symbols.Symbol_Vectors.Vector;
         RHS_Alias    : Alias_Array_Access; --  System.Address;  -- lemon.h:103
--         RHS_Alias    : Symbols.Alias_Vectors.Vector;
         Line         : Natural;  -- lemon.h:104
         Code         : access Unbounded_String;
         Code_Prefix  : access Unbounded_String;
         Code_Suffix  : access Unbounded_String;
         No_Code      : Boolean;
         Code_Emitted : Integer;  -- lemon.h:109
         Prec_Sym     : access Symbols.Symbol_Record;
         Index        : Integer;
         Rule         : Integer;
         Can_Reduce   : Boolean;
         Does_Reduce  : Boolean;
         Next_LHS     : access Rule_Record;  -- lemon.h:115
         Next         : access Rule_Record;  -- lemon.h:116
      end record;
   --   pragma Convention (C_Pass_By_Copy, Rule_Record);  -- lemon.h:96

   type Rule_Access is access all Rule_Record;

  -- Alias for the LHS (NULL if none)
  -- True if left-hand side is the start symbol
  -- Line number for the rule
  -- Number of RHS symbols
  -- The RHS symbols
  -- An alias for each RHS symbol (NULL if none)
  -- Line number at which code begins
  -- The code executed when this rule is reduced
  -- Setup code before code[] above
  -- Breakdown code after code[] above
  -- True if this rule has no associated C code
  -- True if the code has been emitted already
  -- Precedence symbol for this rule
  -- An index number for this rule
  -- Rule number as used in the generated tables
  -- True if this rule is ever reduced
  -- Reduce actions occur after optimization
  -- Next rule with the same LHS
  -- Next rule in the global list
  -- A configuration is a production rule of the grammar together with
  --** a mark (dot) showing how much of that rule has been processed so far.
  --** Configurations also contain a follow-set which is a list of terminal
  --** symbols which are allowed to immediately follow the end of the rule.
  --** Every configuration is recorded as an instance of the following:

   --  function Rule_Sort (Rule : in Rule_Access) return Rule_Access;
   --  pragma Import (C, Rule_Sort, "lime_rule_sort");

   procedure Assing_Sequential_Rule_Numbers
     (Lemon_Rule : in     Rule_Access;
      Start_Rule :    out Rule_Access);

end Rules;

