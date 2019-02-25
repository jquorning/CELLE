--
--  The author disclaims copyright to this source code.  In place of
--  a legal notice, here is a blessing:
--
--    May you do good and not evil.
--    May you find forgiveness for yourself and forgive others.
--    May you share freely, not taking more than you give.
--

with Ada.Strings.Unbounded;

with Interfaces.C;

with System;
limited with Symbols;

package Rules is

   use Interfaces.C;

--   type Symbol_Proxy_Record is private;
--   type Symbol_Proxy_Access is private;

   type Rule_Record;
   type Rule_1_Access is access all Rule_Record;

   --  Left-hand side of the rule
   use Ada.Strings.Unbounded;

   type RHS_Array_Access is access all Symbols.Symbol_Access_Array;

   --  A configuration is a production rule of the grammar together with
   --  a mark (dot) showing how much of that rule has been processed so far.
   --  Configurations also contain a follow-set which is a list of terminal
   --  symbols which are allowed to immediately follow the end of the rule.
   --  Every configuration is recorded as an instance of the following:
   type Rule_Record is
      record
         LHS          : access Symbols.Symbol_Record;  -- lemon.h:97
         LHS_Alias    : Unbounded_String;    -- Alias for the LHS (NULL if none)
         LHS_Start    : Boolean;             -- True if left-hand side is the start symbol
         Rule_Line    : Integer;             -- Line number for the rule
--       N_RHS        : Integer;    -- Number of RHS symbols
--       RHS          : System.Address;
         RHS          : RHS_Array_Access;    -- The RHS symbols
         RHS_Alias    : System.Address;      -- An alias for each RHS symbol (NULL if none)
         Line         : Integer;             -- Line number at which code begins
         Code         : Unbounded_String;    -- The code executed when this rule is reduced
         Code_Prefix  : Unbounded_String;    -- The code executed when this rule is reduced
         Code_Suffix  : Unbounded_String;    -- Setup code before code[] above
         No_Code      : Boolean;             -- True if this rule has no associated C code
         Code_Emitted : Boolean;             -- True if the code has been emitted already
         Prec_Sym     : access Symbols.Symbol_Record;  -- Precedence symbol for this rule
         Index        : Integer;             -- An index number for this rule
         Rule         : Integer;             -- Rule number as used in the generated tables
         Can_Reduce   : Boolean;             -- True if this rule is ever reduced
         Does_Reduce  : Boolean;             -- Reduce actions occur after optimization
         Next_LHS     : access Rule_Record;  -- Next rule with the same LHS
         Next         : access Rule_Record;  -- Next rule in the global list
      end record;

   type Rule_Access is access all Rule_Record;

   --  Breakdown code after code[] above

   --  function Rule_Sort (Rule : in Rule_Access) return Rule_Access;
   --  pragma Import (C, Rule_Sort, "lime_rule_sort");

   procedure Assing_Sequential_Rule_Numbers
     (Lemon_Rule : in     Rule_Access;
      Start_Rule :    out Rule_Access);

end Rules;

