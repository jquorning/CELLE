--
--  The author disclaims copyright to this source code.  In place of
--  a legal notice, here is a blessing:
--
--    May you do good and not evil.
--    May you find forgiveness for yourself and forgive others.
--    May you share freely, not taking more than you give.
--

with Interfaces.C.Strings;

with System;

package Rules is

   use Interfaces.C;

--   type Symbol_Proxy_Record is private;
   type Symbol_Proxy_Access is private;

   type Rule_Record;
   type Rule_Access is access all Rule_Record;

   --  Left-hand side of the rule
   type Rule_Record is
      record
         lhs         : Symbol_Proxy_Access;  -- lemon.h:97
         lhsalias    : Interfaces.C.Strings.chars_ptr;  -- lemon.h:98
         lhsStart    : aliased int;  -- lemon.h:99
         ruleline    : aliased int;  -- lemon.h:100
         nrhs        : aliased int;  -- lemon.h:101
         rhs         : System.Address;  -- lemon.h:102
         rhsalias    : System.Address;  -- lemon.h:103
         line        : aliased int;  -- lemon.h:104
         code        : Interfaces.C.Strings.chars_ptr;  -- lemon.h:105
         codePrefix  : Interfaces.C.Strings.chars_ptr;  -- lemon.h:106
         codeSuffix  : Interfaces.C.Strings.chars_ptr;  -- lemon.h:107
         noCode      : aliased int;  -- lemon.h:108
         codeEmitted : aliased int;  -- lemon.h:109
         precsym     : Symbol_Proxy_Access;
         index       : aliased int;  -- lemon.h:111
         iRule       : aliased int;  -- lemon.h:112
         canReduce   : aliased Boolean;  -- lemon.h:113
         doesReduce  : aliased Boolean;  -- lemon.h:114
         nextlhs     : Rule_Access;  -- lemon.h:115
         next        : Rule_Access;  -- lemon.h:116
      end record;
   pragma Convention (C_Pass_By_Copy, Rule_Record);  -- lemon.h:96

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

   procedure Dummy;

private

   type Symbol_Proxy_Type;
   type Symbol_Proxy_Access is access all Symbol_Proxy_Type;

end Rules;

