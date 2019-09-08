--
--
--

with Lime;

package Builds is


   procedure Find_Rule_Precedences (Lemon : in out Lime.Lemon_Record);
   --  Find a precedence symbol of every rule in the grammar.
   --
   --  Those rules which have a precedence symbol coded in the input
   --  grammar using the "[symbol]" construct will already have the
   --  rp->precsym field filled.  Other rules take as their precedence
   --  symbol the first RHS symbol with a defined precedence.  If there
   --  are not RHS symbols with a defined precedence, the precedence
   --  symbol field is left blank.


end Builds;
