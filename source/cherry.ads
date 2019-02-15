--
--  The author disclaims copyright to this source code.  In place of
--  a legal notice, here is a blessing:
--
--    May you do good and not evil.
--    May you find forgiveness for yourself and forgive others.
--    May you share freely, not taking more than you give.
--

with Lime;
with Symbols;

package Cherry is

   procedure Dummy is null;

   procedure Find_States (Lemp : in out Lime.Lemon_Record);
   --  Compute all LR(0) states for the grammar.  Links are added to
   --  between some states so that the LR(1) follow sets can be
   --  computed later.

   procedure Add_The_Accepting_Token
     (Lemp : in out Lime.Lemon_Record;
      SP   : in out Symbols.Symbol_Access);
   --  Add the accepting Token.

private

   pragma Export (C, Find_States, "cherry_find_states");
   pragma Export (C, Add_The_Accepting_Token,
                  "cherry_add_the_accepting_token");

end Cherry;
