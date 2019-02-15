--
--  The author disclaims copyright to this source code.  In place of
--  a legal notice, here is a blessing:
--
--    May you do good and not evil.
--    May you find forgiveness for yourself and forgive others.
--    May you share freely, not taking more than you give.
--

with Lime;

package Cherry is

   procedure Dummy is null;

   procedure Find_States (Lemp : in out Lime.Lemon_Record);
   --  Compute all LR(0) states for the grammar.  Links are added to
   --  between some states so that the LR(1) follow sets can be
   --  computed later.

private

   pragma Export (C, Find_States, "lime_find_states");

end Cherry;
