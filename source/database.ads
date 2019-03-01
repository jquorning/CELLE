--
--  The author disclaims copyright to this source code.  In place of
--  a legal notice, here is a blessing:
--
--    May you do good and not evil.
--    May you find forgiveness for yourself and forgive others.
--    May you share freely, not taking more than you give.
--

with Lime;

package Database is

   Lemon : aliased Lime.Lemon_Record;
   --  Global Lemon_Record instance

   pragma Import (C, Lemon, "lem");
   --  Ada mirror of C lem (struct lemon) data structure

   procedure Dump (Lemon : in Lime.Lemon_Record);
   --  pragma Export (C, Dump, "lime_power_on_self_test");
   --  Print out parts of Lemon to standard output.

end Database;
