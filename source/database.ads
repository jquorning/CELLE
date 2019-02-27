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

   use Lime;

   Lemon     : aliased Lemon_Record;
   pragma Import (C, Lemon, "lem");

   Lime_Lemp : aliased Lemon_Record;
   pragma Import (C, Lime_Lemp, "lem");
   --  Ada mirror of C lem (struct lemon) data structure.

   procedure Dump (Lemon : in Lemon_Record);
   --  pragma Export (C, Dump, "lime_power_on_self_test");
   --  Print out parts of Lemon to standard output.

end Database;
