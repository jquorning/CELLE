--
--  The author disclaims copyright to this source code.  In place of
--  a legal notice, here is a blessing:
--
--    May you do good and not evil.
--    May you find forgiveness for yourself and forgive others.
--    May you share freely, not taking more than you give.
--

with Ada.Characters.Handling;

package body Options is


   procedure Set_Language
   is
      use Ada.Characters.Handling;
      LANG : constant String := To_Upper (Options.Language_String.all);
   begin
      if LANG = "ADA" then
         Language := Language_Ada;
      elsif LANG = "C" then
         Language := Language_C;
      else
         raise Constraint_Error
           with "Error: Unknown language";
      end if;
   end Set_Language;


end Options;
