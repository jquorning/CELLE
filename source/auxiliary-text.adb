--
--  The author disclaims copyright to this source code.  In place of
--  a legal notice, here is a blessing:
--
--    May you do good and not evil.
--    May you find forgiveness for yourself and forgive others.
--    May you share freely, not taking more than you give.
--

package body Auxiliary.Text is


   procedure Trim
     (Item  : in     String;
      First : in out Natural;
      Last  : in out Natural;
      Side  : in     Ada.Strings.Trim_End)
   is
   begin
      for Index in First .. Last loop
         if Item (Index) /= Ada.Strings.Space then
            First := Index;
            exit;
         end if;
      end loop;
   end Trim;

end Auxiliary.Text;
