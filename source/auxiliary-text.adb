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

   function In_First_Part
     (From : in String;
      Item : in String) return Boolean
   is
   begin
      if
        From'Length >= Item'Length and then
        From (From'First .. From'First + Item'Length - 1) = Item
      then
         return True;
      else
         return False;
      end if;
   end In_First_Part;

end Auxiliary.Text;
