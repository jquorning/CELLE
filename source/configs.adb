--
--  The author disclaims copyright to this source code.  In place of
--  a legal notice, here is a blessing:
--
--    May you do good and not evil.
--    May you find forgiveness for yourself and forgive others.
--    May you share freely, not taking more than you give.
--

package body Configs is


   function "<" (Left, Right : in Config_Record) return Boolean
   is
      use type Rules.Dot_Type;
   begin
      if Left.Rule.Index < Right.Rule.Index then
         return True;
      elsif Left.Rule.Index = Right.Rule.Index then
         return False;
      else
         return Left.Dot < Right.Dot;
      end if;
   end "<";


   function "<" (Left, Right : in Config_Access) return Boolean is
      (Left.all < Right.all);


end Configs;
