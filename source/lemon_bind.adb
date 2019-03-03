--
--  The author disclaims copyright to this source code.  In place of
--  a legal notice, here is a blessing:
--
--    May you do good and not evil.
--    May you find forgiveness for yourself and forgive others.
--    May you share freely, not taking more than you give.
--

package body Lemon_Bind is

   procedure Set_Add (Config : in String;
                      I      : in Integer)
   is
      use Interfaces.C.Strings;
   begin
      Set_Add_C (New_String (Config), I);
   end Set_Add;

end Lemon_Bind;
