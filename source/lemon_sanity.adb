--
--  The author disclaims copyright to this source code.  In place of
--  a legal notice, here is a blessing:
--
--    May you do good and not evil.
--    May you find forgiveness for yourself and forgive others.
--    May you share freely, not taking more than you give.
--

with Ada.Text_IO;

with Interfaces.C;
with Interfaces.C.Strings;

with lemon_h;

package body Lemon_Sanity is

   procedure Dump_Lemon_Record is
      use Ada.Text_IO;
      use Interfaces.C;
      use Interfaces.C.Strings;
      use lemon_h;
   begin
      Put_Line ("LIME_POWER_ON_SELF_TEST");
      Put_Line ("Size    : " & Lemon_Type'Size'Img);
      Put_Line ("nstate  : " & int'Image (Lime_Lemp.nstate));
      Put_Line ("nxstate : " & int'Image (Lime_Lemp.nxstate));
      Put_Line ("nrule   : " & int'Image (Lime_Lemp.nrule));
      Put_Line ("nsymbol : " & int'Image (Lime_Lemp.nsymbol));
      Put_Line ("nterminal : " & int'Image (Lime_Lemp.nterminal));
      Put_Line ("minShiftReduce : " & int'Image (Lime_Lemp.minShiftReduce));
      Put_Line ("errAction : " & int'Image (Lime_Lemp.errAction));
      Put_Line ("accAction : " & int'Image (Lime_Lemp.accAction));
      Put_Line ("noAction  : " & int'Image (Lime_Lemp.noAction));
      Put_Line ("minReduce : " & int'Image (Lime_Lemp.minReduce));
      Put_Line ("maxAction : " & int'Image (Lime_Lemp.maxAction));
      New_Line;
      if Lime_Lemp.start = Null_Ptr then
         Put_Line ("start    : (null)");
      else
         Put_Line ("start    : " & Strings.Value (Lime_Lemp.start));
      end if;
      Put_Line ("filename : " & Strings.Value (Lime_Lemp.filename));
      New_Line;
   end Dump_Lemon_Record;

end Lemon_Sanity;
