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

with Lemon_H;
with Database;

package body Lemon_Sanity is

   procedure Dump_Lemon_Record is
      use Ada.Text_IO;
      use Interfaces.C;
      use Interfaces.C.Strings;
      use Lemon_H;
      use Database;
   begin
      Put_Line ("LIME_POWER_ON_SELF_TEST");
      Put_Line ("Size    : " & Lemon_Record'Size'Img);
      Put_Line ("nstate  : " & int'Image (Lime_Lemp.N_State));
      Put_Line ("nxstate : " & int'Image (Lime_Lemp.Nx_State));
      Put_Line ("nrule   : " & int'Image (Lime_Lemp.N_Rule));
      Put_Line ("nsymbol : " & int'Image (int (Lime_Lemp.N_Symbol)));
      Put_Line ("nterminal : " & int'Image (int (Lime_Lemp.N_Terminal)));
      Put_Line ("minShiftReduce : " & int'Image (Lime_Lemp.Min_Shift_Reduce));
      Put_Line ("errAction : " & int'Image (Lime_Lemp.Err_Action));
      Put_Line ("accAction : " & int'Image (Lime_Lemp.Acc_Action));
      Put_Line ("noAction  : " & int'Image (Lime_Lemp.No_Action));
      Put_Line ("minReduce : " & int'Image (Lime_Lemp.Min_Reduce));
      Put_Line ("maxAction : " & int'Image (Lime_Lemp.Max_Action));
      New_Line;
      if Lime_Lemp.Start = Null_Ptr then
         Put_Line ("start    : (null)");
      else
         Put_Line ("start    : " & Strings.Value (Lime_Lemp.Start));
      end if;
      Put_Line ("filename : " & Strings.Value (Lime_Lemp.File_Name));
      New_Line;
   end Dump_Lemon_Record;

end Lemon_Sanity;
