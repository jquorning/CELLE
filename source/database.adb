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

package body Database is

   procedure Dump (Lemon : in Lemon_Record)
   is
      use Ada.Text_IO;
      use Interfaces.C;
      use Interfaces.C.Strings;
      use Database;
   begin
      Put_Line ("PARTIAL DUMP OF LEMON RECORD");
      Put_Line ("----------------------------");
      Put_Line ("Size    : " & Lemon_Record'Size'Img);
      Put_Line ("nstate  : " & int'Image (Lemon.N_State));
      Put_Line ("nxstate : " & int'Image (Lemon.Nx_State));
      Put_Line ("nrule   : " & int'Image (Lemon.N_Rule));
      Put_Line ("nsymbol : " & int'Image (int (Lemon.N_Symbol)));
      Put_Line ("nterminal : " & int'Image (int (Lemon.N_Terminal)));
      Put_Line ("minShiftReduce : " & int'Image (Lemon.Min_Shift_Reduce));
      Put_Line ("errAction : " & int'Image (Lemon.Err_Action));
      Put_Line ("accAction : " & int'Image (Lemon.Acc_Action));
      Put_Line ("noAction  : " & int'Image (Lemon.No_Action));
      Put_Line ("minReduce : " & int'Image (Lemon.Min_Reduce));
      Put_Line ("maxAction : " & int'Image (Lemon.Max_Action));
      New_Line;
      if Lemon.Start = Null_Ptr then
         Put_Line ("start    : (null)");
      else
         Put_Line ("start    : " & Strings.Value (Lemon.Start));
      end if;
      Put_Line ("filename : " & Strings.Value (Lemon.File_Name));
      New_Line;
   end Dump;

end Database;
