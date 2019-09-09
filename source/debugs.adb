--
--
--

with Ada.Text_IO;

with Rules;

package body Debugs is

   procedure JQ_Dump_Rules (Lemon : in Lime.Lemon_Record)
   is
      use Ada.Text_IO;
      use Rules;

      Rule : Rule_Access := Lemon.Rule;
   begin
      while Rule /= null loop
         Put ("RULE INDEX");
         Put (Natural'Image (Rule.Index));
         Put (" RULE");
         Put (Natural'Image (Rule.Rule));
         Put (" LINE");
         Put (Natural'Image (Rule.Line));
         Put (" RULELINE");
         Put (Natural'Image (Rule.Rule_Line));
         New_Line;
         Rule := Rule.Next;
      end loop;
   end JQ_Dump_Rules;

end Debugs;
