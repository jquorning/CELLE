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
      loop
         exit when Rule = null;
         Put ("RULE INDEX");
         Put (Rule.Index'Img);
         Put (" RULE");
         Put (Rule.Rule'Img);
         Put (" LINE");
         Put (Rule.Line'Img);
         Put (" RULELINE");
         Put (Rule.Rule_Line'Img);
         New_Line;
         Rule := Rule.Next;
      end loop;
   end JQ_Dump_Rules;

end Debugs;
