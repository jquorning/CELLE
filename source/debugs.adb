--
--
--

with Ada.Text_IO;

with Rules;
with Symbols;

package body Debugs is

   procedure JQ_Dump_Rules (Session : in Sessions.Session_Type;
                            Mode    : in Integer)
   is
      use Ada.Text_IO;
      use type Rules.Rule_Symbol_Access;
   begin
      for Rule of Session.Rule loop
         Put ("RULE INDEX");
         Put (Natural'Image (Rule.Index));
         Put (" RULE");
         Put (Natural'Image (Rule.Rule));
         Put (" LINE");
         Put (Natural'Image (Rule.Line));
         Put (" RULELINE");
         Put (Natural'Image (Rule.Rule_Line));

         if Mode = 1 then
            Put (" PREC");
            if Rule.Prec_Symbol = null then
               Put (" <null>");
            else
               Put (Symbols.Symbol_Index'Image (Rule.Prec_Symbol.Index));
            end if;
         end if;

         New_Line;
      end loop;
   end JQ_Dump_Rules;


   procedure Debug (On      : in Boolean;
                    Message : in String)
   is
   begin
      if On then
         Ada.Text_IO.Put_Line (Message);
      end if;
   end Debug;


end Debugs;
