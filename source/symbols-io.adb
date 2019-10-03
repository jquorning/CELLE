
with Ada.Text_IO;

with Sets.IO;

package body Symbols.IO is

   --  Debug
   procedure JQ_Dump_Symbols (Session : in Sessions.Session_Type;
                              Mode    : in Integer)
   is
      use Ada.Text_IO;
   begin
      --      for Symbol of Base loop
      for Index in 0 .. Last_Index loop
         declare
            Symbol : Symbol_Access renames Element_At (Index);
         begin
            Put ("SYM ");
            Put (To_String (Symbol.Name));
            Put (" INDEX");
            Put (Symbol_Index'Image (Symbol.Index));
            Put (" NSUB");
            Put (Symbol.Sub_Symbol.Length'Img);
            Put (" KIND");
            Put (Symbol_Kind'Pos (Symbol.Kind)'Img);

            if Mode = 1 then
               Put (" PREC");
               if Symbol.Kind = Multi_Terminal then
                  Put (" (");
                  for J in Symbol.Sub_Symbol.First_Index .. Symbol.Sub_Symbol.Last_Index loop
                     Put (Natural'Image (Symbol.Sub_Symbol.Element (J).Precedence));
                     Put (" ");
                  end loop;
                  Put (")");
               else
                  if Symbol.Precedence = -1 then
                     Put (" -1"); -- Hack
                  else
                     Put (Natural'Image (Symbol.Precedence));
                  end if;
               end if;

               Put (" LAMB ");
               Put (Boolean'Image (Symbol.Lambda));

               Put (" FS ");
               declare
                  use Sets;
               begin
                  if Symbol.First_Set = Null_Set then
                     Put ("<null>");
                  else
                     Sets.IO.Put_Named (Session, Symbol.First_Set);
                  end if;
               end;

            end if;

            New_Line;
         end;
      end loop;
   end JQ_Dump_Symbols;


end Symbols.IO;
