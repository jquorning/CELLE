--
--  The author disclaims copyright to this source code.  In place of
--  a legal notice, here is a blessing:
--
--    May you do good and not evil.
--    May you find forgiveness for yourself and forgive others.
--    May you share freely, not taking more than you give.
--

with Ada.Text_IO;

with Configs;

package body Symbols.IO is

   procedure Put_Named (Session : in Sessions.Session_Type;
                        Set     : in Symbol_Sets.Set_Type)
   is
      pragma Unreferenced (Session);
      use Ada.Text_IO;
      use Symbols;

      First : Boolean := True;
   begin
      Put ("[");
      for Index in Symbol_Sets.First_Index .. Symbol_Sets.Last_Index loop
         if Symbol_Sets.Set_Find (Set, Index) then
            if not First then
               Put (" ");
            end if;
            Put (Name_Of (Element_At (Index)));
            First := False;
         end if;
      end loop;
      Put ("]");
   end Put_Named;

   --
   --  Debug
   --

   procedure JQ_Dump_Symbols (Session : in Sessions.Session_Type;
                              Mode    : in Integer)
   is
      use Ada.Text_IO;
      use Types;
      use Symbol_Sets;
   begin
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
               if Symbol.First_Set = Null_Set then
                  Put ("<null>");
               else
                  Put_Named (Session, Symbol.First_Set);
               end if;

            end if;

            New_Line;
         end;
      end loop;
   end JQ_Dump_Symbols;


end Symbols.IO;
