
with Sessions;
with Symbol_Sets;

package Symbols.IO is


   procedure Put_Named (Session : in Sessions.Session_Type;
                        Set     : in Symbol_Sets.Set_Type);

   --  Debug
   procedure JQ_Dump_Symbols (Session : in Sessions.Session_Type;
                              Mode    : in Integer);

end Symbols.IO;
