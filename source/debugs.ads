--
--
--

with Sessions;

package Debugs is

   procedure JQ_Dump_Rules (Session : in Sessions.Session_Type;
                            Mode    : in Integer);

   procedure Debug (On      : in Boolean;
                    Message : in String);
   --
end Debugs;
