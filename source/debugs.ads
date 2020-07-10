--
--
--

with Sessions;
with States;
with Configs;

package Debugs is

   procedure JQ_Dump_Rules (Session : in Sessions.Session_Type;
                            Mode    : in Integer);

   procedure Put_States (Session : in Sessions.Session_Type;
                         Mode    : in Integer);
   procedure Put_State (State : in States.State_Access);
   procedure Put_Configs (Session : in Sessions.Session_Type;
                         Config_List : in Configs.Config_Access);
   procedure Put_Config (Session : in Sessions.Session_Type;
                         Config  : in Configs.Config_Access);

   procedure Debug (On      : in Boolean;
                    Message : in String);
   --
end Debugs;
