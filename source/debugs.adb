--
--
--

with Ada.Text_IO;

with Rules;
with Types;
with Symbols.IO;

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
         Put (Natural'Image (Rule.Number));
         Put (" LINE");
         Put (Types.Line_Number'Image (Rule.Line));
         Put (" RULELINE");
         Put (Types.Line_Number'Image (Rule.Rule_Line));

         if Mode = 1 then
            Put (" PREC");
            if Rule.Prec_Symbol = null then
               Put (" <null>");
            else
               Put (Types.Symbol_Index'Image (Rule.Prec_Symbol.Index));
            end if;
         end if;

         New_Line;
      end loop;
   end JQ_Dump_Rules;


   procedure Put_States (Session : in Sessions.Session_Type;
                         Mode    : in Integer)
   is
   begin
      for State of Session.Sorted loop
         Put_State (State);
         Ada.Text_IO.New_Line;
         if Mode = 1 then
            Put_Configs (Session, Config_List => State.Config);
         end if;
      end loop;
   end Put_States;


   procedure Put_State (State : in States.State_Access)
   is
      use Ada.Text_IO;
      use States;
   begin
      Put ("NUM");
      Put (State.State_Num'Image);
      Put (" AC.LEN");
      Put (" 999");
--      Put (State.Action.Length'Image);
   end Put_State;



   procedure Put_Configs (Session     : in Sessions.Session_Type;
                          Config_List : in Configs.Config_Access)
   is
      use Configs;

      Config : Config_Access := Config_List;
   begin
      while Config /= null loop
         Put_Config (Session, Config);
         Ada.Text_IO.New_Line;
         Config := Config.Next;
      end loop;
   end Put_Configs;


   procedure Put_Config (Session : in Sessions.Session_Type;
                         Config  : in Configs.Config_Access)
   is
      use Ada.Text_IO;
      use Configs;
   begin
      Put ("  ");
      Put ("RULE");
      Put (Config.Rule.Index'Image);
      Put (" DOT");
      Put (Config.Dot'Image);
      --  Put ("STATE");
      Put (" STATUS ");
      Put (Config.Status'Image);
      Put (" FS ");
      Symbols.IO.Put_Named (Session, Set => Config.Follow_Set);
   end Put_Config;


   procedure Debug (On      : in Boolean;
                    Message : in String)
   is
   begin
      if On then
         Ada.Text_IO.Put_Line (Message);
      end if;
   end Debug;


end Debugs;
