--
--  The author disclaims copyright to this source code.  In place of
--  a legal notice, here is a blessing:
--
--    May you do good and not evil.
--    May you find forgiveness for yourself and forgive others.
--    May you share freely, not taking more than you give.
--

with Interfaces.C.Strings;

with Lime;
with Rules;
with Configs;

package Lemon_Bind is

   procedure Configlist_Init;

   function Configlist_Add_Basis
     (RP : in Rules.Rule_Access;
      I  : in Integer) return Configs.Config_Access;

   procedure Set_Add (Config : in Interfaces.C.Strings.chars_ptr;
                      I      : in Integer);

   procedure Get_State (Lem : in out Lime.Lemon_Record);

private

   pragma Import (C, Configlist_Init,      "lemon_configlist_init");
   pragma Import (C, Configlist_Add_Basis, "lemon_configlist_add_basis");
   pragma Import (C, Set_Add,              "lemon_set_add");
   pragma Import (C, Get_State,            "lemon_get_state");

end Lemon_Bind;
