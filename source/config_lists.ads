with Sessions;
with Configs;
with Rules;

package Config_Lists is

   procedure Init;
   function Add_Basis
     (Rule : in Rules.Rule_Access;
      I    : in Integer) return Configs.Config_Access;
   procedure Closure (Session : in Sessions.Session_Type);
   procedure Sort;
   procedure Sort_Basis;
   function Xreturn return Configs.Config_Access;
   function Basis return Configs.Config_Access;
   procedure Eat (Config : in Configs.Config_Access);
   procedure Reset;

private

   pragma Import (C, Init,       "lemon_configlist_init");
   pragma Import (C, Add_Basis,  "lemon_configlist_add_basis");
   pragma Import (C, Closure,    "Configlist_closure");
   pragma Import (C, Sort,       "Configlist_sort");
   pragma Import (C, Sort_Basis, "Configlist_sortbasis");
   pragma Import (C, Xreturn,    "Configlist_return");
   pragma Import (C, Basis,      "Configlist_basis");
   pragma Import (C, Eat,        "Configlist_eat");
   pragma Import (C, Reset,      "lemon_configlist_reset");

end Config_Lists;
