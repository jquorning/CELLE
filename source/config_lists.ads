--
--  The author disclaims copyright to this source code.  In place of
--  a legal notice, here is a blessing:
--
--    May you do good and not evil.
--    May you find forgiveness for yourself and forgive others.
--    May you share freely, not taking more than you give.
--

with Sessions;
with Configs;
with Rules;

package Config_Lists is

   subtype Dot_Type is Natural;

   procedure Init;
   --  Initialized the configuration list builder.

   function Add
     (Rule : in Rules.Rule_Access;
      Dot  : in Dot_Type) return Configs.Config_Access;
   --  Add another configuration to the configuration list.

   function Add_Basis
     (Rule : in Rules.Rule_Access;
      Dot  : in Dot_Type) return Configs.Config_Access;
   --  Add a basis configuration to the configuration List.

   procedure Closure (Session : in Sessions.Session_Type);
   --  Compute the closure of the configuration List.

   procedure Sort;
   --  Sort the configuration list.

   procedure Sort_Basis;
   --  Sort the basis configuration list.

   function Xreturn return Configs.Config_Access;
   --  Return a pointer to the head of the configuration list and
   --  reset the List.

   function Basis return Configs.Config_Access;
   --  Return a pointer to the head of the configuration list and
   --  reset the list.

   procedure Eat (Config : in out Configs.Config_Access);
   --  Free all elements of the given configuration List.

   procedure Reset;
   --  Initialized the configuration list Builder.

end Config_Lists;
