--
--  The author disclaims copyright to this source code.  In place of
--  a legal notice, here is a blessing:
--
--    May you do good and not evil.
--    May you find forgiveness for yourself and forgive others.
--    May you share freely, not taking more than you give.
--

with Ada.Containers.Ordered_Maps;

package body Config_Tables is

--   use Configs;

   package Config_Maps is
      new Ada.Containers.Ordered_Maps
     ("<"          => Configs."<",
      "="          => Configs."=",
      Key_Type     => Configs.Config_Access,
      Element_Type => Configs.Config_Access);


   Table : Config_Maps.Map := Config_Maps.Empty_Map;


   procedure Init is
   begin
      null;  --      Table.Capacity (64);
   end Init;


   procedure Insert (Config : in Configs.Config_Access)
   is
   begin
      Table.Insert (Config, Config);
   end Insert;


   function Find (Config : in Configs.Config_Access)
                 return Configs.Config_Access
   is
      use Config_Maps;

      Pos : constant Cursor := Table.Find (Config);
   begin
      return (if Pos = No_Element then null else Element (Pos));
   end Find;


   procedure Clear
   is
   begin
      Table.Clear;
   end Clear;


end Config_Tables;
