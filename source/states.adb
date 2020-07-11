--
--  The author disclaims copyright to this source code.  In place of
--  a legal notice, here is a blessing:
--
--    May you do good and not evil.
--    May you find forgiveness for yourself and forgive others.
--    May you share freely, not taking more than you give.
--

with Ada.Containers.Ordered_Maps;

with Rules;

package body States is

   function State_Compare (Left, Right : in Configs.Config_Access) return Boolean;

   function State_Compare (Left, Right : in Configs.Config_Access) return Boolean
   is
      use type Rules.Index_Number;
      use type Rules.Dot_Type;
      use Configs;

      A  : Config_Access := Left;
      B  : Config_Access := Right;
      RC : Integer;
   begin
      RC := 0;
      while RC = 0 and A /= null and B /= null loop
         RC := Integer (A.Rule.Index - B.Rule.Index);
         if RC = 0 then
            RC := Integer (A.Dot - B.Dot);
         end if;
         A := A.Basis;
         B := B.Basis;
      end loop;

      if RC = 0 then
         if A /= null then RC :=  1; end if;
         if B /= null then RC := -1; end if;
      end if;
      return RC < 0;
   end State_Compare;


   package State_Maps is
      new Ada.Containers.Ordered_Maps (Key_Type     => Configs.Config_Access,
                                       Element_Type => State_Access,
                                       "<"          => State_Compare);

   State_Map : State_Maps.Map;


   procedure Initialize is
   begin
      State_Map.Clear;
   end Initialize;


   function Find (Config : in not null Configs.Config_Access) return State_Access
   is
      use State_Maps;

      Pos : constant Cursor := State_Map.Find (Config);
   begin
      return (if Pos = No_Element then null else Element (Pos));
   end Find;


   function Create return State_Access is
   begin
      return new State_Record;
   end Create;


   procedure Insert (State  : in State_Access;
                     Config : in Configs.Config_Access)
   is
   begin
      State_Map.Insert (Config, State);
   end Insert;

   procedure Iterate_Process (Position : in State_Maps.Cursor);

   Static_Process : Process_Access;

   procedure Iterate_Process (Position : in State_Maps.Cursor) is
   begin
      Static_Process.all (State_Maps.Element (Position));
   end Iterate_Process;

   procedure Iterate (Process : in Process_Access)
   is
   begin
      Static_Process := Process;
      State_Map.Iterate (Iterate_Process'Access);
   end Iterate;

end States;
