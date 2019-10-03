--
--  The author disclaims copyright to this source code.  In place of
--  a legal notice, here is a blessing:
--
--    May you do good and not evil.
--    May you find forgiveness for yourself and forgive others.
--    May you share freely, not taking more than you give.
--

with Rules;
with Sets;
with Prop_Links;
limited with States;

package Configs is

   --  A configuration is a production rule of the grammar together with
   --  a mark (dot) showing how much of that rule has been processed so far.
   --  Configurations also contain a follow-set which is a list of terminal
   --  symbols which are allowed to immediately follow the end of the rule.
   --  Every configuration is recorded as an instance of the following:

   type Config_Status is (Complete, Incomplete);

   type Config_Record;
   type Config_Access is access all Config_Record;

   type Config_Record is record
      Rule        : Rules.Rule_Access;          --  The rule upon which the configuration is based
      Dot         : Rules.Dot_Type;             --  The parse point
      Follow_Set  : Sets.Set_Type;              --  Follow-set for this configuration only
      Forward_PL  : Prop_Links.List;            --  Forward propagation links
      Backward_PL : Prop_Links.List;            --  Follow-set backwards propagation links
      State       : access States.State_Record; --  Pointer to state which contains this
      Status      : Config_Status;              --  Used during followset and shift computations
      Next        : Config_Access;              --  Next configuration in the state
      Basis       : Config_Access;              --  The next basis configuration
   end record;

   function "<" (Left, Right : in Config_Record) return Boolean;
   function "<" (Left, Right : in Config_Access) return Boolean;

end Configs;
