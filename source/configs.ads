--
--  The author disclaims copyright to this source code.  In place of
--  a legal notice, here is a blessing:
--
--    May you do good and not evil.
--    May you find forgiveness for yourself and forgive others.
--    May you share freely, not taking more than you give.
--

with Interfaces.C.Strings;

limited with Rules;
limited with States;

package Configs is

   type cfgstatus is
     (COMPLETE,
      INCOMPLETE);
   pragma Convention (C, cfgstatus);  -- lemon.h:125

   type Config_Record;
   type Config_Access is access all Config_Record;

   --  A followset propagation link indicates that the contents of one
   --  configuration followset should be propagated to another whenever
   --  the first changes.   pragma Convention (C_Pass_By_Copy, State_Record);
   type Plink_Record is
      record
         cfp  : access Configs.Config_Record; --  The configuration to which linked
         next : access Plink_Record;          --  The next propagate link
      end record;
   pragma Convention (C_Pass_By_Copy, Plink_Record);  -- lemon.h:188

   type Config_Record is record
      RP          : Rules.Rule_Access;         --  The rule upon which the configuration is based
      DOT         : aliased Integer;           --  The parse point
      Follow_Set  : Interfaces.C.Strings.chars_ptr; --  FWS, Follow-set for this configuration only
      FS_Forward  : access Plink_Record;              --  fplp, forward propagation links
      FS_Backward : access Plink_Record;         --  bplp; Follow-set backwards propagation links
      stp         : access States.State_Record;  --  Pointer to state which contains this
      status      : aliased cfgstatus;         --  used during followset and shift computations
      Next        : Config_Access;             --  Next configuration in the state
      Basis       : Config_Access;             --  bp, The next basis configuration
   end record;
   pragma Convention (C_Pass_By_Copy, Config_Record);

end Configs;
