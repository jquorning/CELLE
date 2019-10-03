--
--  The author disclaims copyright to this source code.  In place of
--  a legal notice, here is a blessing:
--
--    May you do good and not evil.
--    May you find forgiveness for yourself and forgive others.
--    May you share freely, not taking more than you give.
--

with Configs;

package Config_Tables is

   procedure Init;
   --  Allocate a new associative array.

   procedure Insert (Config : in Configs.Config_Access);
   --  Insert a new record into the array.  Return TRUE if successful.
   --  Prior data with the same key is NOT overwritten.

   function Find (Config : in Configs.Config_Access)
                 return Configs.Config_Access;
   --  Return a pointer to data assigned to the given key.  Return NULL
   --  if no such key.

   procedure Clear;
   --  Remove all data from the table.  Pass each data to the function "f"
   --  as it is removed.  ("f" may be null to avoid this step.)

end Config_Tables;
