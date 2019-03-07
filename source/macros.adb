--
--  The author disclaims copyright to this source code.  In place of
--  a legal notice, here is a blessing:
--
--    May you do good and not evil.
--    May you find forgiveness for yourself and forgive others.
--    May you share freely, not taking more than you give.
--

with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;

package body Macros is

   use Ada.Strings.Unbounded;
   package Macro_Vectors is
      new Ada.Containers.Vectors
     (Positive,
      Unbounded_String);

   Macros : Macro_Vectors.Vector;

   procedure Append (Name : in String)
   is
   begin
      Macros.Append (To_Unbounded_String (Name));
   end Append;

end Macros;
