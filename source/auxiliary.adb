--
--  The author disclaims copyright to this source code.  In place of
--  a legal notice, here is a blessing:
--
--    May you do good and not evil.
--    May you find forgiveness for yourself and forgive others.
--    May you share freely, not taking more than you give.
--

with Ada.Characters.Handling;

package body Auxiliary is


   procedure Recreate
     (File      : in out File_Type;
      Mode      : in     File_Mode;
      File_Name : in     String)
   is
   begin
      Create (File, Mode, File_Name);
   exception
      when others =>
         Open (File, Mode, File_Name);
   end Recreate;


   function To_Ada_Symbol (Text : in String) return String is
      use Ada.Characters.Handling;
      Result : String (Text'Range);
      Start  : Boolean := True;
   begin
      for I in Result'Range loop
         if Start then
            Result (I) := To_Upper (Text (I));
            Start := False;
         else
            Result (I) := To_Lower (Text (I));
         end if;

         --  '_' leads to upper case
         if Text (I) = '_' then
            Start := True;
         end if;

         --  Replace '-' with '.' from GNAT style file names to child package names.
         if Text (I) = '-' then
            Result (I) := '.';
            Start := True;
         end if;

      end loop;
      return Result;
   end To_Ada_Symbol;

end Auxiliary;
