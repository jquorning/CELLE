--
--  The author disclaims copyright to this source code.  In place of
--  a legal notice, here is a blessing:
--
--    May you do good and not evil.
--    May you find forgiveness for yourself and forgive others.
--    May you share freely, not taking more than you give.
--

with Ada.Text_IO;

package Auxiliary is

   use Ada.Text_IO;

   procedure Recreate
     (File      : in out File_Type;
      Mode      : in     File_Mode;
      File_Name : in     String);
   --  Create or open file.

   function To_Ada_Symbol (Text : in String) return String;
   --  Convert into ada symbol

   function Is_Upper (C : in Character) return Boolean;
   function Is_Lower (C : in Character) return Boolean;
   function Is_Alpha (C : in Character) return Boolean;

   function Image (Value : in Integer)
                  return String;
   --  Trimmed image of Value.

end Auxiliary;
