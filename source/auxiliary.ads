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
   function Is_Alnum (C : in Character) return Boolean;

   generic
      type Num is range <>;
   function Trim_Image (Value : in Num)
                       return String;
   --  Trimmed image of Value.

   generic
      type Index_Type is (<>);
      type Element_Type is private;
      type Array_Type is array (Index_Type range <>) of Element_Type;
      type Array_Access is access Array_Type;
   procedure Resize_Array (Item     : in out Array_Access;
                           New_Last :        Index_Type;
                           Default  :        Element_Type);
   --  Resize item so that Item'Last is New_Last. Item'First is not changed.
   --  If New_Last < Item'Last'Old then truncate.
   --  If new_Last > Item'Last'Old then copy and leave junk in last part.
   --  Behaviour like C realloc.

end Auxiliary;
