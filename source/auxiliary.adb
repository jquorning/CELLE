--
--  The author disclaims copyright to this source code.  In place of
--  a legal notice, here is a blessing:
--
--    May you do good and not evil.
--    May you find forgiveness for yourself and forgive others.
--    May you share freely, not taking more than you give.
--

with Ada.Strings.Fixed;
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


   function Is_Upper (C : in Character) return Boolean is
   begin
      case C is
         when 'A' .. 'Z' =>
            return True;
         when others     =>
            return False;
      end case;
   end Is_Upper;


   function Is_Lower (C : in Character) return Boolean is
   begin
      case C is
         when 'a' .. 'z' =>
            return True;
         when others     =>
            return False;
      end case;
   end Is_Lower;


   function Is_Alpha (C : in Character) return Boolean is
   begin
      case C is
         when 'A' .. 'Z' =>
            return True;
         when 'a' .. 'z' =>
            return True;
         when others     =>
            return False;
      end case;
   end Is_Alpha;


   function Is_Alnum (C : in Character) return Boolean
   is
   begin
      return
        C in 'a' .. 'z' or
        C in 'A' .. 'Z' or
        C in '0' .. '9';
   end Is_Alnum;

   ----------------
   -- Trim_Image --
   ----------------

   function Trim_Image (Value : in Num) return String is
      use Ada.Strings;
   begin
      return Fixed.Trim (Num'Image (Value), Left);
   end Trim_Image;

   ------------------
   -- Resize_Array --
   ------------------

   --  generic
   --     type Index_Type is (<>);
   --     type Element_Type is private;
   --     type Array_Type is array (Index_Type range <>) of Element_Type;
   --     type Array_Access is access Array_Type;
   procedure Resize_Array (Item     : in out Array_Access;
                           New_Last : in     Index_Type;
                           Default  : in     Element_Type)
   is
   begin
      if New_Last > Item'Last then
         declare
            subtype New_Range is Index_Type range Item'First .. New_Last;
            New_Item : constant Array_Access :=
              new Array_Type'(New_Range => Default);
         begin
            New_Item (New_Range'Range) := Item.all;
            Item := New_Item;
         end;
      elsif New_Last < Item'Last then
         declare
            subtype New_Range is Index_Type range Item'First .. New_Last;
            New_Item : constant Array_Access :=
              new Array_Type'(New_Range => Default);
         begin
            New_Item.all := Item (New_Range'Range);
            Item := New_Item;
         end;
      else
         null; -- Keep item
      end if;
   end Resize_Array;

end Auxiliary;
