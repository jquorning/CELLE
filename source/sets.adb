--
--
--

with Ada.Unchecked_Deallocation;

package body Sets is


   Index_First : Index_Type := Index_Type'First;
   Index_Last  : Index_Type := Index_Type'First;


   procedure Set_Range (First : in Index_Type;
                        Last  : in Index_Type)
   is
   begin
      Index_First := First;
      Index_Last  := Last;
   end Set_Range;


   function Set_New return Set_Type is
   begin
      return new Set_Array'(Index_First .. Index_Last => False);
   end Set_New;


   procedure Set_Free (Set : in out Set_Type) is
      procedure Free is
         new Ada.Unchecked_Deallocation (Object => Set_Array,
                                         Name   => Set_Type);
   begin
      Free (Set);
   end Set_Free;


   function Set_Add (Set  : in out Set_Type;
                     Item : in     Index_Type) return Boolean
   is
      RV : Boolean;
   begin
      pragma Assert (Item >= Index_First);
      pragma Assert (Item <= Index_Last);
      RV := Set (Item);
      Set (Item) := True;
      return not RV;
   end Set_Add;


   function Set_Union (Set_1 : in out Set_Type;
                       Set_2 : in out Set_Type) return Boolean
   is
      Progress : Boolean;
   begin
      Progress := False;
      for I in Index_First .. Index_Last loop
         if Set_2 (I) then
            if not Set_1 (I) then
               Progress := True;
               Set_1 (I) := True;
            end if;
         end if;
      end loop;
      return Progress;
   end Set_Union;


   function Set_Find (Set  : in Set_Type;
                      Item : in Index_Type) return Boolean
   is
   begin
      return Set (Item);
   end Set_Find;

   function First_Index return Index_Type is (Index_First);
   function Last_Index  return Index_Type is (Index_Last);

end Sets;
