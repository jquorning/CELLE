--
--
--

with Ada.Unchecked_Deallocation;

package body Sets is


   Size : Natural := 0;


   procedure Set_Size (N : in Natural) is
   begin
      Size := N + 1;
   end Set_Size;


   function Set_New return Set_Type is
   begin
      return new Set_Array'(0 .. Size => False);
--        char *s;
--    s = (char*)calloc( size, 1);
--    if( s==0 ){
--      extern void memory_error();
--      memory_error();
--    return s;

   end Set_New;


   procedure Set_Free (Set : in out Set_Type) is
      procedure Free is
         new Ada.Unchecked_Deallocation (Object => Set_Array,
                                         Name   => Set_Type);
   begin
      Free (Set);
   end Set_Free;


   function Set_Add (Set  : in out Set_Type;
                     Item : in     Natural) return Boolean
   is
      RV : Boolean;
   begin
      pragma Assert (Item >= 0 and Item < Size);
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
      for I in 0 .. Size - 1 loop
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
                      Item : in Natural) return Boolean
   is
   begin
      return Set (Item);
   end Set_Find;


end Sets;
