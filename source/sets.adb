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
      return new Set_Array'(0 .. Size => '0');
--        char *s;
--    s = (char*)calloc( size, 1);
--    if( s==0 ){
--      extern void memory_error();
--      memory_error();
--    return s;

   end Set_New;


   procedure Set_Free (Set : in out Set_Type) is
      procedure Free is
         new Ada.Unchecked_Deallocation (Set_Array,
                                         Set_Type);
   begin
      Free (Set);
   end Set_Free;


   function Set_Add (Set  : in out Set_Type;
                     Item : in     Natural) return Boolean
   is
      RV : Boolean;
   begin
--  int lemon_set_add(char *s, int e)
      pragma Assert (Item >= 0 and Item < Size);
      RV := (Set (Item) /= '0');
      Set (Item) := '1';
      return RV;
   end Set_Add;


   function Set_Union (Set_1 : in out Set_Type;
                       Set_2 : in out Set_Type) return Boolean
   is
      Progress : Boolean;
   begin
--  int SetUnion(char *s1, char *s2)
      Progress := False;
      for I in 0 .. Size - 1 loop
         if Set_2 (I) = '0' then
            null;
         else
            if Set_1 (I) = '0' then
               Progress := True;
               Set_1 (I) := '1';
            end if;
         end if;
      end loop;
      return Progress;
   end Set_Union;


   function Set_Find (Set  : in Set_Type;
                      Item : in Natural) return Boolean is
   begin
      return Set (Item) = '1';
   end Set_Find;


end Sets;
