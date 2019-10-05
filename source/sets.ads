--
--
--

generic
   type Index_Type is (<>);
package Sets is

   type Set_Type is private;
   Null_Set : constant Set_Type;

   procedure Set_Range (First : in Index_Type;
                        Last  : in Index_Type);
   --  All sets will be of size N
   --  Set the set size

   function Set_New return Set_Type;
   --  Allocate a new set
   --  A new set for element 0..N

   procedure Set_Free (Set : in out Set_Type);
   --  Deallocate a set

   function Set_Add (Set  : in out Set_Type;
                     Item : in     Index_Type) return Boolean;
   --  Add a new element to the set.  Return True if the element was added
   --  and FALSE if it was already there.

   function Set_Union (Set_1 : in out Set_Type;
                       Set_2 : in out Set_Type) return Boolean;
   --  Add every element of s2 to s1.  Return TRUE if s1 changes.
   --  A <- A U B, thru element N

   function Set_Find (Set  : in Set_Type;
                      Item : in Index_Type) return Boolean;
   --  True if Item is in set Set.

   function First_Index return Index_Type;
   function Last_Index  return Index_Type;

private

   type Set_Array is array (Index_Type range <>) of Boolean;
   type Set_Type  is access Set_Array;

   Null_Set : constant Set_Type := null;

end Sets;
