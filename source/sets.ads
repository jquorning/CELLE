--
--
--

package Sets is

   type Set_Type is private;
   Null_Set : constant Set_Type;

   procedure Set_Size (N : in Natural);
   --  All sets will be of size N
   --  Set the set size

   function Set_New return Set_Type;
   --  Allocate a new set
   --  char *SetNew(void);               /* A new set for element 0..N */

   procedure Set_Free (Set : in out Set_Type);
   --  Deallocate a set
   --  void  SetFree(char*);             /* Deallocate a set */

   function Set_Add (Set  : in out Set_Type;
                     Item : in     Natural) return Boolean;
   --  Add a new element to the set.  Return TRUE if the element was added
   --  and FALSE if it was already there.
   --  int lemon_set_add(char*,int);            /* Add element to a set */

   function Set_Union (Set_1 : in out Set_Type;
                       Set_2 : in out Set_Type) return Boolean;
   --  Add every element of s2 to s1.  Return TRUE if s1 changes.
   --  int SetUnion(char *,char *);    /* A <- A U B, thru element N */

   function Set_Find (Set : in Set_Type; Item : in Natural) return Boolean;
   --  #define SetFind(X,Y) (X[Y])       /* True if Y is in set X */

private

   type Set_Array is array (Natural range <>) of Boolean;
   type Set_Type  is access Set_Array;

   Null_Set : constant Set_Type := null;

end Sets;
