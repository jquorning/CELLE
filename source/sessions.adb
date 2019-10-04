--
--
--

package body Sessions is

   procedure Append_State_To_Sorted (State : in States.State_Access);
   Sorted_Vector : State_Vectors.Vector := State_Vectors.Empty_Vector;

   procedure Append_State_To_Sorted (State : in States.State_Access)
   is
   begin
      Sorted_Vector.Append (State);
   end Append_State_To_Sorted;


   function Create_Sorted_States return State_Vectors.Vector
   is
   begin
      Sorted_Vector.Clear;
      States.Iterate (Append_State_To_Sorted'Access);
      return Sorted_Vector;
   end Create_Sorted_States;


end Sessions;
