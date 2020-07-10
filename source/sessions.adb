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


   procedure Create_Sorted_States (Session : in out Session_Type)
   is

   begin
      Sorted_Vector.Clear;
      States.Iterate (Append_State_To_Sorted'Access);
      Session.Sorted := Sorted_Vector;
      Sorted_Vector.Clear;
   end Create_Sorted_States;


end Sessions;
