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

   function Clean_Session return Session_Type is
   begin
      return
        Session_Type'(Sorted       => State_Vectors.Empty_Vector,
                      Rule         => Rule_Lists.Lists.Empty_List,
                      Start_Rule   => Rule_Lists.Lists.No_Element,
                      Num_X_State  => 0,
                      Num_Symbol   => 0,          Num_Terminal   => 0,
                      Min_Shift_Reduce => 0,      Err_Action   => 0,
                      Acc_Action   => 0,         No_Action        => 0,
                      Min_Reduce   => 0,          Max_Action   => 0,
                      Symbols2         => 999,
                      Error_Cnt    => 0,          Error_Symbol => null,
                      Wildcard         => null,
                      Names        => Parser_Names'Access,
                      File_Name    => Null_UString,
                      Out_Name     => Null_UString,
                      Num_Conflict      => 0,
                      Num_Action_Tab    => 0,
                      Num_Lookahead_Tab => 0,
                      Table_Size      => 0,
                      Basis_Flag      => False,
                      Has_Fallback    => False,
                      No_Linenos_Flag => False,
                      Argv0           => Null_UString,
                      Parser          => Report_Parsers.Get_Context,
                      Num_Rule_With_Action => 0);
   end Clean_Session;

end Sessions;
