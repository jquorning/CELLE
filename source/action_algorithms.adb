--
--
--

with Types;

package body Action_Algorithms is

   function Compute_Action (Session : Session_Type;
                            Action  : Action_Record)
                           return Integer
   is
      use Actions;
      use type Types.Symbol_Index;

      State_Num   : constant Integer := Action.X.State.State_Num;
      Min_Reduce  : constant Integer := Integer (Session.Min_Reduce);
      Min_SR      : constant Integer := Integer (Session.Min_Shift_Reduce);
      Rule_Number : constant Integer := Integer (Action.X.Rule.Number);
   begin
      case Action.Kind is

         when Shift =>
            return State_Num;

         when Shift_Reduce =>
            --  Since a SHIFT is inherient after a prior REDUCE, convert any
            --  SHIFTREDUCE action with a nonterminal on the LHS into a simple
            --  REDUCE action:
            if Action.Symbol.Index >= Session.N_Terminal then
               return Min_Reduce + Rule_Number;
            else
               return Min_SR + Rule_Number;
            end if;

         when Reduce   => return Min_Reduce + Rule_Number;
         when Error    => return Integer (Session.Err_Action);
         when C_Accept => return Integer (Session.Acc_Action);
         when others   => return -1;
      end case;
   end Compute_Action;

end Action_Algorithms;
