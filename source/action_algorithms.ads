--
--
--

with Sessions;
with Actions;

package Action_Algorithms is

   subtype Session_Type  is Sessions.Session_Type;
   subtype Action_Record is Actions.Action_Record;

   function Compute_Action (Session : Session_Type;
                            Action  : Action_Record)
                           return Integer;
   --  Given an action, compute the integer value for that action
   --  which is to be put in the action table of the generated machine.
   --  Return negative if no action should be generated.

end Action_Algorithms;
