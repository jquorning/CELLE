--
--
--

with Ada.Strings.Unbounded;
with Backend;

package Generate_Ada is

   use Backend;

   procedure Open_Template
     (Context       : in out Context_Type;
      File_Name     : in out Ada.Strings.Unbounded.Unbounded_String;
      User_Template : in     String;
      Error_Count   : in out Integer);

   procedure Generate_Spec
     (Context   : in out Context_Type;
      Base_Name : in     String;
      Module    : in     String;
      Prefix    : in     String;
      First     : in     Integer;
      Last      : in     Integer);

end Generate_Ada;
