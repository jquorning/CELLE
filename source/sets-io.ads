with Sessions;

package Sets.IO is

   procedure Put (Set : in Set_Type);

   procedure Put_Named (Session : in Sessions.Session_Type;
                        Set     : in Set_Type);

end Sets.IO;
