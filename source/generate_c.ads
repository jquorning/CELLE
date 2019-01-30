--
--
--

--  with Generator;
with Backend;

package Generate_C is

--   use Generator;
   use Backend;

   procedure Open_Template
--     (Cherry        : in     Cherry_Interface;
     (Context       : in out Context_Type;
      User_Template : in     String;
      File_Name     : in     String;
      Error_Count   : in out Integer);

   procedure Generate_Spec
--     (Cherry    : in     Cherry_Interface;
     (Context   : in out Context_Type;
      File_Name : in     String;
      Module    : in     String;
      Prefix    : in     String;
      First     : in     Integer;
      Last      : in     Integer);

end Generate_C;
