--
--  The author disclaims copyright to this source code.  In place of
--  a legal notice, here is a blessing:
--
--    May you do good and not evil.
--    May you find forgiveness for yourself and forgive others.
--    May you share freely, not taking more than you give.
--

with Backend;
with Lime;

package Generate_C is

   use Backend;

   procedure Open_Template
--     (Cherry        : in     Cherry_Interface;
     (Context       : in out Context_Type;
      User_Template : in     String;
      File_Name     : in     String;
      Error_Count   : in out Integer);

   procedure Generate_Spec
     (Lemp      : in     Lime.Lemon_Record;
      Context   : in out Context_Type;
      File_Name : in     String;
      Module    : in     String;
      Prefix    : in     String;
      First     : in     Integer;
      Last      : in     Integer);

end Generate_C;
