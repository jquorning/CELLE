--
--  The author disclaims copyright to this source code.  In place of
--  a legal notice, here is a blessing:
--
--    May you do good and not evil.
--    May you find forgiveness for yourself and forgive others.
--    May you share freely, not taking more than you give.
--
-----------------------------------------------------------------------------
--  This Setup body template is a template for creating setup.adb.
--  Create setup.adb by updating the file PROGRAM_VERSION and running
--  create-setup-adb.sh from same directory.
--

package body Setup is

   function Get_Program_Version return String is
   begin
      return "0.0.4";
   end Get_Program_Version;

   function Get_Build_ISO8601 return String is
   begin
      return "2019-02-04T06:55:22";
   end Get_Build_ISO8601;

   function Get_Uname_M return String is
   begin
      return "x86_64";
   end Get_Uname_M;

   function Get_Uname_N return String is
   begin
      return "pro";
   end Get_Uname_N;

   function Get_Uname_P return String is
   begin
      return "i386";
   end Get_Uname_P;

   function Get_Uname_R return String is
   begin
      return "17.7.0";
   end Get_Uname_R;

   function Get_Uname_S return String is
   begin
      return "Darwin";
   end Get_Uname_S;

end Setup;
