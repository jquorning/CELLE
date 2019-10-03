--
--  The author disclaims copyright to this source code.  In place of
--  a legal notice, here is a blessing:
--
--    May you do good and not evil.
--    May you find forgiveness for yourself and forgive others.
--    May you share freely, not taking more than you give.
--

with GNAT.Strings;

package Options is

   Show_Conflict : aliased Boolean;
   Show_Version  : aliased Boolean;
   RP_Flag       : aliased Boolean;
   Basis_Flag    : aliased Boolean;
   Compress      : aliased Boolean;
   Be_Quiet      : aliased Boolean;
   Statistics    : aliased Boolean;
   MH_Flag       : aliased Boolean;
   No_Line_Nos   : aliased Boolean;
   No_Resort     : aliased Boolean;
   Show_Help     : aliased Boolean;

   use GNAT.Strings;
   Program_Name  : aliased String_Access := new String'("");
   Input_File    : aliased String_Access := new String'("parse.y");
   User_Template : aliased String_Access := new String'("");
   Output_Dir    : aliased String_Access := new String'(".");


   Placeholder_Dummy : aliased GNAT.Strings.String_Access;
   Language_String   : aliased GNAT.Strings.String_Access :=
     new String'("C");
   --  C is the default language like in Lemon.

   --  High level options
   type Language_Type is (Language_Ada, Language_C);
   Language : Language_Type := Language_C;
   --  Not used by C parts.

   procedure Set_Language;

end Options;
