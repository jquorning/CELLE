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

   Show_Conflict   : aliased Boolean := False;
   Show_Version    : aliased Boolean := False;
   RP_Flag         : aliased Boolean := False;
   Basis_Flag      : aliased Boolean := False;
   Compress        : aliased Boolean := False;
   Be_Quiet        : aliased Boolean := False;
   Statistics      : aliased Boolean := False;
   Make_Headers    : aliased Boolean := False; --  Output makeheaders compompatible
   No_Line_Nos     : aliased Boolean := False;
   No_Resort       : aliased Boolean := False;
   Show_Help       : aliased Boolean := False;
   Generate_SQL    : aliased Boolean := False;

   Debug_Level     : aliased Integer := 0;

   use GNAT.Strings;
   Program_Name  : aliased String_Access := new String'("");
   Input_File    : aliased String_Access := new String'("parse.y");
   User_Template : aliased String_Access := new String'("");
   Output_Dir    : aliased String_Access := new String'(".");


   Placeholder_Dummy : aliased GNAT.Strings.String_Access := null;
   Language_String   : aliased GNAT.Strings.String_Access :=
     new String'("C");
   --  C is the default language like in Lemon.

   --  High level options
   type Language_Type is (Language_Ada, Language_C);
   Language : Language_Type := Language_C;
   --  Not used by C parts.

   procedure Set_Language;

end Options;
