--
--  The author disclaims copyright to this source code.  In place of
--  a legal notice, here is a blessing:
--
--    May you do good and not evil.
--    May you find forgiveness for yourself and forgive others.
--    May you share freely, not taking more than you give.
--

with Ada.Strings.Unbounded;

package body Report_Parsers is

   use Ada.Strings.Unbounded;
   type Context_Record is
      record
         ARG : Unbounded_String;
         CTX : Unbounded_String;
      end record;

   Global_Parser_Context : aliased Context_Record;

   function Is_Alnum (C : in Character) return Boolean;
   --


   function Get_Context return Context_Access
   is
   begin
      return Global_Parser_Context'Access;
   end Get_Context;


   function Get_ARG (Parser : in Context_Access) return String
   is
   begin
      return To_String (Parser.ARG);
   end Get_ARG;


   function Get_CTX (Parser : in Context_Access) return String
   is
   begin
      return To_String (Parser.CTX);
   end Get_CTX;


   procedure Trim_Right_Symbol (Item : in     String;
                                Pos  :    out Natural)
   is
      S : String renames Item;
   begin
      Pos := S'Last;

      --  Skip spaces
      while Pos >= 1 and S (Pos - 1) = ' ' loop
         Pos := Pos - 1;
      end loop;

      while Pos >= 1 and (Is_Alnum (S (Pos - 1)) or S (Pos - 1) = '_') loop
         Pos := Pos - 1;
      end loop;

   end Trim_Right_Symbol;


   function Is_Alnum (C : in Character) return Boolean
   is
   begin
      return
        C in 'a' .. 'z' or
        C in 'A' .. 'Z' or
        C in '0' .. '9';
   end Is_Alnum;


end Report_Parsers;
