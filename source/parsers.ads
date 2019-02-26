--
--  The author disclaims copyright to this source code.  In place of
--  a legal notice, here is a blessing:
--
--    May you do good and not evil.
--    May you find forgiveness for yourself and forgive others.
--    May you share freely, not taking more than you give.
--

--  with Ada.Strings.Unbounded;

package Parsers is

   type Context_Access is private;

   --  (Global : in Lime.Lemon_Record) XXX
   function Get_Context return Context_Access;
   --  Get access to the parser part of then Lemon_Record

--
   function Get_ARG (Parser : in Context_Access) return String;
   function Get_CTX (Parser : in Context_Access) return String;

   procedure Trim_Right_Symbol (Item : in     String;
                                Pos  :    out Natural);
   --  Split Item at Pos

private

   type Context_Record;
   type Context_Access is access all Context_Record;

end Parsers;
