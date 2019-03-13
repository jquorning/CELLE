--
--  The author disclaims copyright to this source code.  In place of
--  a legal notice, here is a blessing:
--
--    May you do good and not evil.
--    May you find forgiveness for yourself and forgive others.
--    May you share freely, not taking more than you give.
--

package body Parser_Data is


   function Current_Char (Scanner : in Scanner_Record)
                         return Character
   is
   begin
      return Scanner.Item (Scanner.First);
   end Current_Char;


   function Current_Line (Scanner : in Scanner_Record)
                         return String
   is
   begin
      return Scanner.Item (Scanner.First .. Scanner.Last);
   end Current_Line;


   function Current_Token_Char (Scanner : in Scanner_Record)
                               return Character
   is
   begin
      return Scanner.Item (Scanner.Token);
   end Current_Token_Char;


   function Current_Token_Line (Scanner : in Scanner_Record)
                               return String
   is
   begin
      return Scanner.Item (Scanner.Token .. Scanner.Last);
   end Current_Token_Line;


   procedure Advance (Scanner : in out Scanner_Record;
                      By      : in     Positive)
   is
   begin
      Scanner.Token := Scanner.Token + By;
   end Advance;


end Parser_Data;
