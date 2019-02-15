--
--  The author disclaims copyright to this source code.  In place of
--  a legal notice, here is a blessing:
--
--    May you do good and not evil.
--    May you find forgiveness for yourself and forgive others.
--    May you share freely, not taking more than you give.
--

with Ada.Text_IO;

with Auxiliary.Errors;

package body Errors is


   procedure Error_1 (File_Name : in String;
                      Start     : in String;
                      Name      : in String)
   is
   begin
      Auxiliary.Errors.Error
        (File_Name, 0,
         "The specified start symbol '" & Start &
           "' is not in a nonterminal of the grammar.  '" &
           Name & "' will be used as the start " &
           "symbol instead.");
   end Error_1;


   procedure Error_2 (File_Name   : in String;
                      Name        : in String)
   is
   begin
      Auxiliary.Errors.Error
        (File_Name, 0,
         "The start symbol '" & Name & "' occurs on " &
           "the right-hand side of a rule. This will " &
           "result in a parser which does not work properly.");
   end Error_2;


end Errors;
