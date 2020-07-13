--
--  The author disclaims copyright to this source code.  In place of
--  a legal notice, here is a blessing:
--
--    May you do good and not evil.
--    May you find forgiveness for yourself and forgive others.
--    May you share freely, not taking more than you give.
--

with Ada.Text_IO;

with Sessions;

package Reports is

   subtype File_Type    is Ada.Text_IO.File_Type;
   subtype Session_Type is Sessions.Session_Type;

   procedure Reprint (File    : File_Type;
                      Session : Session_Type);
   --  Duplicate the input file without comments and without actions
   --  on rules

   procedure Report_Output (Session : Session_Type);
   --  Generate the "*.out" log file

   procedure Report_Table (Session            : in out Session_Type;
                           User_Template_Name : in     String);
   --  Generate C source code for the parser

   procedure Compress_Tables (Session : in out Session_Type);
   --  Reduce the size of the action tables, if possible, by making use
   --  of defaults.
   --
   --  In this version, we take the most frequent REDUCE action and make
   --  it the default.  Except, there is no default if the wildcard token
   --  is a possible look-ahead.


   procedure Resort_States (Session : in out Session_Type);
   --  Renumber and resort states so that states with fewer choices
   --  occur at the end.  Except, keep state 0 as the first state.

   procedure Report_Header
     (Session       : Session_Type;
      Token_Prefix  : String;
      Base_Name     : String;
      Module_Name   : String;
      Terminal_Last : Natural);
   --  Generate a header file for the Parser.


end Reports;
