--
--  The author disclaims copyright to this source code.  In place of
--  a legal notice, here is a blessing:
--
--    May you do good and not evil.
--    May you find forgiveness for yourself and forgive others.
--    May you share freely, not taking more than you give.
--

with Sessions;

package Reports is

   procedure Reprint_C (Session : access Sessions.Session_Type);
   procedure Reprint (Session : in Sessions.Session_Type);
   --  Duplicate the input file without comments and without actions
   --  on rules

   procedure Report_Output_C (Session : access Sessions.Session_Type);
   procedure Report_Output (Session : in Sessions.Session_Type);
   --  Generate the "*.out" log file

   procedure Report_Table_C (Session : access Sessions.Session_Type);
   procedure Report_Table (Session            : in out Sessions.Session_Type;
                           User_Template_Name : in     String);
   --  Generate C source code for the parser

   procedure Compress_Tables_C (Session : access Sessions.Session_Type);
   procedure Compress_Tables (Session : in Sessions.Session_Type);
   --  Reduce the size of the action tables, if possible, by making use
   --  of defaults.
   --
   --  In this version, we take the most frequent REDUCE action and make
   --  it the default.  Except, there is no default if the wildcard token
   --  is a possible look-ahead.


   procedure Resort_States_C (Session : access Sessions.Session_Type);
   procedure Resort_States (Session : in Sessions.Session_Type);
   --  Renumber and resort states so that states with fewer choices
   --  occur at the end.  Except, keep state 0 as the first state.

   procedure Dummy;


   procedure Reprint_Of_Grammar
     (Session       : in out Sessions.Session_Type;
      Base_Name     : in     String;
      Token_Prefix  : in     String;
      Terminal_Last : in     Natural);

private

   pragma Export (C, Reprint_C,         "lemon_reprint");
   pragma Export (C, Report_Output_C,   "lemon_report_output");
   pragma Export (C, Report_Table_C,    "lemon_report_table");
   pragma Export (C, Compress_Tables_C, "lemon_compress_tables");
   pragma Export (C, Resort_States_C,   "lemon_resort_states");
   pragma Export (C, Dummy,             "lemon_report_dummy");

end Reports;
