--
--  The author disclaims copyright to this source code.  In place of
--  a legal notice, here is a blessing:
--
--    May you do good and not evil.
--    May you find forgiveness for yourself and forgive others.
--    May you share freely, not taking more than you give.
--

package Reports is

   procedure Reprint;
   --  Duplicate the input file without comments and without actions
   --  on rules

   procedure Report_Output;
   --  Generate the "*.out" log file

   procedure Report_Table;
   --  Generate C source code for the parser

   procedure Compress_Tables;
   --  Reduce the size of the action tables, if possible, by making use
   --  of defaults.
   --
   --  In this version, we take the most frequent REDUCE action and make
   --  it the default.  Except, there is no default if the wildcard token
   --  is a possible look-ahead.


   procedure Resort_States;
   --  Renumber and resort states so that states with fewer choices
   --  occur at the end.  Except, keep state 0 as the first state.

   procedure Dummy;

private

   pragma Export (C, Reprint,         "lemon_reprint");
   pragma Export (C, Report_Output,   "lemon_report_output");
   pragma Export (C, Report_Table,    "lemon_report_table");
   pragma Export (C, Compress_Tables, "lemon_compress_tables");
   pragma Export (C, Resort_States,   "lemon_resort_states");
   pragma Export (C, Dummy,           "lemon_report_dummy");

end Reports;
