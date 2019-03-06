--
--  The author disclaims copyright to this source code.  In place of
--  a legal notice, here is a blessing:
--
--    May you do good and not evil.
--    May you find forgiveness for yourself and forgive others.
--    May you share freely, not taking more than you give.
--

with Lime;

package Reports is

   procedure Reprint_C (Lemp : access Lime.Lemon_Record);
   procedure Reprint (Lemp : in Lime.Lemon_Record);
   --  Duplicate the input file without comments and without actions
   --  on rules

   procedure Report_Output_C (Lemp : access Lime.Lemon_Record);
   procedure Report_Output (Lemp : in Lime.Lemon_Record);
   --  Generate the "*.out" log file

   procedure Report_Table_C (Lemp : access Lime.Lemon_Record);
   procedure Report_Table (Lemp               : in out Lime.Lemon_Record;
                           User_Template_Name : in     String);
   --  Generate C source code for the parser

   procedure Compress_Tables_C (Lemp : access Lime.Lemon_Record);
   procedure Compress_Tables (Lemp : in Lime.Lemon_Record);
   --  Reduce the size of the action tables, if possible, by making use
   --  of defaults.
   --
   --  In this version, we take the most frequent REDUCE action and make
   --  it the default.  Except, there is no default if the wildcard token
   --  is a possible look-ahead.


   procedure Resort_States_C (Lemp : access Lime.Lemon_Record);
   procedure Resort_States (Lemp : in Lime.Lemon_Record);
   --  Renumber and resort states so that states with fewer choices
   --  occur at the end.  Except, keep state 0 as the first state.

   procedure Dummy;


   procedure Reprint_Of_Grammar
     (Lemon_Lemp    : in out Lime.Lemon_Record;
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
