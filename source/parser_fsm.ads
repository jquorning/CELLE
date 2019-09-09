--
--  The author disclaims copyright to this source code.  In place of
--  a legal notice, here is a blessing:
--
--    May you do good and not evil.
--    May you find forgiveness for yourself and forgive others.
--    May you share freely, not taking more than you give.
--

with Sessions;
with Parser_Data;

package Parser_FSM is

   procedure Initialize_FSM (Session : in out Sessions.Session_Type;
                             Scanner : in out Parser_Data.Scanner_Record);
   --  Initialize the parser finite state machine

   procedure Do_State (Session : in out Sessions.Session_Type;
                       Scanner : in out Parser_Data.Scanner_Record;
                       Token   : in     String);
   --  The parser state machine

end Parser_FSM;
