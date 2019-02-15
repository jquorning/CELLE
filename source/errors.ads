--
--  The author disclaims copyright to this source code.  In place of
--  a legal notice, here is a blessing:
--
--    May you do good and not evil.
--    May you find forgiveness for yourself and forgive others.
--    May you share freely, not taking more than you give.
--

package Errors is

   procedure Error (File_Name   : in String;
                    Line_Number : in Natural;
                    Message     : in String);


   procedure Error_1 (File_Name : in String;
                      Start     : in String;
                      Name      : in String);


   procedure Error_2 (File_Name : in String;
                      Name      : in String);

end Errors;
