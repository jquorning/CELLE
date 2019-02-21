--
--  The author disclaims copyright to this source code.  In place of
--  a legal notice, here is a blessing:
--
--    May you do good and not evil.
--    May you find forgiveness for yourself and forgive others.
--    May you share freely, not taking more than you give.
--

package Errors is

   type Kind is
     (W001,
      W002,
      E001,
      E002,
      E003,
      E004,
      E005,
      E006,
      E007,
      F001,
      F002
     );
   subtype Kind_Warning is Kind range W001 .. W001;
   subtype Kind_Error   is Kind range W001 .. Kind'Val (Kind'Pos (F001) - 1);
   subtype Kind_Fatal   is Kind range F001 .. Kind'Last;

   type K_Error_Parse_One_Token is new Kind range E001 .. E007;

   procedure Error_1 (File_Name : in String;
                      Start     : in String;
                      Name      : in String);


   procedure Error_2 (File_Name : in String;
                      Name      : in String);

   procedure Error (File_Name   : String;
                    Line_Number : Natural;
                    Kind        : K_Error_Parse_One_Token);

end Errors;
