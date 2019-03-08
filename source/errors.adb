--
--  The author disclaims copyright to this source code.  In place of
--  a legal notice, here is a blessing:
--
--    May you do good and not evil.
--    May you find forgiveness for yourself and forgive others.
--    May you share freely, not taking more than you give.
--

with Ada.Text_IO;
with Ada.Strings.Fixed;

with DK8543.Errors;

package body Errors is


   procedure Error_Plain (File_Name   : in Unbounded_String;
                          Line_Number : in Natural;
                          Text        : in String;
                          Arguments   : in Argument_List)
   is
      pragma Unreferenced (Arguments);
   begin
      DK8543.Errors.Error
        (Ada.Text_IO.Standard_Output,
         Ada.Strings.Unbounded.To_String (File_Name),
         Line_Number, Text);
   end Error_Plain;

--     procedure Error_X1 (File_Name : in String;
--                         Start     : in String;
--                         Name      : in String)
--     is
--        use DK8543.Errors;
--     begin
--        Error
--          (File_Name, 0,
--           "The specified start symbol '" & Start &
--             "' is not in a nonterminal of the grammar.  '" &
--             Name & "' will be used as the start " &
--             "symbol instead.");
--     end Error_X1;


--     procedure Error_X2 (File_Name   : in String;
--                         Name        : in String)
--     is
--        use DK8543.Errors;
--     begin
--        Error
--          (File_Name, 0,
--           "The start symbol '" & Name & "' occurs on " &
--             "the right-hand side of a rule. This will " &
--             "result in a parser which does not work properly.");
--     end Error_X2;


   type String_Access is access all String;

   function "-" (Item : String) return String_Access;
   function "-" (Item : String) return String_Access is
   begin
      return new String'(Item);
   end "-";

   Table : constant array (K_Error_Parse_One_Token) of String_Access :=
     (E001 => -("There is no prior rule upon which to attach the code fragment which " &
                  "begins on this line."),
      E002 => -("Code fragment beginning on this line is not the first to follow the " &
                  "previous rule."),
      E003 => -"Token 'XXX' should be either '%%' or a nonterminal name.",
      E004 => -"The precedence symbol must be a terminal.",
      E005 => -"There is no prior rule to assign precedence '[XXX]'.",
      E006 => -"Precedence mark on this line is not the first to follow the previous rule.",
      E007 => -"Missing ']' on precedence mark.",
      E008 => -"Expected to see a ':' following the LHS symbol '%1'.",
      E009 => -"'%1' is not a valid alias for the LHS '%2'",
      E010 => -"Missing ')' following LHS alias name '%1'.",
      E011 => -"Missing '->' following: '%1(%2)'.",
      E012 => -"'%1' is not a valid alias for the RHS symbol '%2'",
      E013 => -"Missing ')' following LHS alias name '%1'.",

      E101 => -"String starting on this line is not terminated before the end of the file.",
      E102 => -"String starting on this line is not terminated before the end of the file.",
      E103 => -"Can't open this file for reading.",

      E201 => -"Cannot form a compound containing a non-terminal",
      E202 => -"Illegal character on RHS of rule: '%1'.",
      E203 => -"Unknown declaration keyword: '%%1'.",
      E204 => -"Illegal declaration keyword: '%1'.",
      E205 => -"Symbol name missing after %%destructor keyword",
      E206 => -"Symbol name missing after %%type keyword",
      E207 => -"Symbol %%type '%1' already defined",
      E208 => -"%%token_class argument '%1' should be a token",
      E209 => -"%%token_class must be followed by an identifier: '%1'",
      E210 => -"Symbol '%1' already used",
      E211 => -"%%wildcard argument '%1' should be a token",
      E212 => -"Extra wildcard to token: '%1'",
      E213 => -"Illegal argument to %%%1: %2"
     );


   procedure Error
     (Kind        : in K_Error_Parse_One_Token;
      Line_Number : in Natural;
      Arguments   : in Argument_List := Null_Argument_List)
   is
      use Ada.Strings;

      File_Name  : constant String  := To_String (Default_File_Name);
      Kind_Image : constant String  := Kind'Image & " ";
      Message    : Unbounded_String := To_Unbounded_String (Table (Kind).all);
      Position   : Natural          := 1; --  First_Index (Message);
   begin
      --  Fill in placeholders
      for I in Arguments'Range loop
         declare
            Placeholder : constant String := "%" & Fixed.Trim (Positive'Image (I), Left);
         begin
            Position := Index (Message, Placeholder, Position);
            if Position = 0 then
               raise Program_Error with "No placeholder '" & Placeholder & "'";
            end if;
            Replace_Slice (Message, Position,
                           Position + Placeholder'Length - 1,
                           To_String (Arguments (I)));
         end;
      end loop;

      DK8543.Errors.Error (File        => Ada.Text_IO.Standard_Output,
                           File_Name   => File_Name,
                           Line_Number => Line_Number,
                           Message     => Kind_Image & To_String (Message));
   end Error;


--     procedure Error (Kind        : in K_Error_Parse_One_Token;
--                      Line_Number : in Natural)
--     is
--     begin
--        Error_2 (Kind, "", "", Line_Number);
--     end Error;


--     procedure Error_1 (Kind        : in K_Error_Parse_One_Token;
--                        Argument    : in String;
--                        Line_Number : in Natural)
--     is
--     begin
--        Error_2 (Kind, Argument, "", Line_Number);
--     end Error_1;


--     procedure Error_2 (Kind        : in K_Error_Parse_One_Token;
--                        Argument_1  : in String;
--                        Argument_2  : in String;
--                        Line_Number : in Natural)
--     is
--     begin
--        Error (To_String (Default_File_Name), Kind, Argument_1, Argument_2, Line_Number);
--     end Error_2;


end Errors;
