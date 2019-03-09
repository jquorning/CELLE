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


   type String_Access is access all String;

   function "-" (Item : String) return String_Access;
   function "-" (Item : String) return String_Access is
   begin
      return new String'(Item);
   end "-";

   Table : constant array (K_Error_Parse) of String_Access :=
     (E001 => -("There is no prior rule upon which to attach the code fragment which " &
                  "begins on this line."),
      E002 => -("Code fragment beginning on this line is not the first to follow the " &
                  "previous rule."),
      E003 => -"Token '$1' should be either '%%' or a nonterminal name.",
      E004 => -"The precedence symbol must be a terminal.",
      E005 => -"There is no prior rule to assign precedence '[$1]'.",
      E006 => -"Precedence mark on this line is not the first to follow the previous rule.",
      E007 => -"Missing ']' on precedence mark.",
      E008 => -"Expected to see a ':' following the LHS symbol '$1'.",
      E009 => -"'$1' is not a valid alias for the LHS '$2'",
      E010 => -"Missing ')' following LHS alias name '$1'.",
      E011 => -"Missing '->' following: '$1($2)'.",
      E012 => -"'$1' is not a valid alias for the RHS symbol '$2'",
      E013 => -"Missing ')' following LHS alias name '$1'.",

      E101 => -"Can't open this file for reading.",
      E102 => -"C code starting on this line is not terminated before the end of the file.",

      E201 => -"Cannot form a compound containing a non-terminal",
      E202 => -"Illegal character on RHS of rule: '$1'.",
      E203 => -"Unknown declaration keyword: '%$1'.",
      E204 => -"Illegal declaration keyword: '$1'.",
      E205 => -"Symbol name missing after %destructor keyword",
      E206 => -"Symbol name missing after %type keyword",
      E207 => -"Symbol %type '$1' already defined",
      E208 => -"%token_class argument '%1' should be a token",
      E209 => -"%token_class must be followed by an identifier: '$1'",
      E210 => -"Symbol '$1' already used",
      E211 => -"%wildcard argument '$1' should be a token",
      E212 => -"Extra wildcard to token: '$1'",
      E213 => -"Illegal argument to %$1: $2"
     );


   procedure Parser_Error
     (Kind        : in K_Error_Parse;
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
            Placeholder : constant String := "$" & Fixed.Trim (Positive'Image (I), Left);
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
      Error_Count := Error_Count + 1;
   end Parser_Error;


   procedure Set_File_Name
     (File_Name : in Ada.Strings.Unbounded.Unbounded_String)
   is
   begin
      Errors.Default_File_Name := File_Name;
   end Set_File_Name;


--     procedure Error (Kind        : in K_Error_Parse_One_Token;
--                      Arguments   : in Argument_List;
--                      Line_Number : in Natural             := Start_Line)
--     is
--     begin
--        Errors.Error (Kind, Line_Number, Arguments);
--        Error_Count := Error_Count + 1;
--     end Error;


--     procedure Error (Kind        : in K_Error_Parse_One_Token;
--                      Line_Number : in Natural                 := Start_Line)
--     is
--     begin
--        Error (Kind, Null_Argument_List, Line_Number);
--     end Error;


end Errors;
