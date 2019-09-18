--
--  The author disclaims copyright to this source code.  In place of
--  a legal notice, here is a blessing:
--
--    May you do good and not evil.
--    May you find forgiveness for yourself and forgive others.
--    May you share freely, not taking more than you give.
--

with Ada.Text_IO;

with DK8543.Errors;

package body Errors is

   type String_Access is access all String;

   function "-" (Item : in String) return String_Access;
   function "-" (Item : in String) return String_Access is
   begin
      return new String'(Item);
   end "-";

   Table : constant array (Error_Id) of String_Access :=
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
      E014 => -("The specified start symbol '$1' Start is not in a nonterminal " &
                  "of the grammar.  '$2' will be used as the start symbol instead."),
      E015 => -("The start symbol '$1' occurs on the right-hand " &
                  "side of a rule. This will result in a parser which " &
                  "does not work properly."),
      E016 => -"Can not open '$1' for reading.",

      E101 => -"Can't open this file for reading.",
      E102 => -"C code starting on this line is not terminated before the end of the file.",
      E103 => -"String starting on this line is not terminated before the end of the file.",

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
      E213 => -"Illegal argument to %$1: $2",
      E214 => -"%token argument $1 should be a token",
      E215 => -"%fallback argument $1 should be a token",
      E216 => -"More than one fallback assigned to token $1",
      E217 => -"Symbol $1 has already be given a precedence.",
      E218 => -"Can't assign a precedence to $1.",

      E301 => -"This rule can not be reduced."
     );


   procedure Parser_Error
     (Id          : in Error_Id;
      Line_Number : in Natural;
      Argument_1  : in String := "";
      Argument_2  : in String := "")
   is
      use Ada.Strings;

      File_Name  : constant String  := To_String (Default_File_Name);
      Kind_Image : constant String  := Id'Image & " ";
      Message    : Unbounded_String := To_Unbounded_String (Table (Id).all);
      Position   : Natural          := 1;
   begin

      --  Substitued $1 placeholder
      Position := Index (Message, "$1", Position);
      if Position /= 0 then
            Replace_Slice (Message, Position,
                           Position + 2 - 1,
                           Argument_1);
      end if;

      --  Substitute $2 placeholder
      Position := Index (Message, "$2", Position);
      if Position /= 0 then
            Replace_Slice (Message, Position,
                           Position + 2 - 1,
                           Argument_2);
      end if;

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


end Errors;
