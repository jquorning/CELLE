--
--  The author disclaims copyright to this source code.  In place of
--  a legal notice, here is a blessing:
--
--    May you do good and not evil.
--    May you find forgiveness for yourself and forgive others.
--    May you share freely, not taking more than you give.
--

with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;
with Ada.Characters.Latin_1;
with Ada.Text_IO;

package body Macros is

   function Is_Space (C : Character) return Boolean;
   --  True when C is a white space

   use Ada.Strings.Unbounded;
   package Macro_Vectors is
      new Ada.Containers.Vectors
     (Positive,
      Unbounded_String);

   Macros : Macro_Vectors.Vector;

   procedure Append (Name : in String)
   is
   begin
      Macros.Append (To_Unbounded_String (Name));
   end Append;


   procedure Preprocess (Buffer  : in out String;
                         Success :    out Boolean)
   is
      use Ada.Characters;

--      Preproc_Ifdef   : constant String := "%ifdef";
--      Preproc_Ifndef  : constant String := "%ifndef";
--      Preproc_Endif   : constant String := "%endif";

      I, J, N      : Integer;
      Exclude      : Integer := 0;
      Start        : Integer := 0;
      Lineno       : Integer := 1;
      Start_Lineno : Integer := 1;
   begin

      I := Buffer'First;
      loop
         exit when Buffer (I) = Latin_1.NUL;

         if Buffer (I) = Latin_1.LF then
            Lineno := Lineno + 1;
         end if;

         if Buffer (I) /= '%' or (I > Buffer'First and then Buffer (I - 1) /= Latin_1.LF) then
            goto Continue;
         end if;

         if Buffer (I .. I + 5) = "%endif" and Is_Space (Buffer (I + 6)) then

            if Exclude /= 0 then
               Exclude := Exclude - 1;
               if Exclude = 0 then
                  for J in Start .. I - 1 loop
                     if Buffer (J) /= Latin_1.LF then
                        Buffer (J) := ' ';
                     end if;
                  end loop;
               end if;
            end if;

            J := I;
            loop
               exit when Buffer (J) = Latin_1.NUL;
               exit when Buffer (J) = Latin_1.LF;
               Buffer (J) := ' ';
               J := J + 1;
            end loop;

         elsif
           (Buffer (I .. I + 5) = "%ifdef" and Is_Space (Buffer (I + 6)))
           or
           (Buffer (I .. I + 6) = "%ifndef" and Is_Space (Buffer (I + 7)))
         then

            if Exclude /= 0 then
               Exclude := Exclude + 1;
            else

               J := I + 7;
               loop
                  exit when not Is_Space (Buffer (J));
                  J := J + 1;
               end loop;

               --  Find lenght of macro name
               N := 0;
               loop
                  exit when Buffer (J + N) = Latin_1.NUL;
                  exit when Is_Space (Buffer (J + N));
                  N := N + 1;
               end loop;

               --  Find macro name in list of appended macro names
               Exclude := 1;
               for K of Macros loop
                  if Buffer (J .. J + N) = K then
                     Exclude := 0;
                     exit;
                  end if;
               end loop;

               if Buffer (I + 3) = 'n' then
                  if Exclude /= 0 then
                     Exclude := 0;
                  else
                     Exclude := 1;
                  end if;
               end if;

               if Exclude /= 0 then
                  Start        := I;
                  Start_Lineno := Lineno;
               end if;

            end if;

            J := I;
            loop
               exit when Buffer (J) = Latin_1.NUL;
               exit when Buffer (J) = Latin_1.LF;
               Buffer (J) := ' ';
               J := J + 1;
            end loop;
         end if;

         <<Continue>>
         I := I + 1;
      end loop;

      if Exclude /= 0 then
         Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error,
                               "unterminated %%ifdef starting on line " & Start_Lineno'Img);
         Success := False;
         return;
      end if;

      Success := True;

   end Preprocess;


   function Is_Space (C : Character) return Boolean is
   begin
      return
        C = Ada.Characters.Latin_1.Space or
        C = Ada.Characters.Latin_1.HT    or
        C = Ada.Characters.Latin_1.LF;
   end Is_Space;


end Macros;
