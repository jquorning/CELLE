--
--
--

with Rules;
with Symbols;

package body Builds is


   procedure Find_Rule_Precedences (Lemon : in out Lime.Lemon_Record)
   is
      use Rules;
      use Symbols;

      RP : Rule_Access;
   begin
      RP := Lemon.Rule;
      loop
         exit when RP = null;
         if RP.Prec_Sym = null then
            for I in RP.RHS.all'Range loop
               exit when RP.Prec_Sym /= null;
               declare
                  SP : constant Symbol_Access := RP.RHS (I);
               begin
                  if SP.Kind = Multi_Terminal then

                     for J in SP.Sub_Sym.First_Index .. SP.Sub_Sym.Last_Index loop
                        if SP.Sub_Sym (J).Prec >= 0 then
                           RP.Prec_Sym := SP.Sub_Sym (J);
                           exit;
                        end if;
                     end loop;

                  elsif SP.Prec >= 0 then
                     RP.Prec_Sym := RP.RHS (I);
                  end if;
               end;
            end loop;
         end if;
         RP := RP.Next;
      end loop;
   end Find_Rule_Precedences;


end Builds;
