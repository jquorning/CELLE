with Symbols;
with States;

package Delme is

   function Sorted_At (Extra : in Symbols.Extra_Access;
                       Index : in Symbols.Symbol_Index)
                      return States.State_Access;

end Delme;
