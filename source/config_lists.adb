--
--  The author disclaims copyright to this source code.  In place of
--  a legal notice, here is a blessing:
--
--    May you do good and not evil.
--    May you find forgiveness for yourself and forgive others.
--    May you share freely, not taking more than you give.
--

with Ada.Containers.Doubly_Linked_Lists;
with Ada.Unchecked_Deallocation;

with Debugs;
with Config_Tables;
with Prop_Links;
with Symbol_Sets;
with Errors;
with Symbols;

package body Config_Lists is

   function Config_New return Configs.Config_Access;
   --  Return a pointer to a new configuration.

   procedure Config_Delete (Config : in out Configs.Config_Access);
   --  The configuration "old" is no longer Used.

   use type Configs.Config_Access;

   package Configuration_Lists is
      new Ada.Containers.Doubly_Linked_Lists (Element_Type => Configs.Config_Access);

   Config_List : Configuration_Lists.List := Configuration_Lists.Empty_List;
   Basis_List  : Configuration_Lists.List := Configuration_Lists.Empty_List;


   procedure Init is
   begin
      Config_List.Clear;
      Basis_List.Clear;
      Config_Tables.Init;
   end Init;


   function Add
     (Rule : in Rules.Rule_Access;
      Dot  : in Rules.Dot_Type) return Configs.Config_Access
   is
      use Configs;

      Model  : aliased Config_Record;
      Config : Config_Access;
   begin
      Model.Rule := Rule;
      Model.Dot  := Dot;
      Config := Config_Tables.Find (Model'Unchecked_Access);
      if Config = null then
         Config := Config_New;
         Config.all :=
           Config_Record'(Rule        => Rule,
                          Dot         => Dot,
                          Follow_Set  => Symbol_Sets.Set_New,
                          State       => null,
                          Forward_PL  => Prop_Links.Propagation_Lists.Empty_List,
                          Backward_PL => Prop_Links.Propagation_Lists.Empty_List,
                          Status      => Incomplete, -- XXX Do not know
                          Next        => null,
                          Basis       => null);
         Config_List.Append (Config);
         Config_Tables.Insert (Config);
      end if;
      return Config;
   end Add;


   function Add_Basis
     (Rule : in Rules.Rule_Access;
      Dot  : in Rules.Dot_Type) return Configs.Config_Access
   is
      use Configs;

      Model  : aliased Config_Record;
      Config : Config_Access;
   begin
      Model.Rule := Rule;
      Model.Dot  := Dot;
      Config := Config_Tables.Find (Model'Unchecked_Access);
      if Config = null then
         Config := Config_New;
         Config.all :=
           Config_Record'(Rule        => Rule,
                          Dot         => Dot,
                          Follow_Set  => Symbol_Sets.Set_New,
                          State       => null,
                          Forward_PL  => Prop_Links.Propagation_Lists.Empty_List,
                          Backward_PL => Prop_Links.Propagation_Lists.Empty_List,
                          Status      => Incomplete, -- XXX Do not know
                          Next        => null,
                          Basis       => null);
         Config_List.Append (Config);
         Basis_List.Append (Config);
         Config_Tables.Insert (Config);
      end if;
      return Config;
   end Add_Basis;


   procedure Closure (Session : in Sessions.Session_Type)
   is
      use Configs;
      use Rules;
      use Symbols;
      use Symbol_Sets;
      use type Configuration_Lists.Cursor;

      New_Config : Config_Access;
      Rule       : Rule_Access;
      New_Rule   : Rule_Access;
      Symbol     : Symbol_Access;
      RHS_Symbol : Symbol_Access;
      Dot        : Dot_Type;
      Dummy      : Boolean;
      Last_RHS   : Boolean;
      Config_Pos : Configuration_Lists.Cursor;
   begin
      pragma Assert (not Config_List.Is_Empty);

      --  Use cursor for looping because there will be appended element in loop.

      Config_Pos := Config_List.First;
      while Config_Pos /= Configuration_Lists.No_Element loop
         declare
            Config : constant Config_Access := Configuration_Lists.Element (Config_Pos);
         begin
            Rule := Config.Rule;
            Dot  := Config.Dot;

            Debugs.Debug (True, "Dot: " & Dot'Image);
            if Dot < Rule.RHS.Last_Index then

               Symbol := Symbol_Access (Rule.RHS.Element (Dot));

               if Symbol.Kind = Non_Terminal then
                  if Symbol.Rule = null and Symbol /= Session.Error_Symbol then
                     Errors.Parser_Error (Errors.E401,
                                          Line_Number => Rule.Line,
                                          Argument_1  => Name_Of (Symbol));
                  end if;

                  New_Rule := Rule_Access (Symbol.Rule);
                  while New_Rule /= null loop
                     New_Config := Add (New_Rule, Dot => 0);
                     Last_RHS := False;

                     Debugs.Debug (True, "Rule.RHS.Length: " & Rule.RHS.Length'Image);
                     for I in Dot + 1 .. Rule.RHS.Last_Index loop
                        if I = Rule.RHS.Last_Index then
                           Last_RHS := True;
                        end if;

                        RHS_Symbol := Symbol_Access (Rule.RHS.Element (I));

                        case RHS_Symbol.Kind is

                           when Terminal =>
                              Dummy := Set_Add (New_Config.Follow_Set, RHS_Symbol.Index);
                              exit;

                           when Multi_Terminal =>
                              for K in
                                Integer range 0 .. Integer (RHS_Symbol.Sub_Symbol.Length) - 1
                              loop
                                 Dummy := Set_Add (New_Config.Follow_Set,
                                                   RHS_Symbol.Sub_Symbol.Element (K).Index);
                              end loop;
                              exit;

                           when others =>
                              Dummy := Set_Union (New_Config.Follow_Set, RHS_Symbol.First_Set);
                              exit when not RHS_Symbol.Lambda;

                        end case;
                     end loop;

                     if Last_RHS then
                        Config.Forward_PL.Append (Prop_Links.Config_Access (New_Config));
                     end if;
                     New_Rule := New_Rule.Next_LHS;
                  end loop;
               end if;

            end if;
         end;
         Configuration_Lists.Next (Config_Pos);
      end loop;
   end Closure;


   procedure Sort is
      package Config_Sorts is
         new Configuration_Lists.Generic_Sorting;
   begin
      Config_Sorts.Sort (Config_List);
   end Sort;


   procedure Sort_Basis is
      package Basis_Sorts is
         new Configuration_Lists.Generic_Sorting;
   begin
      Basis_Sorts.Sort (Basis_List);
   end Sort_Basis;


   function Xreturn return Configs.Config_Access
   is
      Old : constant Configs.Config_Access := Config_List.First_Element;
   begin
      Config_List.Clear;
      return Old;
   end Xreturn;


   function Basis return Configs.Config_Access
   is
      Old : constant Configs.Config_Access := Basis_List.First_Element;
   begin
      Basis_List.Clear;
      return Old;
   end Basis;


   procedure Eat (Config : in out Configs.Config_Access)
   is
      use Symbol_Sets;
      use Configs;

      Next_Config : Config_Access;
   begin
      while Config /= null loop
         Next_Config := Config.Next;
         pragma Assert (Config.Forward_PL.Is_Empty);
         pragma Assert (Config.Backward_PL.Is_Empty);
         if Config.Follow_Set /= Null_Set then
            Symbol_Sets.Set_Free (Config.Follow_Set);
         end if;
         Config_Delete (Config);
         Config := Next_Config;
      end loop;
   end Eat;


   procedure Reset
   is
   begin
      Config_List.Clear;
      Basis_List.Clear;
      Config_Tables.Clear;
   end Reset;


   function Config_New return Configs.Config_Access is
   begin
      return new Configs.Config_Record;
   end Config_New;


   procedure Config_Delete (Config : in out Configs.Config_Access)
   is
      use Configs;
      procedure Free is
         new Ada.Unchecked_Deallocation (Object => Config_Record,
                                         Name   => Config_Access);
   begin
      Free (Config);
      Config := null;
   end Config_Delete;


end Config_Lists;
