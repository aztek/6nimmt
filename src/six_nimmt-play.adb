with Ada.Numerics.Discrete_Random;

with Six_Nimmt.CLI;

package body Six_Nimmt.Play is

   function Pick_Card (P : Player; T : Table) return Card is
      --!pp off
      (case P.Intelligence is
       when Human => CLI.Prompt_Card (P.Player_Hand),
       when AI    => Pick_Random_Card (P.Player_Hand));
      --!pp on

   function Pick_Row (P : Player; T : Table) return Row_Index is
      --!pp off
      (case P.Intelligence is
       when Human => CLI.Prompt_Row_Index,
       when AI    => Pick_Cheapest_Row (T));
      --!pp on

   function Pick_Random_Card (H : Hand) return Card is
      package Discrete_Random is new Ada.Numerics.Discrete_Random
        (Result_Subtype => Count_Type);
      use Discrete_Random;

      Gen : Generator;

      Count        : Count_Type := Card_Sets.Length (H);
      Random_Index : Count_Type;

      type Card_Array_Type is array (Count_Type range <>) of Card;
      Card_Array  : Card_Array_Type (0 .. Count);
      Array_Index : Count_Type := Card_Array'First;
   begin
      for C of H loop
         Card_Array (Array_Index) := C;
         Array_Index              := Array_Index + 1;
      end loop;

      Reset (Gen);
      Random_Index := Random (Gen) mod Count;

      return Card_Array (Random_Index);
   end Pick_Random_Card;

   function Pick_Cheapest_Row (T : Table) return Row_Index is
      Row_Cost : Natural;
      Min_Cost : Natural   := 999;
      R        : Row_Index := Row_Index'First;
   begin
      for I in T'Range loop
         Row_Cost := Bank_Value (Bank (T (I)));
         if Row_Cost < Min_Cost then
            Min_Cost := Row_Cost;
            R        := I;
         end if;
      end loop;
      return R;
   end Pick_Cheapest_Row;

   function Select_Row (T : Table; C : Card) return Row_Index is
      J        : Row_Index := Row_Index'First;
      Row_Diff : Integer;
      Diff     : Integer   := 104;
   begin
      --  Find the row whose top card is the closest to C
      for I in T'Range loop
         Row_Diff := Card_Diff (C, T (I).Last_Element);
         if 0 < Row_Diff and Row_Diff < Diff then
            J    := I;
            Diff := Row_Diff;
         end if;
      end loop;
      return J;
   end Select_Row;

   procedure Play_Card (T : in out Table; C : in Card) is
      J : Row_Index;
   begin
      J := Select_Row (T, C);
      T (J).Insert (C);
   end Play_Card;

   procedure Collect_Row
     (T : in out Table; B : in out Bank; I : Row_Index; C : Card)
   is
   begin
      for K of T (I) loop
         B.Include (K);
      end loop;
      T (I) := Card_Sets.To_Set (C);
   end Collect_Row;

   procedure Play_Turn (T : in out Table; P : in out Player; C : Card) is
      R : Row_Index;
   begin
      if Lowest_Card (C, T) then
         R := Pick_Row (P, T);
         Collect_Row (T, P.Player_Bank, R, C);
      else
         R := Select_Row (T, C);
         if Card_Sets.Length (T (R)) = Max_Row_Width then
            Collect_Row (T, P.Player_Bank, R, C);
         else
            T (R).Insert (C);
         end if;
      end if;
      Card_Sets.Delete (P.Player_Hand, C);
   end Play_Turn;

   function Play_Round (T : in out Table; Ps : in out Players) return Round is
      R : Round;
      C : Card;
      P : Player_Position;
   begin
      for P in Ps'Range loop
         C := Pick_Card (Ps (P), T);
         Card_Maps.Include (R, C, P);
      end loop;

      for Cursor in Card_Maps.Iterate (R) loop
         C := Card_Maps.Key (Cursor);
         P := Card_Maps.Element (Cursor);
         Play_Turn (T, Ps (P), C);
      end loop;

      return R;
   end Play_Round;

end Six_Nimmt.Play;
