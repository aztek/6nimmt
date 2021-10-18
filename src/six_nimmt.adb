with Ada.Numerics.Discrete_Random;

with Ada.Text_IO; use Ada.Text_IO;

package body Six_Nimmt is

   function Shuffled_Deck (Max_Card : Card) return Deck is
      package Discrete_Random is new Ada.Numerics.Discrete_Random
        (Result_Subtype => Card);
      use Discrete_Random;

      D    : Deck;
      Gen  : Generator;
      K, T : Card;

      type Card_Array is array (1 .. Max_Card) of Card;
      Deck_A : Card_Array;
   begin
      for C in 1 .. Max_Card loop
         Deck_A (C) := Card (C);
      end loop;

      Reset (Gen);
      for I in reverse Deck_A'Range loop
         K          := (Random (Gen) mod I) + 1;
         T          := Deck_A (I);
         Deck_A (I) := Deck_A (K);
         Deck_A (K) := T;
      end loop;

      for C of Deck_A loop
         Card_Vectors.Append (D, C);
      end loop;

      return D;
   end Shuffled_Deck;

   function Nr_Heads (C : Card) return Heads is
     --!pp off
      (case C is
       when 55 => 7,
       when 11 | 22 | 33 | 44 | 66 | 77 | 88 | 99 => 5,
       when 10 | 20 | 30 | 40 | 50 | 60 | 70 | 80 | 90 | 100 => 3,
       when  5 | 15 | 25 | 35 | 45 | 65 | 75 | 85 | 95 => 2,
       when others => 1);
     --!pp on

   function Card_Diff (C_1 : Card; C_2 : Card) return Integer is
     (Integer (C_1) - Integer (C_2));

   function ">" (C_1 : Card; C_2 : Card) return Boolean is
     (Card_Diff (C_1, C_2) > 0);

   function Bank_Value (B : Bank) return Natural is
      Value : Natural := 0;
   begin
      for C of B loop
         Value := Value + Nr_Heads (C);
      end loop;
      return Value;
   end Bank_Value;

   function Deal (D : in out Deck) return Hand is
      H : Hand;
   begin
      for I in 1 .. Init_Hand_Size loop
         Card_Sets.Insert (H, D.First_Element);
         Card_Vectors.Delete_First (D, 1);
      end loop;
      return H;
   end Deal;

   procedure Setup_Game (D : in out Deck; T : out Table; Ps : out Players) is
   begin
      for P of Ps loop
         P.Player_Hand := Deal (D);
      end loop;
      T := Setup_Table (D);
   end Setup_Game;

   function Setup_Table (D : in out Deck) return Table is
      T : Table;
   begin
      for R of T loop
         R := Card_Sets.To_Set (D.First_Element);
         Card_Vectors.Delete_First (D, 1);
      end loop;
      return T;
   end Setup_Table;

   function Lowest_Card (C : Card; T : Table) return Boolean is
   begin
      for R of T loop
         if C > R.Last_Element then
            return False;
         end if;
      end loop;
      return True;
   end Lowest_Card;

   function Decide_Winner (Ps : Players) return Player is
      Winner       : Player  := Ps (Ps'First);
      Score        : Natural;
      Lowest_Score : Natural := Bank_Value (Winner.Player_Bank);
   begin
      for P of Ps loop
         Score := Bank_Value (P.Player_Bank);
         if Score < Lowest_Score then
            Lowest_Score := Score;
            Winner       := P;
         end if;
      end loop;
      return Winner;
   end Decide_Winner;

end Six_Nimmt;
