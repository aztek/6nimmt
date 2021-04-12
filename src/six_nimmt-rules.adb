with Ada.Containers; use Ada.Containers;
with Ada.Containers.Ordered_Sets;
with Ada.Containers.Vectors;
with Ada.Numerics.Discrete_Random;
with Ada.Text_IO; use Ada.Text_IO;

procedure SixNimmt is
   -- The game consists of 104 cards, numbered from 1 to 104.
   type Card is new Integer range 1..104;

   package Card_Sets is new Ada.Containers.Ordered_Sets (Element_Type => Card);
   use Card_Sets;

   package Card_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Natural,
      Element_Type => Card);
   use Card_Vectors;

   subtype Deck is Vector
      with Dynamic_Predicate => Deck'Size <= Card'Last;

   procedure Full_Shuffled_Deck (D : out Deck)
      --  with Post => Size (Deck) = Card'Last
   is
      package Discrete_Random is new Ada.Numerics.Discrete_Random
         (Result_Subtype => Card);
      use Discrete_Random;

      G : Generator;
      K, T : Card;

      type Card_Array is array (Card) of Card;
      Deck_A : Card_Array;
   begin
      for C in Card'Range loop
         Deck_A (C) := (C);
      end loop;

      Reset (G);
      for I in reverse Deck_A'Range loop
         K := (Random (G) mod I) + 1;
         T := Deck_A (I);
         Deck_A (I) := Deck_A (K);
         Deck_A (K) := T;
      end loop;

      D := Empty_Vector;
      for C of Deck_A loop
         D := D & C;
      end loop;

   end Full_Shuffled_Deck;

   -- Each card bears on it one to seven bull's heads symbols
   -- that represent penalty points.
   subtype Heads is Integer
      with Static_Predicate => Heads in 1 | 2 | 3 | 5 | 7;

   -- Card 55 bear 7 heads.
   -- Cards 11, 22, 33, 44, 66, 77, 88, 99 bear 5 cards.
   -- Cards 10, 20, 30, 40, 50, 60, 70, 80, 90, 100 bear 3 heads.
   -- Cards 5, 15, 25, 35, 45, 65, 75, 85, 95 bear 2 heads.
   -- All other cards bear 1 head.
   function Nr_Heads (C : Card) return Heads is begin
      case C is
         when 55 => return 7;
         when 11 | 22 | 33 | 44 | 66 | 77 | 88 | 99 => return 5;
         when 10 | 20 | 30 | 40 | 50 | 60 | 70 | 80 | 90 | 100 => return 3;
         when  5 | 15 | 25 | 35 | 45 | 65 | 75 | 85 | 95 => return 2;
         when others => return 1;
      end case;
   end Nr_Heads;

   function Show_Card (C : Card) return String is
   begin
      return Card'Image (C) & " [" & (Heads'Image (Nr_Heads (C))) & " ]";
   end Show_Card;

   function Card_Diff (C_1 : Card; C_2 : Card) return Integer is begin
      return Integer (C_1) - Integer (C_2);
   end Card_Diff;

   -- The game is for two to ten players.
   type Players is new Integer range 2..10;

   -- In the beginning of the game, each player is dealt ten cards.
   Init_Hand_Size : constant := 10;

   -- During the game players take cards one by one from their hand and
   -- place them on the table.
   subtype Hand is Set
      with Dynamic_Predicate => Hand'Size <= Init_Hand_Size;

   procedure Print_Hand (H : Hand) is
   begin
      Put ("✋ ");
      for C of H loop
         Put (Show_Card (C) & " ");
      end loop;
      Put_Line ("");
   end Print_Hand;

   procedure Deal (D : in out Deck; H : out Hand)
      with
         Pre => Is_Empty (H)
   is
   begin
      for I in 1..Init_Hand_Size loop
         Insert (H, D.First_Element);
         Delete_First (D, 1);
      end loop;
   end Deal;

   -- 
   subtype Bank is Set;

   -- type Player is record
   --    Player_Hand : Hand := Empty_Set;
   --    Player_Bank : Bank := Empty_Set;
   -- end record;

   -- Each row consists of at most six cards.
   subtype Row is Set
      with Dynamic_Predicate => 1 <= Row'Size and Row'Size <= 6;
           -- and also ordered

   procedure Print_Row (R : Row) is
   begin
      for C of R loop
         Put_Line (Show_Card (C) & " ");
      end loop;
   end Print_Row;

   -- There are four rows of cards on the table.
   type Row_Index is range 1..4;
   type Table is array (Row_Index) of Row;

   procedure Print_Table (T : Table) is
   begin
      for I in T'Range loop
         Put (Row_Index'Image (I) & ":");
         Print_Row (T (I));
      end loop;
   end Print_Table;

   -- Check whether a given card is the lowest in the row.
   function Lowest_Card (C : Card; R : Row) return Boolean
      with Pre => True, -- card not in the row?
           Post => (if Lowest_Card'Result then
                      (for all K of R => Card_Diff (K, C) > 0)
                   else
                      (for some K of R => Card_Diff (C, K) > 0))
   is
   begin
      for K of R loop
         if Card_Diff (C, K) > 0 then
            return False;
         end if;
      end loop;
      return True;
   end Lowest_Card;

   -- Check whether the given card is the lowest on the table.
   function Lowest_Card (C : Card; T : Table) return Boolean
      with Pre => True,
           Post => (if Lowest_Card'Result then
                       (for all R of T => Lowest_Card (C, R))
                    else
                       (for some R of T => Lowest_Card (C, R)))
   is
   begin
      for R of T loop
         if not Lowest_Card (C, R) then
            return False;
         end if;
      end loop;
      return True;
   end Lowest_Card;

   -- Starting with the lowest valued card, and continuing in increasing order,
   -- each player must put their card at the end of one of the four rows on
   -- the table, following these rules:
   --
   -- * The card must be put on a row where the latest (end) card is lower
   --   in value than the card that is about to be played.
   --
   -- * The card must be put on the row where the latest (end) card is
   --   the closest in value to the card that is about to be played
   --   (e.g. if your card is 33, then place it next to the 30 not the 29)
   --
   -- * If the row where the played card must be placed already contains 5 cards
   --   (the player's card is the 6th), the player must gather the 5 cards on
   --   the table, leaving only the 6th card in their place to start a new row.
   --   The gathered cards must be taken separated and never mixed with the hand
   --   cards. The sum of the number of cattle head on the gathered cards will
   --   be calculated at the end of the round.
   --
   -- * If the played card is lower than all the latest cards present on the
   --   four rows, the player must choose a row and gather the cards on that row
   --   (usually the row with the fewest cattle heads), leaving only the played
   --   card on the row.
   procedure Play (T : in out Table; C : in Card)
   with
      Pre => not Lowest_Card (C, T),
      Post => true -- all inserted cards are intact
                   -- C is inserted
                   -- nothing else is inserted
                   -- latest card is lower that C (GUARANTEED BY DATA INV)
                   -- latest card is closest in value to C than all others
   is
      R : Row;
      J : Row_Index := Row_Index'First;
      Row_Diff : Integer;
      Diff : Integer := 104;
      Extended_Row : Row;
   begin
      -- Find the row whose top card is the closest to C
      for I in Row_Index loop
         R := T (I);
         Row_Diff := Card_Diff (C, R.Last_Element);
         if 0 < Row_Diff and Row_Diff < Diff then
            J := I;
            Diff := Row_Diff;
         end if;
      end loop;

      -- Place the card at the end of that row
      Extended_Row := T (J);
      Extended_Row.Insert (C);
      T (J) := Extended_Row;
   end Play;

   -- 
   procedure Place_Lowest (T : in out Table; B : in out Bank;
                           I : Row_Index; C : Card) is
   begin
      for K of T (I) loop
         B.Include (K);
      end loop;
      T (I) := To_Set (C);
   end Place_Lowest;

   Full_Deck : Deck;
   T : Table;

   Player_Hand : Hand := Empty_Set;

begin

   Full_Shuffled_Deck (Full_Deck);

   for I in Row_Index loop
      T (I) := To_Set (Full_Deck.First_Element);
      Delete_First (Full_Deck, 1);
   end loop;

   Print_Table (T);

   Deal (Full_Deck, Player_Hand);

   Print_Hand (Player_Hand);

   -- Put_Line (Integer'Image (Nr_Heads (Card (35))));

end SixNimmt;

--   function NrHeads (C : Card) return Heads is
--      H : out Integer := 1;
--   begin
--      if C mod 5 = 0 then
--         H := H + 1;
--         if C mod 10 = 0 then
--            H := H + 1;
--         end if;
--      end if;
--
--      if C mod 11 = 0 then
--         H := H + 4;
--         if C mod 5 = 0 then
--            H := H + 1;
--         end if;
--      end if;
--   end NrHeads;
