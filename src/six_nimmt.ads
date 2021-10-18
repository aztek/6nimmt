with Ada.Containers; use Ada.Containers;
with Ada.Containers.Ordered_Maps;
with Ada.Containers.Ordered_Sets;
with Ada.Containers.Vectors;

package Six_Nimmt is

   --  The game consists of 104 cards, numbered from 1 to 104.

   type Card is range 1 .. 104;

   package Card_Sets is new Ada.Containers.Ordered_Sets (Element_Type => Card);

   package Card_Vectors is new Ada.Containers.Vectors
     (Index_Type => Natural, Element_Type => Card);

   subtype Deck is Card_Vectors.Vector with
        Dynamic_Predicate => True; -- Card_Vectors.Length (Deck) <= Card'Last;

   function Shuffled_Deck (Max_Card : Card) return Deck;

   --  Each card bears on it one to seven bull's heads symbols that represent
   --  penalty points.

   subtype Heads is Natural with
        Static_Predicate => Heads in 1 | 2 | 3 | 5 | 7;

      --!pp off

   --  * Card 55 bear 7 heads.

   --  * Cards 11, 22, 33, 44, 66, 77, 88, 99 bear 5 cards.

   --  * Cards 10, 20, 30, 40, 50, 60, 70, 80, 90, 100 bear 3 heads.

   --  * Cards 5, 15, 25, 35, 45, 65, 75, 85, 95 bear 2 heads.

   --  * All other cards bear 1 head.

   --!pp on

   function Nr_Heads (C : Card) return Heads;

   function Card_Diff (C_1 : Card; C_2 : Card) return Integer;

   function ">" (C_1 : Card; C_2 : Card) return Boolean;

   --  In the beginning of the game, each player is dealt ten cards.

   Init_Hand_Size : constant := 10;

   --  During the game players take cards one by one from their hand and place
   --  them on the table.

   subtype Hand is Card_Sets.Set with
        Dynamic_Predicate => Card_Sets.Length (Hand) <= Init_Hand_Size;

   Empty_Hand : Hand := Card_Sets.Empty_Set;

   function Deal (D : in out Deck) return Hand with
      Post => True; -- Size (Deal'Result) = Init_Hand_Size;

   subtype Bank is Card_Sets.Set;

   Empty_Bank : Bank := Card_Sets.Empty_Set;

   function Bank_Value (B : Bank) return Natural;

   type Player_Type is (Human, AI);

   type Player is record
      Intelligence : Player_Type;
      Player_Hand  : Hand := Empty_Hand;
      Player_Bank  : Bank := Empty_Bank;
   end record;

   --  The game is for two to ten players.

   --  type Nr_Players is new Integer range 2 .. 10;

   Nr_Players : constant := 3;

   type Player_Position is range 1 .. Nr_Players;

   type Players is array (Player_Position) of Player;

   package Card_Maps is new Ada.Containers.Ordered_Maps
     (Key_Type => Card, Element_Type => Player_Position);

   Nr_Rounds : constant := 10;

   subtype Round is Card_Maps.Map;

   --  Each row consists of at most six cards.

   Max_Row_Width : constant := 5;

   subtype Row is Card_Sets.Set;
   --  Dynamic_Predicate => 1 <= Card_Sets.Length (Row) and Card_Sets.Length
   --  (Row) <= Max_Row_Width;

   --  There are four rows of cards on the table.

   Nr_Rows : constant := 4;

   type Row_Index is range 1 .. Nr_Rows;

   type Table is array (Row_Index) of Row;

   procedure Setup_Game (D : in out Deck; T : out Table; Ps : out Players);

   function Setup_Table (D : in out Deck) return Table;

   --  Check whether the given card is the lowest one among the top cards of
   --  each row on the table.

   function Lowest_Card (C : Card; T : Table) return Boolean with
      Pre  => True,
      Post => (Lowest_Card'Result = (for all R of T => R.Last_Element > C));

end Six_Nimmt;
