with Ada.Containers.Ordered_Sets;

package Six_Nimmt.Play is

   function Pick_Card (P : Player; T : Table) return Card; -- with
   -- Post => Card_Sets.Contains (P.Player_Hand, Pick_Card'Result);

   function Pick_Row (P : Player; T : Table) return Row_Index;

   function Select_Row (T : Table; C : Card) return Row_Index;

   --  Starting with the lowest valued card, and continuing in increasing
   --  order, each player must put their card at the end of one of the four
   --  rows on the table, following these rules:

   --  * The card must be put on a row where the latest (end) card is lower
   --    in value than the card that is about to be played.

   --  * The card must be put on the row where the latest (end) card is
   --    the closest in value to the card that is about to be played
   --    (e.g. if your card is 33, then place it next to the 30 not the 29)

   --  * If the row where the played card must be placed already contains 5
   --    cards (the player's card is the 6th), the player must gather the 5
   --    cards on the table, leaving only the 6th card in their place to start
   --    a new row. The gathered cards must be taken separated and never mixed
   --    with the hand cards. The sum of the number of cattle head on the
   --    gathered cards will be calculated at the end of the round.

   --  * If the played card is lower than all the latest cards present on the
   --    four rows, the player must choose a row and gather the cards on that
   --    row (usually the row with the fewest cattle heads), leaving only the
   --    played card on the row.

   --  TODO: Add to post-conditions:

   --  * all inserted cards are intact

   --  * C is inserted

   --  * nothing else is inserted

   --  * latest card is lower that C (GUARANTEED BY DATA INV)

   --  * latest card is closest in value to C than all others

   procedure Play_Card (T : in out Table; C : in Card) with
      Pre  => not Lowest_Card (C, T),
      Post => True;

   procedure Collect_Row

     (T : in out Table; B : in out Bank; I : Row_Index; C : Card) with
      Pre  => Lowest_Card (C, T),
      Post => True;

   procedure Play_Turn (T : in out Table; P : in out Player; C : Card);

   function Play_Round (T : in out Table; Ps : in out Players) return Round;

private

   function Pick_Random_Card (H : Hand) return Card; -- with
   -- Post => Card_Sets.Contains (H, Pick_Random_Card'Result);

   function Pick_Cheapest_Row (T : Table) return Row_Index;

end Six_Nimmt.Play;
