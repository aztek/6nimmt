with Six_Nimmt.Play; use Six_Nimmt.Play;
with Six_Nimmt.CLI;  use Six_Nimmt.CLI;

procedure Six_Nimmt.Main is
   D : Deck;
   T : Table;
   --!pp off
   Ps : Players := (
      (Intelligence => Human, others => <>),
      (Intelligence => AI,    others => <>),
      (Intelligence => AI,    others => <>)
   );
   --!pp on
   Winner         : Player;
   Previous_Round : Round;
begin
   D := Shuffled_Deck (Nr_Rounds * Ps'Length + Nr_Rows);
   Setup_Game (D, T, Ps);
   for Round in 1 .. Nr_Rounds loop
      Redraw_Screen (T, Ps, Previous_Round);
      Previous_Round := Play_Round (T, Ps);
   end loop;
   Game_Over (T, Ps, Previous_Round);
   Winner := Decide_Winner (Ps);
   Announce_Winner (Winner);
end Six_Nimmt.Main;
