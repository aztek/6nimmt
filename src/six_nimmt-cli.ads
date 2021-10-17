package Six_Nimmt.CLI is

   procedure Redraw_Screen (T : Table; Ps : Players; R : Round);

   procedure Game_Over (T : Table; Ps : Players; R : Round);

   function Prompt_Card (H : Hand) return Card with
      Post => Card_Sets.Contains (H, Prompt_Card'Result);

   function Prompt_Row_Index return Row_Index;

end Six_Nimmt.CLI;
