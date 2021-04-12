with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;
with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Text_IO; use Ada.Strings.Unbounded.Text_IO;
with Ada.Integer_Text_IO;

with Ada.Strings.Maps; use Ada.Strings.Maps;

with Ada.Containers.Vectors;

with Ada.Text_IO; use Ada.Text_IO;

package body Six_Nimmt.CLI is

   function Show_Card (C : Card) return Unbounded_String is
      Image : Unbounded_String;
   begin
      Append (Image, ESC & "[47m" & ESC & "[30;1m");
      Append (Image, Card'Image (C));
      Append (Image, Nr_Heads (C) * "🐮");
      Append (Image, " " & ESC & "[0m");
      return Image;
   end Show_Card;

   function Show_Card_Set (S : Card_Sets.Set) return Unbounded_String is
      Result : Unbounded_String;
   begin
      for C of S loop
         Append (Result, Show_Card (C) & " ");
      end loop;
      return Result;
   end Show_Card_Set;

   function Player_Avatar (P : Player) return String is
      (case P.Intelligence is
       when Human => "🙂",
       when AI    => "🤖");

   function Printable_Length (Str : Unbounded_String) return Natural is
      -- function Is_Printable (C : Character) return Boolean
      -- is ((C >= Character'Val (32) and C <= Character'Val (127)) or
      --     C = LC_Icelandic_Eth or C = LC_A_Circumflex);

      Len : Natural := 0;
      C : Character;
      Control_Seq : Boolean := False;
      Emoji_Seq : Boolean := False;
   begin
      for I in 1..Ada.Strings.Unbounded.Length (Str) loop
         C := Element (Str, I);

         if C = ESC then
            Control_Seq := True;
         end if;

         if not Control_Seq then
            if C >= Character'Val (32) and C <= Character'Val (127) then
               Len := Len + 1;
            elsif C = LC_A_Circumflex then
               Len := Len + 1;
            elsif C = LC_Icelandic_Eth then
               Len := Len + 2;
            end if;
         end if;

         if Control_Seq and C = 'm' then
            Control_Seq := False;
         end if;
      end loop;
      return Len;
   end Printable_Length;

   function Pad_To (Str : Unbounded_String; Width : Natural; Padding : Unbounded_String)
   return Unbounded_String
      with Post => Printable_Length (Pad_To'Result) >= Width
   is
      L : constant Natural := Printable_Length (Str);
      Padded : Unbounded_String := Str;
   begin
      if Width > L then
         Append (Padded, ((Width - L) * Padding));
         return Padded;
      else
         return Str;
      end if;
   end Pad_To;

   function Pad_To (Str : Unbounded_String; Width : Natural; Padding : String)
   return Unbounded_String
   is (Pad_To (Str, Width, To_Unbounded_String (Padding)));

   Box_TL : constant String := "┌"; -- "┌";
   Box_TR : constant String := "┐"; -- "┐";

   Box_BL : constant String := "└"; -- "└";
   Box_BR : constant String := "┘"; -- "┘";

   Box_VB : constant String := "│"; -- "│";
   Box_HB : constant String := "─"; -- "─";
   Box_BT : constant String := "┴"; -- "┴";

   Box_ML : constant String := "├"; -- "├";
   Box_MC : constant String := "┼"; -- "┼";
   Box_MR : constant String := "┤"; -- "┤";

   function Box_Header (Width : Natural; Header : Unbounded_String)
   return Unbounded_String
   is (Box_TL & Pad_To (Header, Width, Box_HB) & Box_TR);

   function Box_Header (Width : Natural; Header : String := "")
   return Unbounded_String
   is (Box_Header (Width, To_Unbounded_String (Header)));

   function Box_Row (Width : Natural; Row : Unbounded_String)
   return Unbounded_String
   is (Box_VB & Pad_To (" " & Row, Width, " ") & Box_VB);

   function Box_Row (Width : Natural; Row : String)
   return Unbounded_String
   is (Box_Row (Width, To_Unbounded_String (Row)));

   function Box_Middle (Width : Natural; Middle : Unbounded_String)
   return Unbounded_String
   is (Box_ML & Pad_To (Middle, Width, Box_HB) & Box_MR);

   function Box_Middle (Width : Natural; Middle : String)
   return Unbounded_String
   is (Box_Middle (Width, To_Unbounded_String (Middle)));

   function Box_Footer (Width : Natural; Footer : Unbounded_String)
   return Unbounded_String
   is (Box_BL & Pad_To (Footer, Width, Box_HB) & Box_BR);

   function Box_Footer (Width : Natural; Footer : String := "")
   return Unbounded_String
   is (Box_Footer (Width, To_Unbounded_String (Footer)));

   package Texts is new Ada.Containers.Vectors
     (Index_Type   => Natural,
      Element_Type => Unbounded_String);
   use Texts;
   
   subtype Text is Vector;

   procedure Print_Text (T : Text) is begin
      for L of T loop
         Put_Line (L);
      end loop;
   end Print_Text;

   function Zip_Text (T_1, T_2 : Text) return Text is
      Zipped : Text;
      Line : Unbounded_String;
      C_1 : Texts.Cursor := First (T_1);
      C_2 : Texts.Cursor := First (T_2);
   begin
      loop
         if Has_Element (C_1) and Has_Element (C_2) then
            Line := Element (C_1) & " " & Element (C_2);
            Append (Zipped, Line);
            Next (C_1);
            Next (C_2);
         elsif Has_Element (C_1) then
            Append (Zipped, Element (C_1));
            Next (C_1);
         elsif Has_Element (C_2) then
            Append (Zipped, Element (C_2));
            Next (C_2);
         else
            return Zipped;
         end if;
      end loop;
   end Zip_Text;

   function Draw_Scoreboard (Ps : Players; Width : Natural := 8) return Text is
      Output : Text;
      Score : Unbounded_String;
   begin
      Append (Output, Box_Header (Width, " Scores "));
      for P of Ps loop
         Score := To_Unbounded_String (Integer'Image (Bank_Value (P.Player_Bank)));
         Append (Output, Box_Row (Width, Player_Avatar (P) & Score));
      end loop;
      Append (Output, Box_Footer (Width));
      return Output;
   end Draw_Scoreboard;

   function Draw_Previous_Turn (Ps : Players; R : Round; Width : Natural := 30)
   return Text is
      Output : Text;
      C : Card;
      P : Player_Position;
   begin
      if Card_Maps.Is_Empty (R) then
         return Output;
      end if;

      Append (Output, Box_Header (Width, " Previous round "));
      for Cursor in R.Iterate loop
         C := Card_Maps.Key (Cursor);
         P := Card_Maps.Element (Cursor);
         Append (Output, Box_Row (Width, Player_Avatar (Ps (P)) & " played " & Show_Card (C)));
      end loop;
      Append (Output, Box_Footer (Width));
      return Output;
   end Draw_Previous_Turn;

   function Draw_Table (T : Table; Width : Natural := 67) return Text is
      Output : Text;
      Line : Unbounded_String;
   begin
      Append (Output, Box_Header (Width, " Table "));
      for I in T'Range loop
         Line := Character'Val(48 + I) & " " & Box_VB & " " & Show_Card_Set (T (I));
         Append (Output, Box_Row (Width, Line));
         if I /= T'Last then
            Append (Output, Box_Middle (Width, Box_HB & Box_HB & Box_HB & Box_MC));
         end if;
      end loop;
      Append (Output, Box_Footer (Width, Box_HB & Box_HB & Box_HB & Box_BT));
      return Output;
   end Draw_Table;

   function Draw_Player (P : Player; Width : Natural := 67) return Text is
      Output : Text;
      Hand_Row : Unbounded_String;
   begin
      if Card_Sets.Is_Empty (P.Player_Hand) then
         return Output;
      end if;

      Append (Output, Box_Header (Width, " Your hand "));
      for C of P.Player_Hand loop
         if Printable_Length (Hand_Row & Show_Card (C) & " ") >= Width then
            Append (Output, Box_Row (Width, Hand_Row));
            Hand_Row := Show_Card (C) & " ";
         else
            Append (Hand_Row, Show_Card (C) & " ");
         end if;
      end loop;
      Append (Output, Box_Row    (Width, Hand_Row));
      Append (Output, Box_Footer (Width));
      return Output;
   end Draw_Player;

   procedure Redraw_Screen (T : Table; Ps : Players; R : Round) is
      Output : Text;
   begin
      Put (ESC & "[2J"); -- Clear the screen

      Output := Zip_Text (Draw_Scoreboard (Ps), Draw_Previous_Turn (Ps, R));
      Append (Output, Draw_Table (T));
      for P of Ps loop
         if P.Intelligence = Human then
            Append (Output, Draw_Player (P));
         end if;
      end loop;
      Print_Text (Output);
   end Redraw_Screen;

   procedure Game_Over (T : Table; Ps : Players; R : Round) is
      Score : Natural;
      Lowest_Score : Natural := 999;
      Your_Score : Natural;
   begin
      Redraw_Screen (T, Ps, R);
      Put ("  Game over! ");
      for P of Ps loop
         Score := Bank_Value (P.Player_Bank);
         if P.Intelligence = Human then
            Your_Score := Score;
         end if;
         if Lowest_Score < Score then
            Lowest_Score := Score;
         end if;
      end loop;
      if Your_Score = Lowest_Score then
         Put_Line ("You won! 🎉");
      else
         Put_Line ("You lost. 😭");
      end if;
   end Game_Over;

   function Prompt_Card (H : Hand) return Card is
      I : Integer;
   begin
      Put ("  Your turn: ");
      Ada.Integer_Text_IO.Get (I);
      return Card (I);
   end Prompt_Card;

   function Prompt_Row_Index return Row_Index is
      I : Integer;
   begin
      Put ("  Which row? ");
      Ada.Integer_Text_IO.Get (I);
      return Row_Index (I);
   end Prompt_Row_Index;

end Six_Nimmt.CLI;
