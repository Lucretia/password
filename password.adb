-- Simple password generator
-- Luke A. Guest
-- 23/01/10

with Ada.Command_Line;
with Ada.Text_IO;
with Ada.Numerics.Discrete_Random;
with Ada.Characters.Latin_1;

procedure Password is
   package L1 renames Ada.Characters.Latin_1;

   package Random_Char is new Ada.Numerics.Discrete_Random (Character);

   Gen : Random_Char.Generator;
begin
   if Ada.Command_Line.Argument_Count /= 1 then
      Ada.Text_IO.Put_Line ("   Usage: password <length>");
      Ada.Text_IO.Put_Line ("     Creates a random set of characters from the range of [a..zA..Z0..9]");
   else
      declare
         Current :          Character            := Character'First;
         Length  : constant Integer              := Integer'Value (Ada.Command_Line.Argument (1));
         Pass    :          String (1 .. Length) := (others => L1.Space);
         Index   :          Positive             := Positive'First;
      begin
         Random_Char.Reset (Gen);

         while Index /= Length loop
            Current := Random_Char.Random (Gen);

            if Current in   L1.Exclamation         .. L1.Solidus
                          | '0'                    .. '9'
                          | L1.Colon               .. L1.Commercial_At
                          | 'A'                    .. 'Z'
                          | L1.Left_Square_Bracket .. L1.Low_Line
                          | L1.LC_A                .. L1.Tilde
            then
               Pass (Index) := Current;

               Index := @ + 1;  --  New Ada 2020 feature!
            end if;
         end loop;

         Ada.Text_IO.Put_Line ("Password is: " & Pass);
      end;
   end if;
end Password;
