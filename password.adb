-- Simple password generator
-- Luke A. Guest
-- 23/01/10

with Ada.Command_Line;
with Ada.Text_IO;
with Ada.Numerics.Discrete_Random;

procedure Password is
   type Char_Set is
     ('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n',
      'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z',
      '1', '2', '3', '4', '5', '6', '7', '8', '9', '0',
      'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N',
      'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z');

   package Random_Char is new Ada.Numerics.Discrete_Random(Char_Set);
   use Random_Char;

   Gen : Generator;
begin
   if Ada.Command_Line.Argument_Count /= 1 then
      Ada.Text_IO.Put_Line("   Usage: password <length>");
      Ada.Text_IO.Put_Line("     Creates a random set of characters from the range of [a..zA..Z0..9]");
   else
      declare
         Length : constant Integer := Integer'Value(Ada.Command_Line.Argument(1));
         Pass   : String(1 .. Length);
      begin
         Reset(Gen);

         for Index in 1 .. Length loop
            Pass(Index) := Char_Set'Image(Random(Gen))(2);
         end loop;

         Ada.Text_IO.Put_Line("Password is: " & Pass);
      end;
   end if;
end Password;
