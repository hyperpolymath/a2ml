with Ada.Text_IO;
with Ada.Strings.Fixed;
with Ada.Command_Line;
with GNAT.OS_Lib;

procedure Main is
   pragma SPARK_Mode (Off);

   use Ada.Text_IO;
   use Ada.Strings.Fixed;

   function Read_Line_Trim return String is
      Buffer : String (1 .. 1024);
      Last   : Natural;
   begin
      Get_Line (Buffer, Last);
      return Trim (Buffer (1 .. Last), Ada.Strings.Both);
   end Read_Line_Trim;

   function Build_Command (Command : String; Input : String; Mode : String; Out_Path : String; Concat : Boolean) return String is
      Cmd : String := "just cli " & Command & " " & Input;
   begin
      if Mode /= "" then
         Cmd := Cmd & " --mode " & Mode;
      end if;
      if Out_Path /= "" then
         Cmd := Cmd & " --out " & Out_Path;
      end if;
      if Concat then
         Cmd := Cmd & " --concat";
      end if;
      return Cmd;
   end Build_Command;

   procedure Run_Command (Cmd : String) is
      Args   : GNAT.OS_Lib.Argument_List_Access;
      Result : Integer;
   begin
      Args := GNAT.OS_Lib.Argument_String_To_List (Cmd);
      Result := GNAT.OS_Lib.Spawn (Args (Args'First), Args);
      GNAT.OS_Lib.Free (Args);
      Put_Line ("Exit code: " & Integer'Image (Result));
   end Run_Command;

   Choice : String := "";
   Input  : String := "";
   Mode   : String := "checked";
   Outp   : String := "";
   Concat : Boolean := False;
begin
   Put_Line ("A2ML Ada TUI (prototype)");
   Put_Line ("------------------------");
   loop
      Put_Line ("");
      Put_Line ("1) Render HTML");
      Put_Line ("2) Validate (checked)");
      Put_Line ("3) Dump AST (JSON)");
      Put_Line ("4) Toggle mode (lax/checked) [current: " & Mode & "]");
      Put_Line ("5) Toggle concat [current: " & (if Concat then "on" else "off") & "]");
      Put_Line ("6) Set output path [current: " & (if Outp = "" then "stdout" else Outp) & "]");
      Put_Line ("7) Quit");
      Put ("> ");
      Choice := Read_Line_Trim;

      if Choice = "7" then
         exit;
      elsif Choice = "4" then
         if Mode = "checked" then
            Mode := "lax";
         else
            Mode := "checked";
         end if;
      elsif Choice = "5" then
         Concat := not Concat;
      elsif Choice = "6" then
         Put ("Output path (empty for stdout): ");
         Outp := Read_Line_Trim;
      elsif Choice = "1" or else Choice = "2" or else Choice = "3" then
         Put ("Input file (or '-' for stdin): ");
         Input := Read_Line_Trim;
         if Input = "" then
            Put_Line ("No input provided.");
         else
            declare
               Cmd : String :=
                 (if Choice = "1" then Build_Command ("render", Input, Mode, Outp, Concat)
                  elsif Choice = "2" then Build_Command ("validate", Input, Mode, Outp, Concat)
                  else Build_Command ("ast", Input, Mode, Outp, Concat));
            begin
               Put_Line ("Running: " & Cmd);
               Run_Command (Cmd);
            end;
         end if;
      else
         Put_Line ("Unknown choice.");
      end if;
   end loop;
end Main;
