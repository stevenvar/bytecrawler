open Avr

(* déclaration de l'écran *)
let lcd = LiquidCrystal.create4bitmode PIN13 PIN12 PIN18 PIN19 PIN20 PIN21

(* déclaration des pins *)
let plus = PIN7
let minus = PIN6
let resistor = PIN10
let sensor = PINA0

(* conversion de température *)
let convert_temp t =
  let f = (float_of_int (1033 - t) /. 11.67) in
  int_of_float (f*.100.)

(* lecture de la température *)
let read_temp () =
  let t = analog_read sensor in
  Serial.write_string "an=";
  Serial.write_int t;
  convert_temp t

(* Affichage des températures sur l'écran LCD *)
let print_temp wanted real =
  let split_temp t =
    let u = t/10 in
    let dec = t mod 10 in
    (u,dec) in
  LiquidCrystal.clear lcd;
  LiquidCrystal.home lcd;
  let (wu,wd) = split_temp wanted in
  let (ru,rd) = split_temp real in
  LiquidCrystal.print lcd "Wanted T :";
  LiquidCrystal.print lcd ((string_of_int wu)^"."^(string_of_int wd));
  LiquidCrystal.setCursor lcd 0 1;
  LiquidCrystal.print lcd "Actual T :";
  LiquidCrystal.print lcd ((string_of_int ru)^"."^(string_of_int rd))

(*** Fonctions d'entrées/sorties de l'instant synchrone  ***)

(** fonction d'initialisation **)
let init_thermo () =
  (* initialisation de la lecture analogique *)
  Avr.adc_init ();
  (* initialisation de l'écran *)
  LiquidCrystal.lcdBegin lcd 16 2;
  (* initialisation des broches *)
  pin_mode sensor INPUT;
  pin_mode resistor OUTPUT;
  pin_mode plus INPUT;
  pin_mode minus INPUT

(** fonction d'entrée **)
let input_thermo () =
  let plus = digital_read plus in
  let minus = digital_read minus in
  let plus = bool_of_level plus in
  let minus = bool_of_level minus in
  let real_temp = read_temp () in
  (plus,minus,real_temp)

(** fonction de sortie **)
let output_thermo (on,wanted,real,res) =
  delay 2000;
  if on then
    begin
      print_temp wanted real;
      digital_write resistor (if res then HIGH else LOW)
    end
  else
    begin
      LiquidCrystal.home lcd;
      LiquidCrystal.clear lcd;
      LiquidCrystal.print lcd "..."
    end;
