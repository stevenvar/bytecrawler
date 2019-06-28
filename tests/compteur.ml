(* let%node count reset ~return:cpt =
 *   cpt = 0 ->> if reset then 0 else (cpt + 1) *)



open Wcet

type ('a, 'b) count_state =
  {
    mutable count_pre_cpt: 'a ;
    mutable count_out_cpt: 'b }




(* Input functions *)
let input_count_r () = (digital_read PIN10 = HIGH)

open Avr

(* Output function *)
let output_count cpt  = ()

let count_alloc r =
  let cpt = Obj.magic () in
  let pre_cpt = 0 in
  { count_pre_cpt = pre_cpt; count_out_cpt = cpt}

let count_step state r =
  let cpt = state.count_pre_cpt in
  let _cpt_aux1 = if r then 0 else cpt + 1 in
  let pre_cpt = _cpt_aux1 in
  state.count_out_cpt <- cpt; state.count_pre_cpt <- pre_cpt; ()

let () =
  (* let millis_cost = ref (millis ()) in
   * for i = 1 to 10000 do
   *   ignore (millis ());
   * done;
   * millis_cost := (millis ()) - !millis_cost;
   * millis_cost := !millis_cost / 500;
   * Serial.write_string "millis cost =";
   * Serial.write_int !millis_cost;
   *)
  (*  let loop_cost = ref (millis ()) in
   * for i = 1 to 10000 do
   *   ()
   * done;
   * loop_cost := millis () - !loop_cost;
   * Serial.write_string "for cost =";
   * Serial.write_int !loop_cost;
   *)

  let _st = count_alloc () in

  let n = millis () in
    begin_loop ();
    (* for i = 1 to 1000 do *)
      let reset =  input_count_r () in
      count_step _st reset;
      output_count _st.count_out_cpt;
    (* done; *)
    end_loop ();
  let n' = millis () in
  Serial.write_int (n'-n)
