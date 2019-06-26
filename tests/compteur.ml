(* let%node count reset ~return:cpt =
 *   cpt = 0 ->> if reset then 0 else (cpt + 1) *)

(* open Avr *)

open Wcet

type ('a, 'b) count_state =
  {
    mutable count_pre_cpt: 'a ;
    mutable count_out_cpt: 'b }


(* Input functions *)
let input_count_r () = (digital_read PIN10 = HIGH)
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
  let _st = count_alloc () in
  while true do
   begin_loop ();
   let r = input_count_r () in
   count_step _st r;
   output_count _st.count_out_cpt;
   end_loop ()
   done
