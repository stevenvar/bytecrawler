
open Wcet

type ('a,'b) count_state =
  {mutable count_fby_aux :'a ;
   mutable count_out_cpt :'b}

let count_alloc () =
 let cpt = Obj.magic () in
 let aux = 0 in
 {count_fby_aux = aux ; count_out_cpt = cpt }

let count_next state r =
  let aux = state.count_fby_aux in
  let cpt = if r then 0 else aux in
  let fby_aux = cpt + 1 in
  state.count_fby_aux <- fby_aux ;
  state.count_out_cpt <- cpt


let _ =
  let st = count_alloc () in
  let b = rand_bool () in
   begin_loop ();
   count_next st b;
   end_loop ()
