open Pervasive

(* let f x = *)
(*   let y = ref 42 in *)
(*   fun x -> *)
(*     let k = if x > 0 then !y else 4 in *)
(*     y := !y + 1; *)
(*     k *)

(* let _ = *)
(*   let f = f 9 in *)
(*   begin_loop (); *)
(*   f 3; *)
(*   end_loop (); *)

let f x y = x + y

let _ =
  let k = f 3 in
  k 4
