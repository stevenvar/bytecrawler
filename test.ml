
(* let f x = x *)

(* let g a = a *)

let f () =
  let x = 245 in
  fun () -> x

let _ =
  let k = f () in
  k ()

