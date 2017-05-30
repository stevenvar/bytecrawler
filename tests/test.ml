open Pervasive

(* type 'a list = Nil | Cons of 'a * 'a list *)

(* let f x = if x >= 0 then 2 else x *)

(* let g x = f (x + 1) *)

(* let rec taille = function *)
(*   | Nil -> 0 *)
(*   | Cons (x,xs) -> 1 + taille xs *)


(* let _ = *)
  (* Format.printf "%d"  @@ taille (Cons (5, Cons (4,(Cons (3,Nil))))) *)
(* print_int @@  f 8 *)

(* let rec odd n = *)
(*   if n = 0 then true *)
(*   else even (n - 1) *)
(* and even n = *)
(*   if n = 0 then false *)
(*   else  odd (n - 1);; *)

(* Format.printf "%d" 8989898 *)


(* let f x y z = x + y + z *)

(* let _ = *)
  (* let k = f 4 9 in *)
  (* k 28 *)

let f x = if x > 0 then 1 else 2 
let _ = f 4
