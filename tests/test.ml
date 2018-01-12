open Pervasive

(* let f x =
 *   let y = ref 42 in
 *   fun x ->
 *     let k = if x > 0 then !y else 4 in
 *     y := !y + 1;
 *     k
 *
 * let _ =
 *   let f = f 9 in
 *   begin_loop ();
 *   f 3;
 *   end_loop (); *)


let nat () =
  let st_n = ref 0 in
  let nat_step () =
    let n = !st_n in
    st_n := n + 1;
    n
in nat_step

let _ =
  let nat  = nat () in
  begin_loop ();
  nat ();
  end_loop ()
