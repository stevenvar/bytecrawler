open Wcet


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
