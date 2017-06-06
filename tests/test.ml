open Pervasive

let f x =
  let y = ref 42 in
  fun x ->
    let k = if x > 0 then !y else 4 in
    y := !y + 1;
    k

let _ =
  let f = f 3939 in
  for i = 10 to 13 do
    f 3
  done
