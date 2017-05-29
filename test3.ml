
type t = | Alice | Bob | Charlie of int | David 

let test v =
  match v with
  | Alice   -> 100
  | Bob     -> 101
  | Charlie _ -> 102
  | David  -> 103

let _ = test Bob 
