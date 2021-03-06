exception Stack_empty

(* type 'a t = { mutable tab : 'a array ; mutable size : int } *)

type 'a t = 'a list ref

(* let create () = { tab = [||] ; size = 0 } *)

let create () = ref []

(* let is_empty s = s.size = 0 *)

let is_empty s = !s = []

(* let stack_full s = Array.length s.tab = s.size *)

(* let stack_realloc s =
 *   let tab' = Array.make (s.size * 2) s.tab.(0) in
 *   Array.blit s.tab 0 tab' 0 s.size;
 *   { size = s.size ; tab = tab'} *)

(* let push s x =
 *   if is_empty s then
 *     (s.size <- 1 ; s.tab <- [| x |])
 *   else
 *     let s' = if stack_full s then stack_realloc s else s in
 *     s'.tab.(s'.size) <- x;
 *     s'.size <- s'.size + 1;
 *     s.size <- s'.size;
 *     s.tab <- s'.tab *)

let push s x = 
  s := x::!s

(* let top s =
 *   if is_empty s then raise Stack_empty
 *   else
 *     s.tab.(s.size -1) *)

let top s =
  if is_empty s then raise Stack_empty
  else
    List.hd !s

(* let pop s =
 *   if is_empty s then raise Stack_empty
 *   else
 *     let e = top s in
 *     s.size <- s.size -1 ;
 *     e *)

let pop s =
  if is_empty s then raise Stack_empty
  else
    let e = top s in
    s:=List.tl !s;
    e 


(* let peek s n =
 *   if is_empty s || s.size < n then raise Stack_empty
 *   else
 *     s.tab.(s.size - n - 1 ) *)


let peek s n = List.nth !s n 
  

(* let popn s n =
 *   for i = 0 to n - 1 do
 *     pop s
 *   done *)


let popn s n = 
  for i = 0 to n - 1 do 
    pop s 
  done

(* let length s = s.size *)

let length s = List.length !s

(* let set s n x =
 *   s.tab.(s.size - 1 - n) <- x *)

let set s n x = 
  let a = Array.of_list !s in 
  a.(n) <- x;
  s := Array.to_list a

(* let fold_left f init s =
 *   let acc = ref init in
 *   for i = s.size -1 downto 0 do
 *     acc := f !acc s.tab.(i)
 *   done;
 *   !acc *)

let fold_left f init s = List.fold_left f init !s

(* let to_string s f =
 *   let rec string_of_list i l =
 *       match l with
 *       | [] -> ""
 *       | [x] -> Format.asprintf "\t %d \t %s\n" i (f x)
 *       | x::xs ->
 *         if i = length s then Format.asprintf "\t %d \t %s\n" i (f x)
 *         else
 *           Format.asprintf "\t %d \t %s,\n%s" i (f x) (string_of_list (i+1) xs)
 *   in
 *   let string_of_array a = string_of_list 0 (Array.to_list a) in
 *   let str = string_of_array s.tab in
 *   Format.asprintf "[\n%s]<size=%d>" str s.size *)


let to_string s f =  
  let rec string_of_list i l = 
    match l with 
      | [] -> ""
      | [x] -> Printf.sprintf "\t %d \t %s\n" i (f x)
      | x::xs -> 
         Printf.sprintf "\t %d \t %s,\n%s" i (f x) (string_of_list (i+1) xs)
  in 
  let s = string_of_list 0 !s in 
  Printf.sprintf "\n%s\n" s

let load s = !s

let store s s' = 
  s := s'
