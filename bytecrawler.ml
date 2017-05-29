open OByteLib
open OByteLib.Instr

type value = Ptr of int
           | Dummy
           | Int of int
           | Float of float
           | String of string
           | Object of value array
           | Block of int * value array
           | Closure of value * value array
           | Closure_rec of value * int array * value array * int

module Stack = struct
  exception Stack_empty

  type 'a t = { mutable tab : 'a array ; mutable size : int }

  let create () = { tab = [||] ; size = 0 }

  let is_empty s = s.size = 0

  let stack_full s = Array.length s.tab = s.size

  let stack_realloc s =
    let tab' = Array.make (s.size * 2) s.tab.(0) in
    Array.blit s.tab 0 tab' 0 s.size;
    { size = s.size ; tab = tab'}

  let push s x =
    if is_empty s then
      (s.size <- 1 ; s.tab <- [| x |])
    else
      let s' = if stack_full s then stack_realloc s else s in
      s'.tab.(s'.size) <- x;
      s'.size <- s'.size + 1;
      s.size <- s'.size;
      s.tab <- s'.tab

  let top s =
    if is_empty s then raise Stack_empty
    else
      s.tab.(s.size -1)

  let pop s =
    if is_empty s then raise Stack_empty
    else
      let e = top s in
      s.size <- s.size -1 ;
      e

  let peek s n =
    if is_empty s || s.size < n then raise Stack_empty
    else
      s.tab.(s.size - n - 1 )

  let popn s n =
    for i = 0 to n - 1 do
      pop s
    done

  let length s = s.size

  let set s n x =
    s.tab.(s.size - 1 - n) <- x

  let to_string s f =
    let str = ref "" in
    for i = s.size -1 downto 0  do
      let x = s.tab.(i) in
      str := !str^(f x)^" , ";
    done;
    str:= String.sub !str 0 (max 0 (String.length !str -3));
    "[|"^(!str)^"|] <size="^(string_of_int s.size)^">"
end

type state = { mutable pc : int;
               mutable acc : value;
               stack : value Stack.t;
               mutable extraArgs : int ;
               mutable env : value;
               global : value array;
               mutable trapSp : int; }

let new_state data = { pc = 0;
                     acc = Dummy;
                     stack = Stack.create ();
                     extraArgs = 0;
                     env = Block (0,[| |]) ;
                     global = data;
                       trapSp = 0 }

let int_op f x y =
  match x,y with
  | Int x, Int y -> Int (f x y)
  | _ -> Dummy

let field blk n =
  match blk with
  | Block (tag,tab) -> tab.(n)
  | Closure (_,tab) -> tab.(n-1)
  | Closure_rec (_,_,tab,_) -> tab.(n)
  | _ -> failwith "not an environment"

let set_field blk n x =
  match blk with
  | Block (_,tab) -> tab.(n) <- x
  | _ -> failwith "not an environment"

let offsetclosure blk n =
  match blk with
  | Closure_rec (o,t,blk,i) -> Closure_rec (o,t,blk,i + n/2)
  | _ -> failwith "not a recursive closure"

let tabs n =
  let rec loop i acc =
    if i <= 0 then acc
    else
      loop (i-1) " \t \t "^acc
  in
  loop n ""


let rec string_of_value = function
  | Dummy -> "?"
  | String s -> "\""^s^"\""
  | Int v -> string_of_int v
  | Float f -> string_of_float f
  | Ptr x -> "@"^string_of_int x
  | Object e ->
    let s = (Array.fold_left (fun acc x -> acc^(string_of_value x)^",") "" e) in
    let s = String.sub s 0 (max 0 (String.length s -1)) in
    "("^s^")"
  | Block (tag,b) ->
    let s = (Array.fold_left (fun acc x -> acc^(string_of_value x)^",") ":" b) in
    let s = String.sub s 0 (max 0 (String.length s -1)) in
    "["^(string_of_int tag)^s^"]"
  | Closure (ptr,env) ->
    let s = (Array.fold_left (fun acc x -> acc^(string_of_value x)^",") ":" env) in
    let s = String.sub s 0 (max 0 (String.length s -1)) in
    "{"^(string_of_value ptr)^s^"}"
  | Closure_rec (ptr,t,env,i) ->
    let s = (Array.fold_left (fun acc x -> acc^(string_of_value x)^",") ":" env) in
    let s = String.sub s 0 (max 0 (String.length s -1)) in
    "<"^(string_of_value ptr)^"("^(string_of_int i)^")"^s^">"

let size_of_value v =
  match v with
  | Closure (_,b) |
    Closure_rec (_,_,b,_) |
    Block (_,b) -> Array.length b
  | _ -> 0

let print_state state level t =
  Printf.printf
    "%sstack = %s
%senv = %s
%sacc= %s
%sextra_args = %d
%s_________________
%s%d:%s\n\n"
    (tabs level)
    (Stack.to_string state.stack string_of_value)
    (tabs level)
    (string_of_value state.env)
    (tabs level)
    (string_of_value state.acc)
    (tabs level)
    state.extraArgs
    (tabs level)
    (tabs level)
    state.pc
    (Instr.to_string t.(state.pc))

let rec ptr_of_value = function
  | Ptr x -> x
  | Closure (ptr,env) -> ptr_of_value ptr
  | Closure_rec (ptr,t,_,i) -> if i = 0 then ptr_of_value ptr else
      t.(i-1)
  | _ -> failwith "not a ptr"

let int_of_value = function
  | Int v -> v
  | x -> failwith @@ (string_of_value x)^" : not an int"

(* let ending_block a from = *)
(*   let rec loop i = *)
(*     let instr = a.(i) in *)
(*     match instr with *)
(*     | BRANCH ptr -> i *)
(*     | RETURN _ -> i *)
(*     | APPTERM1 _ -> i *)
(*     | STOP -> i *)
(*     | _ -> loop (i+1) *)
(*   in *)
(*   loop from *)

(* let starting_blocks a = *)
(*   let rec loop i l = *)
(*     let instr = a.(i) in *)
(*     if instr = STOP then l *)
(*     else ( *)
(*     let new_l = *)
(*       match instr with *)
(*       | BRANCH ptr -> ptr::l *)
(*       | BRANCHIF ptr -> ptr::(i+1)::l *)
(*       | CLOSURE (s,ptr) -> ptr::l *)
(*       | _ -> l *)
(*     in *)
(*     loop (i+1) new_l ) *)
(*   in *)
(*   loop 0 [0] *)

let env_of_closure env =
  match env with
  | Closure (_, blk) -> blk
  | _ -> failwith "not a closure"

exception Stop


let rec interp_loop level bytecode state : unit =
  let inst = bytecode.(state.pc) in
  let next = { state with pc = state.pc + 1 } in
  let level = max level 0 in
  print_state state level bytecode;
  let _ = read_line () in
  match inst with
  | ACC0 -> let i = Stack.peek state.stack 0 in
    interp_loop level bytecode { next with acc = i }
  | ACC1 -> let i = Stack.peek state.stack 1 in
    interp_loop level bytecode { next with acc = i }
  | ACC2 -> let i = Stack.peek state.stack 2 in
    interp_loop level bytecode { next with acc = i }
  | ACC3 -> let i = Stack.peek state.stack 3 in
    interp_loop level bytecode { next with acc = i }
  | ACC4 -> let i = Stack.peek state.stack 4 in
    interp_loop level bytecode { next with acc = i }
  | ACC5 -> let i = Stack.peek state.stack 5 in
    interp_loop level bytecode { next with acc = i }
  | ACC6 -> let i = Stack.peek state.stack 6 in
    interp_loop level bytecode { next with acc = i }
  | ACC7 -> let i = Stack.peek state.stack 7 in
    interp_loop level bytecode { next with acc = i }
  | ACC n -> let i = Stack.peek state.stack n in
    interp_loop level bytecode { next with acc = i }
  | PUSH -> Stack.push state.stack state.acc;
    interp_loop level bytecode next
  | PUSHACC0 ->
    Stack.push state.stack state.acc;
    let i = Stack.peek state.stack 0 in
    interp_loop level bytecode { next with acc = i }
  | PUSHACC1 ->
    Stack.push state.stack state.acc;
    let i = Stack.peek state.stack 1 in
    interp_loop level bytecode { next with acc = i }
  | PUSHACC2 ->
    Stack.push state.stack state.acc;
    let i = Stack.peek state.stack 2 in
    interp_loop level bytecode { next with acc = i }
  | PUSHACC3 ->
    Stack.push state.stack state.acc;
    let i = Stack.peek state.stack 3 in
    interp_loop level bytecode { next with acc = i }
  | PUSHACC4 ->
    Stack.push state.stack state.acc;
    let i = Stack.peek state.stack 4 in
    interp_loop level bytecode { next with acc = i }
  | PUSHACC5 ->
    Stack.push state.stack state.acc;
    let i = Stack.peek state.stack 5 in
    interp_loop level bytecode { next with acc = i }
  | PUSHACC6 ->
    Stack.push state.stack state.acc;
    let i = Stack.peek state.stack 6 in
    interp_loop level bytecode { next with acc = i }
  | PUSHACC7 ->
    Stack.push state.stack state.acc;
    let i = Stack.peek state.stack 7 in
    interp_loop level bytecode { next with acc = i }
  | PUSHACC n ->
    Stack.push state.stack state.acc;
    let i = Stack.peek state.stack n in
    interp_loop level bytecode { next with acc = i }
  | POP n -> Stack.popn state.stack n;
    interp_loop level bytecode next
  | ASSIGN n -> Stack.set state.stack n state.acc;
    interp_loop level bytecode { next with acc = Dummy }
  | ENVACC1                   ->
    interp_loop level bytecode { next with acc = field state.env 1 }
  | ENVACC2                   ->
    interp_loop level bytecode { next with acc = field state.env 2 }
  | ENVACC3                   ->
    interp_loop level bytecode { next with acc = field state.env 3 }
  | ENVACC4                   ->
    interp_loop level bytecode { next with acc = field state.env 4 }
  | ENVACC n                   ->
    interp_loop level bytecode { next with acc = field state.env n }
  | PUSHENVACC1               ->
    Stack.push state.stack state.acc;
    let acc = field state.env 1 in
    interp_loop level bytecode { next with acc = acc }
  | PUSHENVACC2               ->
    Stack.push state.stack state.acc;
    interp_loop level bytecode { next with acc = field state.env 2 }
  | PUSHENVACC3               ->
    Stack.push state.stack state.acc;
    interp_loop level bytecode { next with acc = field state.env 3 }
  | PUSHENVACC4               ->
    Stack.push state.stack state.acc;
    interp_loop level bytecode { next with acc = field state.env 4 }
  | PUSHENVACC n               ->
    Stack.push state.stack state.acc;
    interp_loop level bytecode { next with acc = field state.env n }
  | PUSH_RETADDR ptr ->
    Stack.push state.stack (Int state.extraArgs);
    Stack.push state.stack state.env;
    Stack.push state.stack (Ptr ptr);
    interp_loop level bytecode next
  | APPLY n ->
    interp_loop (level+1) bytecode { next with pc = ptr_of_value state.acc ;
                                           env = state.acc ;
                                           extraArgs = n - 1 }
  | APPLY1 ->
    let arg = Stack.pop state.stack in
    Stack.push state.stack (Int state.extraArgs);
    Stack.push state.stack state.env;
    Stack.push state.stack (Ptr (state.pc + 1));
    Stack.push state.stack arg;
    interp_loop (level+1) bytecode { next with pc = ptr_of_value state.acc ;
                                           env = state.acc ;
                                           extraArgs = 0 }
  | APPLY2 ->
    let arg1 = Stack.pop state.stack in
    let arg2 = Stack.pop state.stack in
    Stack.push state.stack (Int state.extraArgs);
    Stack.push state.stack state.env;
    Stack.push state.stack (Ptr (state.pc + 1));
    Stack.push state.stack arg2;
    Stack.push state.stack arg1;
    interp_loop level bytecode { next with pc = ptr_of_value state.acc ;
                                           env = state.acc ;
                                           extraArgs = 1 }
  | APPLY3 ->
    let arg1 = Stack.pop state.stack in
    let arg2 = Stack.pop state.stack in
    let arg3 = Stack.pop state.stack in
    Stack.push state.stack (Int state.extraArgs);
    Stack.push state.stack state.env;
    Stack.push state.stack (Ptr (state.pc + 1) );
    Stack.push state.stack arg3;
    Stack.push state.stack arg2;
    Stack.push state.stack arg1;
    interp_loop level bytecode { next with pc = ptr_of_value state.acc ;
                                           env = state.acc ;
                                           extraArgs = 2 }
  | APPTERM (n, s)  ->
    for i = 0 to n - 1 do
      let arg = Stack.peek state.stack (n- i - 1) in
      Stack.set state.stack (s - i - 1) arg
    done;
    Stack.popn state.stack (s - n);
    let extraArgs = state.extraArgs + (n - 1) in
    let env = state.acc in
    interp_loop level bytecode { next with pc = ptr_of_value state.acc ;
                                           env = env ;
                                           extraArgs = extraArgs }
  | APPTERM1 n ->
    let arg = Stack.top state.stack in
    Stack.popn state.stack (n - 1);
    (* Format.printf "---> %d <<<---" (int_of_value arg); *)
    Stack.set state.stack 0 arg;
    let env = state.acc in
    interp_loop level bytecode { next with pc = ptr_of_value state.acc ;
                                           env = env}
  | APPTERM2 s ->
    let arg1 = Stack.top state.stack in
    let arg2 = Stack.top state.stack in
    Stack.popn state.stack (s - 2);
    Stack.set state.stack 0 arg1;
    Stack.set state.stack 1 arg2;
    let env = state.acc in
    let extraArgs = state.extraArgs + 1 in
    interp_loop level bytecode { next with pc = ptr_of_value state.acc;
                                           env = env ; extraArgs =
                                                         extraArgs }
  | APPTERM3 s ->
     let arg1 = Stack.top state.stack in
     let arg2 = Stack.top state.stack in
     let arg3 = Stack.top state.stack in
     Stack.popn state.stack (s - 3);
     Stack.set state.stack 0 arg1;
     Stack.set state.stack 1 arg2;
     Stack.set state.stack 2 arg3;
    let extraArgs = state.extraArgs + 2 in
    let env = state.acc in
    interp_loop level bytecode { next with pc = ptr_of_value state.acc ;
                                           env = env ;
                                           extraArgs = extraArgs }
  | RETURN n ->
    Stack.popn state.stack n;
    if (state.extraArgs = 0) then
      (
        let pc = ptr_of_value (Stack.pop state.stack) in
        let env = Stack.pop state.stack in
        let extraArgs = int_of_value (Stack.pop state.stack) in
        interp_loop (level-1) bytecode { next with pc ; env ; extraArgs}
      )
    else (
      let pc = ptr_of_value state.acc in
      let env = state.acc in
      let extraArgs = state.extraArgs - 1 in
      interp_loop level bytecode { next with pc; env ; extraArgs}
    )
  | RESTART ->
    let blk = env_of_closure state.env in
    let n = Array.length blk - 1 in
    Format.printf "--> %d" n;
    for i = n downto 1 do
      Stack.push state.stack blk.(i)
    done;
    let extraArgs = state.extraArgs + n in
    let env = blk.(0) in
    interp_loop level bytecode { next with env ; extraArgs}
  | GRAB n ->
    if state.extraArgs >= n then
      let extraArgs = state.extraArgs - n in
      interp_loop level bytecode { next with extraArgs = extraArgs }
    else
      let a = Array.make (state.extraArgs + 2) (state.acc) in
      for i = 1 to state.extraArgs + 1 do
        a.(i) <- Stack.pop state.stack
      done;
      let acc = Closure (Ptr (state.pc - 1) , a) in
      (* set_field state.acc 1 state.env; *)
      (* for i = 0 to state.extraArgs do *)
      (*   set_field state.acc (i+2) (Stack.pop state.stack) *)
      (* done; *)
      let sp = state.trapSp + state.extraArgs + 1 in
      let pc = ptr_of_value (Stack.pop state.stack) in
      let env = Stack.pop state.stack in
      let extraArgs = int_of_value (Stack.pop state.stack) in
      interp_loop level bytecode { next with acc ; pc ; trapSp = sp;  env ; extraArgs }
  | CLOSURE (n,ptr) ->
    let a = Array.make n (state.acc) in
    for i = 1 to n - 1 do
      a.(i) <- Stack.pop state.stack;
    done;
    interp_loop level bytecode { next with acc = Closure(Ptr ptr,a) }
  | CLOSUREREC (f, v, ptr, t)   ->
    (* f = number of functions
     * v = number of variables *)
    (* if v > 0 then Stack.push state.stack state.acc; *)
    let blk = Array.make v (state.acc) in
    for i = 1 to v - 1 do
      blk.(i) <- Stack.pop state.stack;
    done;
    let acc = Closure_rec (Ptr ptr,t,blk,0) in
    Stack.push state.stack acc;
    for i = 1 to Array.length t do
      Stack.push state.stack (Block (0,[||]));
    done;
    interp_loop level bytecode {next with acc = acc }
  | OFFSETCLOSUREM2           ->
    let acc = offsetclosure state.env (-2) in
    interp_loop level bytecode { next with acc}
  | OFFSETCLOSURE0            ->
    let acc = offsetclosure state.env 0 in
    interp_loop level bytecode { next with acc}
  | OFFSETCLOSURE2            ->
    let acc = offsetclosure state.env 2 in
    interp_loop level bytecode { next with acc}
  | OFFSETCLOSURE n           ->
    let acc = offsetclosure state.env n in
    interp_loop level bytecode { next with acc}
  | PUSHOFFSETCLOSUREM2       ->
    Stack.push state.stack state.acc;
    let acc = offsetclosure state.env (-2) in
    interp_loop level bytecode { next with acc}
  | PUSHOFFSETCLOSURE0        ->
    Stack.push state.stack state.acc;
    let acc = offsetclosure state.env 0 in
    interp_loop level bytecode { next with acc}
  | PUSHOFFSETCLOSURE2        ->
    Stack.push state.stack state.acc;
    let acc = offsetclosure state.env 2 in
    interp_loop level bytecode { next with acc }
  | PUSHOFFSETCLOSURE n       ->
    Stack.push state.stack state.acc;
    let acc = offsetclosure state.env n
    in interp_loop level bytecode { next with acc }
  | GETGLOBAL n               ->
    let acc = state.global.(n) in
    interp_loop level bytecode { next with acc = acc }
  | PUSHGETGLOBAL n           ->
    Stack.push state.stack state.acc;
    let acc = state.global.(n) in
    interp_loop level bytecode { next with acc = acc }
  | GETGLOBALFIELD (n, p)     ->
    let acc = field state.global.(n) p in
    interp_loop level bytecode { next with acc = acc }
  | PUSHGETGLOBALFIELD (n, p) ->
    Stack.push state.stack state.acc;
    let acc = field state.global.(n) p in
    interp_loop level bytecode { next with acc = acc }
  | SETGLOBAL n               ->
    state.global.(n) <- state.acc ;
    interp_loop level bytecode { next with acc = Dummy }
  | ATOM0                     ->
    let blk = Block (0, [||]) in
    interp_loop level bytecode {next with acc = blk }
  | ATOM tag                  ->
    let blk = Block (tag, [||]) in
    interp_loop level bytecode { next with acc = blk }
  | PUSHATOM0                 ->
    Stack.push state.stack state.acc;
    let blk = Block (0 , [||]) in
    interp_loop level bytecode { next with acc = blk }
  | PUSHATOM tag              ->
    Stack.push state.stack state.acc;
    let blk = Block (0, [||]) in
    interp_loop level bytecode { next with acc = blk }
  | MAKEBLOCK (tag, sz)       ->
    let a = Array.make sz Dummy in
    let blk = Block (tag, a) in
    a.(0) <- state.acc;
    for i = 1 to sz - 1 do
      a.(i) <- Stack.pop state.stack;
    done;
    interp_loop level bytecode { next with acc = blk }
  | MAKEBLOCK1 tag            ->
    let a = Array.make 1 (state.acc) in
    let blk = Block (tag, a) in
    interp_loop level bytecode { next with acc = blk }
  | MAKEBLOCK2 tag            ->
    let a = Array.make 2 (state.acc) in
    let blk = Block (tag, a) in
    a.(1) <- Stack.pop state.stack;
    interp_loop level bytecode { next with acc = blk }
  | MAKEBLOCK3 tag            ->
    let a = Array.make 3 (state.acc) in
    let blk = Block (tag, a) in
    a.(1) <- Stack.pop state.stack;
    a.(2) <- Stack.pop state.stack;
    interp_loop level bytecode { next with acc = blk }
  | MAKEFLOATBLOCK n         ->
    let a = Array.make n Dummy in
    let blk = Block (Obj.double_array_tag,a) in
    a.(0) <- state.acc;
    Stack.popn state.stack (n-1);
    interp_loop level bytecode { next with acc = blk }
  | GETFIELD0                 ->
    let acc = field state.acc 0 in
    interp_loop level bytecode { next with acc = acc }
  | GETFIELD1                 ->
    let acc = field state.acc 1 in
    interp_loop level bytecode { next with acc = acc }
  | GETFIELD2                 ->
    let acc = field state.acc 2 in
    interp_loop level bytecode { next with acc = acc }
  | GETFIELD3                 ->
    let acc = field state.acc 3 in
    interp_loop level bytecode { next with acc = acc }
  | GETFIELD n                ->
    let acc = field state.acc n in
    interp_loop level bytecode { next with acc = acc }
  | GETFLOATFIELD i ->
    let acc = Dummy in
    interp_loop level bytecode { next with acc = acc }
  | SETFIELD0                 ->
    set_field state.acc 0 (Stack.pop state.stack);
    let acc = Dummy in
    interp_loop level bytecode { next with acc = acc }
  | SETFIELD1                 ->
    set_field state.acc 1 (Stack.pop state.stack);
    let acc = Dummy in
    interp_loop level bytecode { next with acc = acc }
  | SETFIELD2                 ->
    set_field state.acc 2 (Stack.pop state.stack);
    let acc = Dummy in
    interp_loop level bytecode { next with acc = acc }
  | SETFIELD3                 ->
    set_field state.acc 3 (Stack.pop state.stack);
    let acc = Dummy in
    interp_loop level bytecode { next with acc = acc }
  | SETFIELD n                 ->
    set_field state.acc n (Stack.pop state.stack);
    let acc = Dummy in
    interp_loop level bytecode { next with acc = acc }
  | SETFLOATFIELD n           ->
    let acc = Dummy in
    interp_loop level bytecode { next with acc = acc }
  | VECTLENGTH                ->
    let acc = Dummy in
    interp_loop level bytecode { next with acc = acc }
  | GETVECTITEM               ->
    ignore @@ Stack.pop state.stack;
    let acc = Dummy in
    interp_loop level bytecode { next with acc = acc }
  | SETVECTITEM               ->
    ignore @@ Stack.pop state.stack;
    ignore @@ Stack.pop state.stack;
    let acc = Dummy in
    interp_loop level bytecode { next with acc = acc }
  | GETSTRINGCHAR             ->
    ignore @@ Stack.pop state.stack;
    let acc = Dummy in
    interp_loop level bytecode { next with acc = acc }
  | SETSTRINGCHAR             ->
    ignore @@ Stack.pop state.stack;
    ignore @@ Stack.pop state.stack;
    let acc = Dummy in
    interp_loop level bytecode { next with acc = acc }
  | BRANCH b -> interp_loop level bytecode { next with pc = b }
  | BRANCHIF ptr              ->
     begin
      match state.acc with
        | Dummy ->
          begin
          try
            interp_loop (level+1) bytecode { next with pc = ptr };
          with _ -> ();
            try
              interp_loop level bytecode next
            with _ -> ()
        end
      | Int 0 -> interp_loop level bytecode next
      | _ -> interp_loop (level+1) bytecode { next with pc = ptr }
    end
  | BRANCHIFNOT ptr           ->
    begin
      match state.acc with
      | Dummy ->
        begin
          try
            interp_loop (level+1) bytecode { next with pc = ptr }
          with _ -> ();
            try
              interp_loop level bytecode next
            with _ -> ();
      end
      | Int 0 -> interp_loop (level+1) bytecode { next with pc = ptr }
      | _ -> interp_loop level bytecode next
      (* | _ -> failwith "wrong accumulator" *)
    end
  | SWITCH (n, ptrs)          ->
    (* Probleme ici  *)
    begin
    match state.acc with
      | Int v ->
        interp_loop level bytecode { next with pc = ptrs.(v) }
      | Block (tag,b) ->  interp_loop level bytecode { next with pc = ptrs.(tag + (n land 0xFFFF)) }
      | _ -> failwith "?"
  end
  | BOOLNOT                   ->
    let acc = match state.acc with
      | Int 0 -> Int 1
      | Int 1 -> Int 0
      | _ -> failwith "not a bool"
    in
    interp_loop level bytecode { next with acc }
  | PUSHTRAP ptr              ->
    Stack.push state.stack (Int state.extraArgs);
    Stack.push state.stack state.env;
    Stack.push state.stack (Int state.trapSp);
    Stack.push state.stack (Ptr ptr);
    interp_loop level bytecode { next with trapSp = Stack.length state.stack }
  | POPTRAP                   ->
    ignore @@ Stack.pop state.stack;
    let trapSp = int_of_value (Stack.pop state.stack) in
    ignore @@ Stack.pop state.stack;
    ignore @@ Stack.pop state.stack;
    interp_loop level bytecode { next with trapSp = trapSp }
  | C_CALL1 idx ->
    let acc = Dummy in
    interp_loop level bytecode { next with acc = acc}
  | C_CALL2 idx ->
    ignore @@ Stack.pop state.stack;
    interp_loop level bytecode next
  | C_CALL3 idx ->
    ignore @@ Stack.pop state.stack;
    ignore @@ Stack.pop state.stack;
    interp_loop level bytecode next
  | C_CALL4 idx ->
    ignore @@ Stack.pop state.stack;
    ignore @@ Stack.pop state.stack;
    ignore @@ Stack.pop state.stack;
    interp_loop level bytecode next
  | C_CALL5 idx ->
    ignore @@ Stack.pop state.stack;
    ignore @@ Stack.pop state.stack;
    ignore @@ Stack.pop state.stack;
    ignore @@ Stack.pop state.stack;
    interp_loop level bytecode next
  | C_CALLN (narg, idx)       ->
    Stack.push state.stack state.acc;
    Stack.popn state.stack narg;
    interp_loop level bytecode next
  | RAISE | RERAISE | RAISE_NOTRACE -> raise Exit
  | CHECK_SIGNALS             ->
    interp_loop level bytecode next
  | CONST0                    -> interp_loop level bytecode { next with acc = Int 0 }
  | CONST1                    -> interp_loop level bytecode { next with acc = Int 1 }
  | CONST2                    -> interp_loop level bytecode { next with acc = Int 2 }
  | CONST3                    -> interp_loop level bytecode { next with acc = Int 3 }
  | CONSTINT n                -> interp_loop level bytecode { next with acc = Int n }
  | PUSHCONST0                ->
    Stack.push state.stack state.acc;
    interp_loop level bytecode { next with acc = Int 0 }
  | PUSHCONST1                ->
    Stack.push state.stack state.acc;
    interp_loop level bytecode { next with acc = Int 1 }
  | PUSHCONST2                ->
    Stack.push state.stack state.acc;
    interp_loop level bytecode { next with acc = Int 2 }
  | PUSHCONST3                ->
    Stack.push state.stack state.acc;
    interp_loop level bytecode { next with acc = Int 3 }
  | PUSHCONSTINT n            ->
    Stack.push state.stack state.acc;
    interp_loop level bytecode { next with acc = Int n }
  | NEGINT                    ->
    let acc = int_op (fun x y -> -x ) state.acc (Int 0) in
    interp_loop level bytecode { next with acc = acc }
  | ADDINT                    ->
    let v = (Stack.pop state.stack) in
    let acc = int_op (fun x y -> x+y ) state.acc v in
    interp_loop level bytecode { next with acc }
  | SUBINT                    ->
    let v = (Stack.pop state.stack) in
    let acc = int_op (fun x y -> x-y ) state.acc v in
    interp_loop level bytecode { next with acc }
  | MULINT                    ->
    let v = (Stack.pop state.stack) in
    let acc = int_op (fun x y -> x*y ) state.acc v in
    interp_loop level bytecode { next with acc }
  | DIVINT                    ->
    let v = (Stack.pop state.stack) in
    let acc = int_op (fun x y -> x/y ) state.acc v in
    interp_loop level bytecode { next with acc }
  | MODINT                    ->
    let v = (Stack.pop state.stack) in
    let acc = int_op (fun x y -> x mod y ) state.acc v in
    interp_loop level bytecode { next with acc }
  | ANDINT                    ->
     let v = (Stack.pop state.stack) in
    let acc = int_op (fun x y -> x land y ) state.acc v in
    interp_loop level bytecode { next with acc }
  | ORINT                     ->
    let v = (Stack.pop state.stack) in
    let acc = int_op (fun x y -> x lor y ) state.acc v in
    interp_loop level bytecode { next with acc }
  | XORINT                    ->
      let v = (Stack.pop state.stack) in
      let acc = int_op (fun x y -> x lxor y ) state.acc v in
      interp_loop level bytecode { next with acc }
  | LSLINT                    ->
    let v = (Stack.pop state.stack) in
    let acc = int_op (fun x y -> x lsl y ) state.acc v in
    interp_loop level bytecode { next with acc }
  | LSRINT                    ->
    let v = (Stack.pop state.stack) in
    let acc = int_op (fun x y -> x lsr y ) state.acc v in
    interp_loop level bytecode { next with acc }
  | ASRINT                    ->
    let v = (Stack.pop state.stack) in
    let acc = int_op (fun x y -> x asr y ) state.acc v in
    interp_loop level bytecode { next with acc }
  | EQ                        ->
    let v = (Stack.pop state.stack) in
    let acc = int_op (fun x y -> if x = y then 1 else 0 ) state.acc v in
    interp_loop level bytecode { next with acc }
  | NEQ ->
      let v = (Stack.pop state.stack) in
      let acc = int_op (fun x y -> if x <> y then 1 else 0 ) state.acc v in
      interp_loop level bytecode { next with acc }
  | LTINT                     ->
    let v = (Stack.pop state.stack) in
    let acc = int_op (fun x y -> if x < y then 1 else 0) state.acc v in
    interp_loop level bytecode { next with acc }
  | LEINT                     ->
    let v = (Stack.pop state.stack) in
    let acc = int_op (fun x y -> if x <= y then 1 else 0 ) state.acc v in
    interp_loop level bytecode { next with acc }
  | GTINT                     ->
    let v = (Stack.pop state.stack) in
    let acc = int_op (fun x y -> if x > y then 1 else 0 ) state.acc v in
    interp_loop level bytecode { next with acc }
  | GEINT                     ->
    let v = (Stack.pop state.stack) in
    let acc = int_op (fun x y -> if x >= y then 1 else 0) state.acc v in
    interp_loop level bytecode { next with acc }
  | OFFSETINT n               ->
    let acc = int_op (fun x y -> x + y ) state.acc (Int n) in
    interp_loop level bytecode { next with acc }
  | OFFSETREF n               ->
    begin
      match state.acc with
      | Block (tag,t) -> t.(0) <- int_op (fun x y -> x + y) t.(0) (Int n)
      | _ -> failwith "not a block"
    end;
    interp_loop level bytecode next
  | ISINT                     ->
    let acc = match state.acc with
      | Int i -> Int 1
      | _ -> Int 0
    in
    interp_loop level bytecode { next with acc }
  | GETMETHOD                 ->
    let acc = Dummy in
    interp_loop level bytecode { next with acc }
  | BEQ (n, ptr)              ->
       begin
      match state.acc with
      | Dummy ->
        interp_loop (level+1) bytecode { next with pc = ptr };
        interp_loop level bytecode next
      | Int v -> if n = v then
          interp_loop (level+1) bytecode { next with pc = ptr }
        else
          interp_loop level bytecode next
      | _ -> failwith "wrong accumulator"
    end
  | BNEQ (n, ptr)             ->
   begin
      match state.acc with
      | Dummy ->
        interp_loop (level+1) bytecode { next with pc = ptr };
        interp_loop level bytecode next
      | Int v -> if n <> v then
          interp_loop (level+1) bytecode { next with pc = ptr }
        else
          interp_loop level bytecode next
      | _ -> failwith "wrong accumulator"
    end
  | BLTINT (n, ptr)           ->
       begin
      match state.acc with
      | Dummy ->
        interp_loop (level+1) bytecode { next with pc = ptr };
        interp_loop level bytecode next
      | Int v -> if n < v then
          interp_loop (level+1) bytecode { next with pc = ptr }
        else
          interp_loop level bytecode next
      | _ -> failwith "wrong accumulator"
    end
  | BLEINT (n, ptr)           ->
     begin
      match state.acc with
      | Dummy ->
        interp_loop (level+1) bytecode { next with pc = ptr };
        interp_loop level bytecode next
      | Int v -> if n <= v then
          interp_loop (level+1) bytecode { next with pc = ptr }
        else
          interp_loop level bytecode next
      | _ -> failwith "wrong accumulator"
    end
  | BGTINT (n, ptr)           ->
    begin
      match state.acc with
      | Dummy ->
        interp_loop (level+1) bytecode { next with pc = ptr };
        interp_loop level bytecode next
      | Int v -> if n > v then
          interp_loop (level+1) bytecode { next with pc = ptr }
        else
          interp_loop level bytecode next
      | _ -> failwith "wrong accumulator"
    end
  | BGEINT (n, ptr) ->
        begin
      match state.acc with
      | Dummy ->
        interp_loop (level+1) bytecode { next with pc = ptr };
        interp_loop level bytecode next
      | Int v -> if n >= v then
          interp_loop (level+1) bytecode { next with pc = ptr }
        else
          interp_loop level bytecode next
      | _ -> failwith "wrong accumulator"
    end
  | ULTINT ->
    let n = int_op (fun x y -> x + min_int) state.acc (Int 0) in
    let p = int_op (fun x y -> x + min_int) (Stack.pop state.stack) (Int 0) in
    let acc = int_op (fun x y -> if x < y then 1 else 0) n p in
    interp_loop level bytecode { next with acc }
  | UGEINT ->
    let n = int_op (fun x y -> x + min_int) state.acc (Int 0) in
    let p = int_op (fun x y -> x + min_int) (Stack.pop state.stack) (Int 0) in
    let acc = int_op (fun x y -> if x >= y then 1 else 0) n p in
    interp_loop level bytecode { next with acc }
  (* | BULTINT (n,ptr) -> *)
  (*   interp_loop level bytecode { next with pc = ptr }; *)
  (*   interp_loop level bytecode next *)
  (* | BUGEINT (n, ptr) -> *)
  (*   interp_loop level bytecode { next with pc = ptr }; *)
  (*   interp_loop level bytecode next *)
  | GETPUBMET (tag, cache)    -> failwith "todo getpubmet"
  | GETDYNMET                 -> failwith "todo getdynmet"
  | STOP                      -> ()
  | EVENT                     -> failwith "todo event"
  | BREAK                     -> failwith "todo break"
  | _ -> failwith "unknown instr"



let nb_cycles inst = 1

let rec print_value_list l =
  match l with
  | [] -> ()
  | (h,ll)::t ->
    print_int h;
    print_string "-> [";
    List.iter (fun x -> print_string ((string_of_value x)^",")) ll;
    print_endline "]";

    print_value_list t

let rec value_of_obytelib v =
  match v with
  | OByteLib.Value.Int x -> Int x
  | OByteLib.Value.Int32 x -> Int (Int32.to_int x)
  | OByteLib.Value.Int64 x -> Int (Int64.to_int x)
  | OByteLib.Value.Nativeint x -> Int (Nativeint.to_int x)
  | OByteLib.Value.Block (tag,tab) -> Block (tag, Array.map value_of_obytelib tab)
  | OByteLib.Value.Float f -> Float f
  | OByteLib.Value.String s -> String s
  | OByteLib.Value.Object e -> Object (Array.map value_of_obytelib e)
  | _ -> failwith "not accepted"



let () =
  if Array.length Sys.argv -1 = 0 then
    failwith ("usage : ./bytecrawler file.byte")
  ;
  let bytefile = Bytefile.read Sys.argv.(1) in
  let data = bytefile.Bytefile.data in
  let symb = bytefile.Bytefile.symb in
  let primitives = bytefile.Bytefile.prim in
  let bytecode = bytefile.Bytefile.code in
  Printf.printf "\nGLOBAL =\n";
  Array.iteri (fun i s -> Format.printf "\t %d : %s\n" i (Value.to_string s)) data;
  OByteLib.Code.print data symb primitives stdout  bytecode;
  let global = Array.map value_of_obytelib data in
  Array.iteri (fun i s -> Format.printf "\t %d : %s\n" i (string_of_value s)) global;
  interp_loop 0 bytecode (new_state global)
