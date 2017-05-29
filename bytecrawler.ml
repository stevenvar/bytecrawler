open OByteLib
open OByteLib.Instr

type value = Ptr of int
           | Dummy
           | Value of int
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
    s.tab.(n) <- x

  let to_string s f =
    let str = ref "" in
    for i = s.size -1 downto 0  do
      let x = s.tab.(i) in
      str := !str^(f x)^" , ";
    done;
    str:= String.sub !str 0 (max 0 (String.length !str -3));
    "["^(!str)^"] <size="^(string_of_int s.size)^">"
end

type state = { mutable pc : int;
               mutable acc : value;
               stack : value Stack.t;
               mutable extraArgs : int ;
               mutable env : value;
               global : value array;
               mutable trapSp : int; }

let new_state () = { pc = 0;
                     acc = Dummy;
                     stack = Stack.create ();
                     extraArgs = 0;
                     env = Block (0,[| |]) ;
                     global = Array.make 100 Dummy;
                     trapSp = 0 }

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
  | Value v -> string_of_int v
  | Ptr x -> "@"^string_of_int x
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
    "<"^(string_of_value ptr)^s^">"

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
    state.pc
    (Instr.to_string t.(state.pc))

let rec ptr_of_value = function
  | Ptr x -> x
  | Closure (ptr,env) -> ptr_of_value ptr
  | Closure_rec (ptr,_,_,_) -> ptr_of_value ptr
  | _ -> failwith "not a ptr"

let int_of_value = function
  | Value v -> v
  | _ -> failwith "not a value"

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

exception Stop


let rec interp_loop level bytecode state : unit =
  let inst = bytecode.(state.pc) in
  let next = { state with pc = state.pc + 1 } in
  print_state state level bytecode;
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
    Stack.push state.stack (Value state.extraArgs);
    Stack.push state.stack state.env;
    Stack.push state.stack (Ptr ptr);
    interp_loop level bytecode next
  | APPLY n ->
    interp_loop (level+1) bytecode { next with pc = ptr_of_value state.acc ;
                                           env = state.acc ;
                                           extraArgs = n - 1 }
  | APPLY1 ->
    let arg = Stack.pop state.stack in
    Stack.push state.stack (Value state.extraArgs);
    Stack.push state.stack state.env;
    Stack.push state.stack (Ptr (state.pc + 1));
    Stack.push state.stack arg;
    interp_loop (level+1) bytecode { next with pc = ptr_of_value state.acc ;
                                           env = state.acc ;
                                           extraArgs = 0 }
  | APPLY2 ->
    let arg1 = Stack.pop state.stack in
    let arg2 = Stack.pop state.stack in
    Stack.push state.stack (Value state.extraArgs);
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
    Stack.push state.stack (Value state.extraArgs);
    Stack.push state.stack state.env;
    Stack.push state.stack (Ptr (state.pc + 1) );
    Stack.push state.stack arg3;
    Stack.push state.stack arg2;
    Stack.push state.stack arg1;
    interp_loop level bytecode { next with pc = ptr_of_value state.acc ;
                                           env = state.acc ;
                                           extraArgs = 2 }
  | APPTERM (n, s)  ->
    Stack.popn state.stack (s - n);
    let extraArgs = state.extraArgs + (n - 1) in
    let env = state.acc in
    interp_loop level bytecode { next with pc = ptr_of_value state.acc ;
                                           env = env ;
                                           extraArgs = extraArgs }
  | APPTERM1 s ->
    Stack.popn state.stack (s - 1);
    let env = state.acc in
    interp_loop (level-1) bytecode { next with pc = ptr_of_value state.acc ;
                                           env = env}
  | APPTERM2 s ->
    Stack.popn state.stack (s - 2);
    let env = state.acc in
    let extraArgs = state.extraArgs + 1 in
    interp_loop (level-1) bytecode { next with pc = ptr_of_value state.acc;
                                           env = env ; extraArgs =
                                                         extraArgs }
  | APPTERM3 s ->
    Stack.popn state.stack (s - 3);
    let extraArgs = state.extraArgs + 2 in
    let env = state.acc in
    interp_loop level bytecode { next with pc = ptr_of_value state.acc ;
                                           env = env ;
                                           extraArgs = extraArgs }
  | RETURN n -> Stack.popn state.stack n;
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
    let n = size_of_value state.env - 2 in
    for i = n downto 1 do
      Stack.push state.stack (field state.env i)
    done;
    let extraArgs = state.extraArgs + n in
    let env =  field state.env 0 in
    interp_loop level bytecode { next with env ; extraArgs}
  | GRAB n ->
    if state.extraArgs >= n then
      let extraArgs = state.extraArgs - n in
      interp_loop level bytecode { next with extraArgs = extraArgs }
    else
      let a = Array.make (state.extraArgs + 2) (state.acc) in
      (* for i = 1 to state.extraArgs + 1 do *)
      (*   a.(i) <- Stack.pop state.stack *)
      (* done; *)
      let acc = Closure (Ptr (state.pc - 3) , a) in
      set_field state.acc 1 state.env;
      for i = 0 to state.extraArgs do
        set_field state.acc (i+2) (Stack.pop state.stack)
      done;
      let sp = state.trapSp + state.extraArgs + 1 in
      let pc = int_of_value (Stack.pop state.stack) in
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
      interp_loop (level+1) bytecode { next with pc = ptr };
      interp_loop level bytecode next
  | BRANCHIFNOT ptr           ->
      interp_loop (level+1) bytecode { next with pc = ptr };
      interp_loop level bytecode next
  | SWITCH (n, ptrs)          ->
    (* Probleme ici  *)
    (* if is_int !accu then pc := ptrs.((obj !accu : int)) *)
    (* else pc := ptrs.(tag !accu + (n land 0xFFFF)) *)
    failwith "no match allowed"
  | BOOLNOT                   ->
    let acc = match state.acc with
      | Value 0 -> Value 1
      | Value 1 -> Value 0
      | _ -> failwith "not a bool"
    in
    interp_loop level bytecode { next with acc }
  | PUSHTRAP ptr              ->
    Stack.push state.stack (Value state.extraArgs);
    Stack.push state.stack state.env;
    Stack.push state.stack (Value state.trapSp);
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
  | CONST0                    -> interp_loop level bytecode { next with acc = Value 0 }
  | CONST1                    -> interp_loop level bytecode { next with acc = Value 1 }
  | CONST2                    -> interp_loop level bytecode { next with acc = Value 2 }
  | CONST3                    -> interp_loop level bytecode { next with acc = Value 3 }
  | CONSTINT n                -> interp_loop level bytecode { next with acc = Value n }
  | PUSHCONST0                ->
    Stack.push state.stack state.acc;
    interp_loop level bytecode { next with acc = Value 0 }
  | PUSHCONST1                ->
    Stack.push state.stack state.acc;
    interp_loop level bytecode { next with acc = Value 1 }
  | PUSHCONST2                ->
    Stack.push state.stack state.acc;
    interp_loop level bytecode { next with acc = Value 2 }
  | PUSHCONST3                ->
    Stack.push state.stack state.acc;
    interp_loop level bytecode { next with acc = Value 3 }
  | PUSHCONSTINT n            ->
    Stack.push state.stack state.acc;
    interp_loop level bytecode { next with acc = Value n }
  | NEGINT                    ->
    let acc = Dummy in
    interp_loop level bytecode { next with acc = acc }
  | ADDINT                    ->
    let v = int_of_value (Stack.pop state.stack) in
    let acc = int_of_value state.acc in
    interp_loop level bytecode { next with acc = Value (acc + v) }
  | SUBINT                    ->
    ignore (Stack.pop state.stack);
    let acc = Dummy in
    interp_loop level bytecode { next with acc }
  | MULINT                    ->
    let v = int_of_value (Stack.pop state.stack) in
    let acc = int_of_value state.acc in
    interp_loop level bytecode { next with acc = Value (acc * v) }
  | DIVINT                    ->
    let v = int_of_value (Stack.pop state.stack) in
    let acc = int_of_value state.acc in
    interp_loop level bytecode { next with acc = Value (acc / v) }
  | MODINT                    ->
    let v = int_of_value (Stack.pop state.stack) in
    let acc = int_of_value state.acc in
    interp_loop level bytecode { next with acc = Value (acc mod v) }
  | ANDINT                    ->
    let v = int_of_value (Stack.pop state.stack) in
    let acc = int_of_value state.acc in
    interp_loop level bytecode { next with acc = Value (acc land v) }
  | ORINT                     ->
    let v = int_of_value (Stack.pop state.stack) in
    let acc = int_of_value state.acc in
    interp_loop level bytecode { next with acc = Value (acc lor v) }
  | XORINT                    ->
    let v = int_of_value (Stack.pop state.stack) in
    let acc = int_of_value state.acc in
    interp_loop level bytecode { next with acc = Value (acc lxor v) }
  | LSLINT                    ->
    let v = int_of_value (Stack.pop state.stack) in
    let acc = int_of_value state.acc in
    interp_loop level bytecode { next with acc = Value (acc lsl v) }
  | LSRINT                    ->
    let v = int_of_value (Stack.pop state.stack) in
    let acc = int_of_value state.acc in
    interp_loop level bytecode { next with acc = Value (acc lsr v) }
  | ASRINT                    ->
    let v = int_of_value (Stack.pop state.stack) in
    let acc = int_of_value state.acc in
    interp_loop level bytecode { next with acc = Value (acc asr v) }
  | EQ                        ->
    let v = Stack.pop state.stack in
    interp_loop level bytecode { next with acc = if (state.acc = v) then
                                               Value 1 else
                                               Value 0 }
  | NEQ ->
    let v = Stack.pop state.stack in
    interp_loop level bytecode { next with acc = if (state.acc <> v) then Value 1 else Value 0 }
  | LTINT                     ->
    let v = int_of_value (Stack.pop state.stack) in
    let acc = int_of_value state.acc in
    interp_loop level bytecode { next with acc = if (acc < v) then Value 1 else Value 0 }
  | LEINT                     ->
    let v = int_of_value (Stack.pop state.stack) in
    let acc = int_of_value state.acc in
    interp_loop level bytecode { next with acc = if (acc <= v) then Value 1 else Value 0}
  | GTINT                     ->
    let v = int_of_value (Stack.pop state.stack) in
    let acc = int_of_value state.acc in
    interp_loop level bytecode { next with acc = if (acc > v) then Value 1 else Value 0}
  | GEINT                     ->
    let v = int_of_value (Stack.pop state.stack) in
    let acc = int_of_value state.acc in
    interp_loop level bytecode { next with acc = if (acc >= v) then Value 1 else Value 0}
  | OFFSETINT n               ->
    let acc = Value (int_of_value state.acc + n) in
    interp_loop level bytecode { next with acc }
  | OFFSETREF n               ->
    let acc = Dummy in
    interp_loop level bytecode { next with acc }
  | ISINT                     ->
    let acc = match state.acc with
      | Value i -> Value 1
      | _ -> Value 0
    in
    interp_loop level bytecode { next with acc }
  | GETMETHOD                 ->
    let acc = Dummy in
    interp_loop level bytecode { next with acc }
  | BEQ (n, ptr)              ->
    let pc = if n = (int_of_value state.acc) then ptr else next .pc in
    interp_loop level bytecode { next with pc }
  | BNEQ (n, ptr)             ->
    let pc = if n <> (int_of_value state.acc) then ptr else next.pc in
    interp_loop level bytecode { next with pc }
  | BLTINT (n, ptr)           ->
    let pc = if n < int_of_value (state.acc) then
        ptr else next.pc
    in
    interp_loop level bytecode { next with pc }
  | BLEINT (n, ptr)           ->
    let acc = int_of_value state.acc in
    let pc = if n <= acc then ptr else state.pc + 1 in
    interp_loop level bytecode { next with pc = pc }
  | BGTINT (n, ptr)           ->
    let acc = int_of_value state.acc in
    let pc = if n > acc then ptr else state.pc + 1 in
    interp_loop level bytecode { next with pc = pc }
  | BGEINT (n, ptr) ->
    let acc = Dummy in
    interp_loop (level+1) bytecode { next with pc = ptr };
    interp_loop level bytecode next
  | ULTINT
  | UGEINT -> let acc = Dummy in
    interp_loop level bytecode { next with acc = acc  }
  | BULTINT (n,ptr) ->
    interp_loop level bytecode { next with pc = ptr };
    interp_loop level bytecode next
  | BUGEINT (n, ptr) ->
    interp_loop level bytecode { next with pc = ptr };
    interp_loop level bytecode next
  | GETPUBMET (tag, cache)    ->
    Stack.push state.stack state.acc;
    let acc = Dummy in
    interp_loop level bytecode { next with acc = acc }
  | GETDYNMET                 ->
    let acc = Dummy in
    interp_loop level bytecode { next with acc = acc }
  | STOP                      -> ()
  | EVENT                     -> failwith "todo event"
  | BREAK                     -> failwith "todo break"
  | _ -> failwith "unknown inst"

let interp_inst inst state =
  let next = { state with pc = state.pc + 1 } in
  match inst with
  | ACC0 -> let i = Stack.peek state.stack 0 in
    { next with acc = i }
  | ACC1 -> let i = Stack.peek state.stack 1 in
    { next with acc = i }
  | ACC2 -> let i = Stack.peek state.stack 2 in
    { next with acc = i }
  | ACC3 -> let i = Stack.peek state.stack 3 in
    { next with acc = i }
  | ACC4 -> let i = Stack.peek state.stack 4 in
    { next with acc = i }
  | ACC5 -> let i = Stack.peek state.stack 5 in
    { next with acc = i }
  | ACC6 -> let i = Stack.peek state.stack 6 in
    { next with acc = i }
  | ACC7 -> let i = Stack.peek state.stack 7 in
    { next with acc = i }
  | ACC n -> let i = Stack.peek state.stack n in
    { next with acc = i }
  | PUSH -> Stack.push state.stack state.acc; next
  | PUSHACC0 ->
    Stack.push state.stack state.acc;
    let i = Stack.peek state.stack 0 in
    { next with acc = i }
  | PUSHACC1 ->
    Stack.push state.stack state.acc;
    let i = Stack.peek state.stack 1 in
    { next with acc = i }
  | PUSHACC2 ->
    Stack.push state.stack state.acc;
    let i = Stack.peek state.stack 2 in
    { next with acc = i }
  | PUSHACC3 ->
    Stack.push state.stack state.acc;
    let i = Stack.peek state.stack 3 in
    { next with acc = i }
  | PUSHACC4 ->
    Stack.push state.stack state.acc;
    let i = Stack.peek state.stack 4 in
    { next with acc = i }
  | PUSHACC5 ->
    Stack.push state.stack state.acc;
    let i = Stack.peek state.stack 5 in
    { next with acc = i }
  | PUSHACC6 ->
    Stack.push state.stack state.acc;
    let i = Stack.peek state.stack 6 in
    { next with acc = i }
  | PUSHACC7 ->
    Stack.push state.stack state.acc;
    let i = Stack.peek state.stack 7 in
    { next with acc = i }
  | PUSHACC n ->
    Stack.push state.stack state.acc;
    let i = Stack.peek state.stack n in
    { next with acc = i }
  | POP n -> Stack.popn state.stack n; next
  | ASSIGN n -> Stack.set state.stack n state.acc;
    { next with acc = Dummy }
  | ENVACC1                   ->
    { next with acc = field state.env 1 }
  | ENVACC2                   ->
    { next with acc = field state.env 2 }
  | ENVACC3                   ->
    { next with acc = field state.env 3 }
  | ENVACC4                   ->
    { next with acc = field state.env 4 }
  | ENVACC n                   ->
    { next with acc = field state.env n }
  | PUSHENVACC1               ->
    Stack.push state.stack state.acc;
    let acc = field state.env 1 in
    { next with acc = acc }
  | PUSHENVACC2               ->
    Stack.push state.stack state.acc;
    { next with acc = field state.env 2 }
  | PUSHENVACC3               ->
    Stack.push state.stack state.acc;
    { next with acc = field state.env 3 }
  | PUSHENVACC4               ->
    Stack.push state.stack state.acc;
    { next with acc = field state.env 4 }
  | PUSHENVACC n               ->
    Stack.push state.stack state.acc;
    { next with acc = field state.env n }
  | PUSH_RETADDR ptr ->
    Stack.push state.stack (Value state.extraArgs);
    Stack.push state.stack state.env;
    Stack.push state.stack (Ptr ptr);
    next
  (* | APPLY n -> *)
  (* { next with pc = ptr_of_env state.acc ; env = state.acc ; extraArgs = n - 1 } *)
  | APPLY1 ->
    let arg = Stack.pop state.stack in
    Stack.push state.stack (Value state.extraArgs);
    Stack.push state.stack state.env;
    Stack.push state.stack (Ptr (state.pc + 1));
    Stack.push state.stack arg;
    { next with pc = ptr_of_value state.acc ;
                env = state.acc ;
                extraArgs = 0 }
  | APPLY2 ->
    let arg1 = Stack.pop state.stack in
    let arg2 = Stack.pop state.stack in
    Stack.push state.stack (Value state.extraArgs);
    Stack.push state.stack state.env;
    Stack.push state.stack (Ptr (state.pc + 1));
    Stack.push state.stack arg2;
    Stack.push state.stack arg1;
    { next with pc = ptr_of_value state.acc ;
                env = state.acc ;
                extraArgs = 1 }
  | APPLY3 ->
    let arg1 = Stack.pop state.stack in
    let arg2 = Stack.pop state.stack in
    let arg3 = Stack.pop state.stack in
    Stack.push state.stack (Value state.extraArgs);
    Stack.push state.stack state.env;
    Stack.push state.stack (Ptr (state.pc + 1) );
    Stack.push state.stack arg3;
    Stack.push state.stack arg2;
    Stack.push state.stack arg1;
    { next with pc = ptr_of_value state.acc ;
                env = state.acc ;
                extraArgs = 2 }
  | APPTERM (n, s)  ->
    (* for i = 0 to n - 1 do *)
    (*   Stack.set state.stack (s - i - 1) (Stack.peek state.stack (n - i - 1)); *)
    (* done; *)
    Stack.popn state.stack (s - n);
    let extraArgs = state.extraArgs + (n - 1) in
    let env = state.acc in
    { next with pc = ptr_of_value state.acc ;
                env = env ;
                extraArgs = extraArgs }
  | APPTERM1 s ->
    (* Stack.set state.stack (s - 1 ) (Stack.peek state.stack 0); *)
    Stack.popn state.stack (s - 1);
    let env = state.acc in
    { next with pc = ptr_of_value state.acc ;
                env = env}
  | APPTERM2 s ->
    (* Stack.set state.stack (s - 1) (Stack.peek state.stack 1); *)
    (* Stack.set state.stack (s - 2) (Stack.peek state.stack 0); *)
    Stack.popn state.stack (s - 2);
    (* Stack.push arg1 state.stack; *)
    (* Stack.push arg2 state.stack; *)
    let env = state.acc in
    let extraArgs = state.extraArgs + 1 in
    { next with pc = ptr_of_value state.acc;
                env = env ; extraArgs =
                              extraArgs }
  | APPTERM3 s ->
    (* Stack.set state.stack (s - 1) (Stack.peek state.stack 2); *)
    (* Stack.set state.stack (s - 2) (Stack.peek state.stack 1); *)
    (* Stack.set state.stack (s - 3) (Stack.peek state.stack 0); *)
    Stack.popn state.stack (s - 3);
    let extraArgs = state.extraArgs + 2 in
    let env = state.acc in
    { next with pc = ptr_of_value state.acc ;
                env = env ;
                extraArgs = extraArgs }
  | RETURN n -> Stack.popn state.stack n;
    if (state.extraArgs = 0) then
      (
        let pc = ptr_of_value (Stack.pop state.stack) in
        let env = Stack.pop state.stack in
        let extraArgs = int_of_value (Stack.pop state.stack) in
        { next with pc ; env ; extraArgs}
      )
    else (
      let pc = ptr_of_value state.acc in
      let env = state.acc in
      let extraArgs = state.extraArgs - 1 in
      { next with pc; env ; extraArgs}
    )
  (* | RESTART -> *)
  (*   let blk = env_of_closure state.env in *)
  (*   let n = Array.length blk - 1 in *)

  (*   for i = n downto 1 do Stack.push state.stack blk.(i) done; *)
  (*   let env = blk.(0) in *)
  (*   let extraArgs = state.extraArgs + n in *)
  (*   { next with env ; extraArgs} *)

  (* | GRAB n -> *)
  (*   if state.extraArgs >= n then *)
  (*     let extraArgs = state.extraArgs - n in *)
  (*     { next with extraArgs = extraArgs } *)
  (*   else *)
  (*     let a = Array.make (state.extraArgs + 2) (state.acc) in *)
  (*     for i = 1 to state.extraArgs + 1 do *)
  (*       a.(i) <- Stack.pop state.stack *)
  (*     done; *)
  (*     let acc = Value.Env (Value.Closure ( state.pc - 1 , a)) in *)
  (*     let pc = int_of_val (Stack.pop state.stack) in *)
  (*     let env = Stack.pop state.stack in *)
  (*     let extraArgs = int_of_val (Stack.pop state.stack) in *)
  (*     { next with acc = acc ; pc = pc ; env = env ; extraArgs = extraArgs } *)
  | CLOSURE (n,ptr) ->
    let a = Array.make n (state.acc) in
    for i = 1 to n - 1 do
      a.(i) <- Stack.pop state.stack;
    done;
    { next with acc = Closure(Ptr ptr,a) }
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
    {next with acc = acc }
  (* | OFFSETCLOSUREM2           -> *)
  (*   let acc = Value.Env (offsetclosure state.env (-2)) *)
  (*   in *)
  (*   { next with acc} *)
  (* | OFFSETCLOSURE0            -> *)
  (*   let acc = Value.Env (offsetclosure state.env (0)) *)
  (*   in *)
  (*   { next with acc} *)

  (* | OFFSETCLOSURE2            -> *)
  (*   let acc = Value.Env (offsetclosure state.env (2)) *)
  (*   in *)
  (*   { next with acc} *)
  (* | OFFSETCLOSURE n           -> *)
  (*   let acc = Value.Env (offsetclosure state.env n) *)
  (*   in *)
  (*   { next with acc} *)
  (* | PUSHOFFSETCLOSUREM2       -> *)
  (*   Stack.push state.stack state.acc; *)
  (*   let acc = Value.Env (offsetclosure state.env (-2)) *)
  (*   in *)
  (*   { next with acc} *)

  (* | PUSHOFFSETCLOSURE0        -> *)
  (*   Stack.push state.stack state.acc; *)
  (*   let acc = Value.Env (offsetclosure state.env 0) *)
  (*   in *)
  (*   { next with acc} *)

  (* | PUSHOFFSETCLOSURE2        -> *)
  (*   Stack.push state.stack state.acc; *)

  (*   let acc = Value.Env (offsetclosure state.env 2) *)
  (*   in { next with acc } *)

  (* | PUSHOFFSETCLOSURE n       -> *)
  (*   Stack.push state.stack state.acc; *)
  (*   let acc = Value.Env (offsetclosure state.env n) *)
  (*   in { next with acc } *)
  | GETGLOBAL n               ->
    let acc = state.global.(n) in
    { next with acc = acc }
  | PUSHGETGLOBAL n           ->
    Stack.push state.stack state.acc;
    let acc = state.global.(n) in
    { next with acc = acc }
  (* | GETGLOBALFIELD (n, p)     -> *)
  (*   let acc = match data.(n) with *)
  (*     | Value.Block (tag,tab) -> tab.(p) *)
  (*     | _ -> failwith "not a block" *)
  (*   in *)
  (*   { next with acc = acc } *)

  (* | PUSHGETGLOBALFIELD (n, p) -> *)
  (*   Stack.push state.stack state.acc; *)
  (*   let acc = match data.(n) with *)
  (*     | Value.Block (tag,tab) -> tab.(p) *)
  (*     | _ -> failwith "not a block" *)
  (*   in { *)
  (*     next with acc = acc } *)
  | SETGLOBAL n               ->
    state.global.(n) <- state.acc ;
    { next with acc = Dummy }
  | ATOM0                     ->
    let blk = Block (0, [||]) in
    {next with acc = blk }
  | ATOM tag                  ->
    let blk = Block (tag, [||]) in
    { next with acc = blk }
  | PUSHATOM0                 ->
    Stack.push state.stack state.acc;
    let blk = Block (0 , [||]) in
    { next with acc = blk }
  | PUSHATOM tag              ->
    Stack.push state.stack state.acc;
    let blk = Block (0, [||]) in
    { next with acc = blk }
  | MAKEBLOCK (tag, sz)       ->
    let a = Array.make sz Dummy in
    let blk = Block (tag, a) in
    a.(0) <- state.acc;
    for i = 1 to sz - 1 do
      a.(i) <- Stack.pop state.stack;
    done;
    { next with acc = blk }
  | MAKEBLOCK1 tag            ->
    let a = Array.make 1 (state.acc) in
    let blk = Block (tag, a) in
    { next with acc = blk }
  | MAKEBLOCK2 tag            ->
    let a = Array.make 2 (state.acc) in
    let blk = Block (tag, a) in
    a.(1) <- Stack.pop state.stack;
    { next with acc = blk }
  | MAKEBLOCK3 tag            ->
    let a = Array.make 3 (state.acc) in
    let blk = Block (tag, a) in
    a.(1) <- Stack.pop state.stack;
    a.(2) <- Stack.pop state.stack;
    { next with acc = blk }
      (*
  | MAKEFLOATBLOCK sz         -> bprintf buf "MAKEFLOATBLOCK %d" sz *)
  | GETFIELD0                 ->
    let acc = field state.acc 0 in
    { next with acc = acc }
  | GETFIELD1                 ->
    let acc = field state.acc 1 in
    { next with acc = acc }
  | GETFIELD2                 ->
    let acc = field state.acc 2 in
    { next with acc = acc }
  | GETFIELD3                 ->
    let acc = field state.acc 3 in
    { next with acc = acc }
  | GETFIELD n                ->
    let acc = field state.acc n in
    { next with acc = acc }
      (*
  | GETFLOATFIELD f buf "GETFLOATFIELD %d" n
(* *\)  | SETFIELD0                 -> *)
(*       (match state.acc with *)
(*        | Value.Block(tag,a) -> a.(0) <- Stack.pop state.stack *)
(*        | _ -> failwith "not a block"); *)
(*       { next with acc = Value.Int 0 } *)
(*     | SETFIELD1                 -> *)
(*       (match state.acc with *)
(*        | Value.Block(tag,a) -> a.(1) <- Stack.pop state.stack *)
(*        | _ -> failwith "not a block"); *)
(*       { next with acc = Value.Int 0 } *)
(*     | SETFIELD2                 -> *)
(*       (match state.acc with *)
(*        | Value.Block(tag,a) -> a.(2) <- Stack.pop state.stack *)
(*        | _ -> failwith "not a block"); *)
(*       { next with acc = Value.Int 0 } *)
(*     | SETFIELD3                 -> *)
(*       (match state.acc with *)
(*        | Value.Block(tag,a) -> a.(3) <- Stack.pop state.stack *)
(*        | _ -> failwith "not a block"); *)
(*       { next with acc = Value.Int 0 } *)
(*   | SETFIELD n                 -> *)
(*       (match state.acc with *)
(*         | Value.Block(tag,a) -> a.(n) <- Stack.pop state.stack *)
(*        | _ -> failwith "not a block"); *)

(*       { next with acc = Value.Int 0 } *)
*)
(*
  | SETFLOATFIELD n           -> bprintf buf "SETFLOATFIELD %d" n
  | VECTLENGTH                -> bprintf buf "VECTLENGTH"
  | GETVECTITEM               -> bprintf buf "GETVECTITEM"
  | SETVECTITEM               -> bprintf buf "SETVECTITEM"
  | GETSTRINGCHAR             -> bprintf buf "GETSTRINGCHAR"
  | SETSTRINGCHAR             -> bprintf buf "SETSTRINGCHAR"
*)
  | BRANCH b -> { next with pc = b }
  | BRANCHIF ptr              ->
    if state.acc <> Value 0 then
      { next with pc = ptr }
    else
      next
  | BRANCHIFNOT ptr           ->
    if state.acc = Value 0 then
      { next with pc = ptr }
    else
      next
  (* | SWITCH (n, ptrs)          -> *)
  (*   let pc = match state.acc with *)
  (*     | Value.Int i -> ptrs.(i) *)
  (*     | Value.Block (tag,a) ->  ptrs.(tag + (n land 0xFFFF)) *)
  (*     | _ -> failwith "block" *)
  (*   in *)
  (*   { next with pc = pc } *)
  | BOOLNOT                   ->
    let acc = match state.acc with
      | Value 0 -> Value 1
      | Value 1 -> Value 0
      | _ -> failwith "not a bool"
    in
    { next with acc }
  | PUSHTRAP ptr              ->
    Stack.push state.stack (Value state.extraArgs);
    Stack.push state.stack state.env;
    Stack.push state.stack (Value state.trapSp);
    Stack.push state.stack (Ptr ptr);
    { next with trapSp = Stack.length state.stack }
  | POPTRAP                   ->
    ignore @@ Stack.pop state.stack;
    let trapSp = int_of_value @@ Stack.pop state.stack in
    ignore @@ Stack.pop state.stack;
    ignore @@ Stack.pop state.stack;
    { next with trapSp = trapSp }
  | C_CALL1 idx ->
    next
  | C_CALL2 idx ->
    ignore @@ Stack.pop state.stack;
    next
  (* | raise | RERAISE | RAISE_NOTRACE -> raise Exit *)
                                   (*
  | RERAISE                   -> bprintf buf "RERAISE"
  | RAISE_NOTRACE             -> bprintf buf "RAISE_NOTRACE"
  | CHECK_SIGNALS             -> bprintf buf "CHECK_SIGNALS"
  *)
  (* | C_CALL1 idx               -> *)
  (*   let s = OByteLib.Prim.find_prim (prim.(idx)) in *)
  (*   let value = to_obj state.acc in *)
  (*   let acc = (Obj.obj s : Obj.t -> Obj.t) value in *)
  (*   { next with acc = of_obj acc } *)
  (* | C_CALL2 idx               -> *)
  (*   let s = OByteLib.Prim.find_prim (prim.(idx)) in *)
  (*   let value = to_obj state.acc in *)
  (*   let arg = to_obj (Stack.pop state.stack) in *)
  (*   let acc = (Obj.obj s : Obj.t -> Obj.t -> Obj.t) value arg in *)
  (*   { next with acc = of_obj acc } *)
  (*  let s = OByteLib.Prim.find_prim (prim.(idx)) in
      let value = int_of_val state.acc in
      let arg1 = int_of_val (Stack.pop state.stack) in
      let acc = (Obj.obj s : Obj.t -> Obj.t -> Obj.t) (Obj.repr value) (Obj.repr arg1)
      in
      { next with acc = Value.Int (Obj.obj acc : int); } *)
  (* *\) *)
  (* | C_CALL3 idx               -> *)
  (*     let s = OByteLib.Prim.find_prim (prim.(idx)) in *)
  (*     let value = to_obj state.acc in *)
  (*     let arg = to_obj (Stack.pop state.stack) in *)
  (*     let arg2 = to_obj (Stack.pop state.stack) in *)
  (*     let acc = (Obj.obj s : Obj.t -> Obj.t -> Obj.t -> Obj.t ) value arg arg2 in *)
  (*     { next with acc = of_obj acc } *)

  (* | C_CALL4 idx               -> *)
  (*   let s = OByteLib.Prim.find_prim (prim.(idx)) in *)
  (*   let value = to_obj state.acc in *)
  (*   let arg = to_obj (Stack.pop state.stack) in *)
  (*   let arg2 = to_obj (Stack.pop state.stack) in *)
  (*   let arg3 = to_obj (Stack.pop state.stack) in *)
  (* Printf.printf "value = %s , arg = %s , arg2 = %s , arg3 = %s \n %!"
     (Value.to_string (of_obj value))
     (Value.to_string (of_obj arg))
     (Value.to_string (of_obj arg2))
     (Value.to_string (of_obj arg3)) ; *)
  (* let  acc = (Obj.obj s : (Obj.t -> Obj.t -> Obj.t -> Obj.t -> Obj.t) ) value arg arg2 arg3 in *)
  (*       { next with acc = of_obj acc } *)

  (*     | C_CALL5 idx               -> *)
  (*       let s = OByteLib.Prim.find_prim (prim.(idx)) in *)
  (*       let env = Stack.pop state.stack in *)
  (*       let value = to_obj (Stack.pop state.stack) in *)
  (*       let arg = to_obj (Stack.pop state.stack) in *)
  (*       let arg2 = to_obj (Stack.pop state.stack) in *)
  (*       let arg3 = to_obj (Stack.pop state.stack) in *)
  (*       let arg4 = to_obj (Stack.pop state.stack) in *)
  (*       let acc = (Obj.obj s : (Obj.t -> Obj.t -> Obj.t -> Obj.t -> Obj.t -> Obj.t )) value arg arg2 arg3 arg4 in *)
  (*       { next with acc = of_obj acc ; env = env } *)
  (*     | C_CALLN (narg, idx)       -> failwith "todo ccalln" *)
  | CONST0                    -> { next with acc = Value 0 }
  | CONST1                    -> { next with acc = Value 1 }
  | CONST2                    -> { next with acc = Value 2 }
  | CONST3                    -> { next with acc = Value 3 }
  | CONSTINT n                -> { next with acc = Value n }
  | PUSHCONST0                ->
    Stack.push state.stack state.acc;
    { next with acc = Value 0 }
  | PUSHCONST1                ->
    Stack.push state.stack state.acc;
    { next with acc = Value 1 }
  | PUSHCONST2                ->
    Stack.push state.stack state.acc;
    { next with acc = Value 2 }
  | PUSHCONST3                ->
    Stack.push state.stack state.acc;
    { next with acc = Value 3 }
  | PUSHCONSTINT n            ->
    Stack.push state.stack state.acc;
    { next with acc = Value n }
  | NEGINT                    ->
    let acc = int_of_value state.acc in
    { next with acc = Value (-1 * acc) }
  | ADDINT                    ->
    let v = int_of_value (Stack.pop state.stack) in
    let acc = int_of_value state.acc in
    { next with acc = Value (acc + v) }
  | SUBINT                    ->
    let v = int_of_value (Stack.pop state.stack) in
    let acc = int_of_value state.acc in
    { next with acc = Value (acc - v) }
  | MULINT                    ->
    let v = int_of_value (Stack.pop state.stack) in
    let acc = int_of_value state.acc in
    { next with acc = Value (acc * v) }
  | DIVINT                    ->
    let v = int_of_value (Stack.pop state.stack) in
    let acc = int_of_value state.acc in
    { next with acc = Value (acc / v) }
  | MODINT                    ->
    let v = int_of_value (Stack.pop state.stack) in
    let acc = int_of_value state.acc in
    { next with acc = Value (acc mod v) }
  | ANDINT                    ->
    let v = int_of_value (Stack.pop state.stack) in
    let acc = int_of_value state.acc in
    { next with acc = Value (acc land v) }
  | ORINT                     ->
    let v = int_of_value (Stack.pop state.stack) in
    let acc = int_of_value state.acc in
    { next with acc = Value (acc lor v) }
  | XORINT                    ->
    let v = int_of_value (Stack.pop state.stack) in
    let acc = int_of_value state.acc in
    { next with acc = Value (acc lxor v) }
  | LSLINT                    ->
    let v = int_of_value (Stack.pop state.stack) in
    let acc = int_of_value state.acc in
    { next with acc = Value (acc lsl v) }
  | LSRINT                    ->
    let v = int_of_value (Stack.pop state.stack) in
    let acc = int_of_value state.acc in
    { next with acc = Value (acc lsr v) }
  | ASRINT                    ->
    let v = int_of_value (Stack.pop state.stack) in
    let acc = int_of_value state.acc in
    { next with acc = Value (acc asr v) }
  | EQ                        ->
    let v = Stack.pop state.stack in
    { next with acc = if (state.acc = v) then
                    Value 1 else
                    Value 0 }
  | NEQ ->
    let v = Stack.pop state.stack in
    { next with acc = if (state.acc <> v) then Value 1 else Value 0 }
  | LTINT                     ->
    let v = int_of_value (Stack.pop state.stack) in
    let acc = int_of_value state.acc in
    { next with acc = if (acc < v) then Value 1 else Value 0 }
  | LEINT                     ->
    let v = int_of_value (Stack.pop state.stack) in
    let acc = int_of_value state.acc in
    { next with acc = if (acc <= v) then Value 1 else Value 0}
  | GTINT                     ->
    let v = int_of_value (Stack.pop state.stack) in
    let acc = int_of_value state.acc in
    { next with acc = if (acc > v) then Value 1 else Value 0}
  | GEINT                     ->
    let v = int_of_value (Stack.pop state.stack) in
    let acc = int_of_value state.acc in
    { next with acc = if (acc >= v) then Value 1 else Value 0}
  | OFFSETINT n               ->
    let acc = Value (int_of_value state.acc + n) in
    { next with acc }
  (* | OFFSETREF n               -> bprintf buf "OFFSETREF %d" n *)
  | ISINT                     ->
    let acc = match state.acc with
      | Value i -> Value 1
      | _ -> Value 0
    in
    { next with acc }
   (*
  | GETMETHOD                 -> bprintf buf "GETMETHOD"
 *)
  | BEQ (n, ptr)              ->
    let pc = if n = (int_of_value state.acc) then ptr else next.pc in
    { next with pc }
  | BNEQ (n, ptr)             ->
    let pc = if n <> (int_of_value state.acc) then ptr else next.pc in
    { next with pc }
  | BLTINT (n, ptr)           ->
    let pc = if n < int_of_value (state.acc) then
        ptr else next.pc
    in
    { next with pc }
  | BLEINT (n, ptr)           ->
    let acc = int_of_value state.acc in
    let pc = if n <= acc then ptr else state.pc + 1 in
    { next with pc = pc }
  | BGTINT (n, ptr)           ->
    let acc = int_of_value state.acc in
    let pc = if n > acc then ptr else state.pc + 1 in
    { next with pc = pc }
  | BGEINT (n, ptr) ->
    let acc = int_of_value state.acc in
    let pc = if n>= acc then ptr else state.pc + 1 in
    { next with pc }
  (*    | ULTINT                    -> bprintf buf "ULTINT"
        | UGEINT                    -> bprintf buf "UGEINT"
        | BULTINT (n,² ptr)          -> bprintf buf "BULTINT %d %a" n pp_ptr ptr
        | BUGEINT (n, ptr)          -> bprintf buf "BUGEINT %d %a" n pp_ptr ptr
        | GETPUBMET (tag, cache)    -> bprintf buf "GETPUBMET %d %d" tag cache
        | GETDYNMET                 -> bprintf buf "GETDYNMET"
  *)
  | STOP                      -> raise Stop
  | EVENT                     -> failwith "todo event"
  | BREAK                     -> failwith "todo break"
  | _ -> failwith "unknown inst"

let rec exec bytecode =
  let state = new_state () in
  let rec loop s =
    try
      print_state s 0 bytecode;
      let s = interp_inst bytecode.(s.pc) s in
      loop s
    with
      Stop -> print_string "fini"
  in
  loop state

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

let () =
  if Array.length Sys.argv -1 = 0 then
    failwith ("usage : ./bytecrawler file.byte")
  ;
  let bytefile = Bytefile.read Sys.argv.(1) in
  let data = bytefile.Bytefile.data in
  let symb = bytefile.Bytefile.symb in
  let primitives = bytefile.Bytefile.prim in
  let bytecode = bytefile.Bytefile.code in
  OByteLib.Code.print data symb primitives stdout  bytecode;
  (* exec bytecode; *)
  interp_loop 0 bytecode (new_state ())
