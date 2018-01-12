open State
open OByteLib
open OByteLib.Instr

let nb_blocks = ref 0
let max_stack = ref 0
let max_heap = ref 0

let int_op f x y =
  match x,y with
  | Int x, Int y -> Int (f x y)
  | _ -> Dummy

let field blk n =
  match blk with
  | Block (tag,tab) -> tab.(n)
  | Closure (_,tab) -> tab.(n-1)
  | Closure_rec (_,t,blk,i) ->
    (* Format.printf "position : %d  \n" *)
      (* (n - (2 * Array.length t + 1 - 2 * 1)); *)
    (* blk.(n - (2 * Array.length t + 1 - 2 * 1)) *)
    blk.(n-1)
  | _ -> failwith "not an environment"

let set_field blk n x =
  match blk with
  | Block (_,tab) -> tab.(n) <- x
  | _ -> failwith "not an environment"

let offsetclosure blk n =
  match blk with
  | Closure_rec (o,t,blk,i) -> Closure_rec (o,t,blk,i + n/2)
  | _ -> failwith "not a recursive closure"

let interp_loop_0 level bytecode state prims =
  let rec interp_loop level state : unit =
  let inst = bytecode.(state.pc) in
  let next = { state with pc = state.pc + 1 } in
  let level = max level 0 in
  (* Printf.printf "heap_size = %d \n" (heap_size state); *)
  if Mlstack.length state.stack > !max_stack then max_stack := Mlstack.length state.stack;
  (* if State.heap_size state > !max_heap then max_heap := State.heap_size state; *)
  print_state state level bytecode;
  (* let _ = read_line () in *)
  match inst with
  | ACC0 -> let i = Mlstack.peek state.stack 0 in
    interp_loop level { next with acc = i }
  | ACC1 -> let i = Mlstack.peek state.stack 1 in
    interp_loop level { next with acc = i }
  | ACC2 -> let i = Mlstack.peek state.stack 2 in
    interp_loop level { next with acc = i }
  | ACC3 -> let i = Mlstack.peek state.stack 3 in
    interp_loop level { next with acc = i }
  | ACC4 -> let i = Mlstack.peek state.stack 4 in
    interp_loop level { next with acc = i }
  | ACC5 -> let i = Mlstack.peek state.stack 5 in
    interp_loop level { next with acc = i }
  | ACC6 -> let i = Mlstack.peek state.stack 6 in
    interp_loop level { next with acc = i }
  | ACC7 -> let i = Mlstack.peek state.stack 7 in
    interp_loop level { next with acc = i }
  | ACC n -> let i = Mlstack.peek state.stack n in
    interp_loop level { next with acc = i }
  | PUSH -> Mlstack.push state.stack state.acc;
    interp_loop level next
  | PUSHACC0 ->
    Mlstack.push state.stack state.acc;
    let i = Mlstack.peek state.stack 0 in
    interp_loop level { next with acc = i }
  | PUSHACC1 ->
    Mlstack.push state.stack state.acc;
    let i = Mlstack.peek state.stack 1 in
    interp_loop level { next with acc = i }
  | PUSHACC2 ->
    Mlstack.push state.stack state.acc;
    let i = Mlstack.peek state.stack 2 in
    interp_loop level { next with acc = i }
  | PUSHACC3 ->
    Mlstack.push state.stack state.acc;
    let i = Mlstack.peek state.stack 3 in
    interp_loop level { next with acc = i }
  | PUSHACC4 ->
    Mlstack.push state.stack state.acc;
    let i = Mlstack.peek state.stack 4 in
    interp_loop level { next with acc = i }
  | PUSHACC5 ->
    Mlstack.push state.stack state.acc;
    let i = Mlstack.peek state.stack 5 in
    interp_loop level { next with acc = i }
  | PUSHACC6 ->
    Mlstack.push state.stack state.acc;
    let i = Mlstack.peek state.stack 6 in
    interp_loop level { next with acc = i }
  | PUSHACC7 ->
    Mlstack.push state.stack state.acc;
    let i = Mlstack.peek state.stack 7 in
    interp_loop level { next with acc = i }
  | PUSHACC n ->
    Mlstack.push state.stack state.acc;
    let i = Mlstack.peek state.stack n in
    interp_loop level { next with acc = i }
  | POP n -> Mlstack.popn state.stack n;
    interp_loop level next
  | ASSIGN n -> Mlstack.set state.stack n state.acc;
    interp_loop level { next with acc = Dummy }
  | ENVACC1                   ->
    interp_loop level { next with acc = field state.env 1 }
  | ENVACC2                   ->
    interp_loop level { next with acc = field state.env 2 }
  | ENVACC3                   ->
    interp_loop level { next with acc = field state.env 3 }
  | ENVACC4                   ->
    interp_loop level { next with acc = field state.env 4 }
  | ENVACC n                   ->
    interp_loop level { next with acc = field state.env n }
  | PUSHENVACC1               ->
    Mlstack.push state.stack state.acc;
    let acc = field state.env 1 in
    interp_loop level { next with acc = acc }
  | PUSHENVACC2               ->
    Mlstack.push state.stack state.acc;
    interp_loop level { next with acc = field state.env 2 }
  | PUSHENVACC3               ->
    Mlstack.push state.stack state.acc;
    interp_loop level { next with acc = field state.env 3 }
  | PUSHENVACC4               ->
    Mlstack.push state.stack state.acc;
    interp_loop level { next with acc = field state.env 4 }
  | PUSHENVACC n               ->
    Mlstack.push state.stack state.acc;
    interp_loop level { next with acc = field state.env n }
  | PUSH_RETADDR ptr ->
    Mlstack.push state.stack (Int state.extraArgs);
    Mlstack.push state.stack state.env;
    Mlstack.push state.stack (Ptr ptr);
    interp_loop level next
  | APPLY n ->
    interp_loop (level+1) { next with pc = ptr_of_value state.acc ;
                                           env = state.acc ;
                                           extraArgs = n - 1 }
  | APPLY1 ->
    let arg = Mlstack.pop state.stack in
    Mlstack.push state.stack (Int state.extraArgs);
    Mlstack.push state.stack state.env;
    Mlstack.push state.stack (Ptr (state.pc + 1));
    Mlstack.push state.stack arg;
    interp_loop (level+1) { next with pc = ptr_of_value state.acc ;
                                           env = state.acc ;
                                           extraArgs = 0 }
  | APPLY2 ->
    let arg1 = Mlstack.pop state.stack in
    let arg2 = Mlstack.pop state.stack in
    Mlstack.push state.stack (Int state.extraArgs);
    Mlstack.push state.stack state.env;
    Mlstack.push state.stack (Ptr (state.pc + 1));
    Mlstack.push state.stack arg2;
    Mlstack.push state.stack arg1;
    interp_loop level { next with pc = ptr_of_value state.acc ;
                                           env = state.acc ;
                                           extraArgs = 1 }
  | APPLY3 ->
    let arg1 = Mlstack.pop state.stack in
    let arg2 = Mlstack.pop state.stack in
    let arg3 = Mlstack.pop state.stack in
    Mlstack.push state.stack (Int state.extraArgs);
    Mlstack.push state.stack state.env;
    Mlstack.push state.stack (Ptr (state.pc + 1) );
    Mlstack.push state.stack arg3;
    Mlstack.push state.stack arg2;
    Mlstack.push state.stack arg1;
    interp_loop level { next with pc = ptr_of_value state.acc ;
                                           env = state.acc ;
                                           extraArgs = 2 }
  | APPTERM (n, s)  ->
    for i = 0 to n - 1 do
      let arg = Mlstack.peek state.stack (n- i - 1) in
      Mlstack.set state.stack (s - i - 1) arg
    done;
    Mlstack.popn state.stack (s - n);
    let extraArgs = state.extraArgs + (n - 1) in
    let env = state.acc in
    interp_loop level { next with pc = ptr_of_value state.acc ;
                                           env = env ;
                                           extraArgs = extraArgs }
  | APPTERM1 n ->
    let arg = Mlstack.peek state.stack 0 in
    Mlstack.popn state.stack n;
    (* Format.printf "---> %d <<<---" (int_of_value arg); *)
    Mlstack.push state.stack arg;
    let env = state.acc in
    interp_loop level { next with pc = ptr_of_value state.acc ;
                                           env = env}
  | APPTERM2 n ->
    Format.printf "APPTERM2 %d" n;
    let arg1 = Mlstack.peek state.stack 0 in
    let arg2 = Mlstack.peek state.stack 1 in
    Mlstack.popn state.stack n;
    Mlstack.push state.stack arg2;
    Mlstack.push state.stack arg1;
    let env = state.acc in
    let extraArgs = state.extraArgs + 1 in
    interp_loop level { next with pc = ptr_of_value state.acc;
                                           env = env ; extraArgs =
                                                         extraArgs }
  | APPTERM3 s ->
     let arg1 = Mlstack.peek state.stack 0 in
     let arg2 = Mlstack.peek state.stack 1 in
     let arg3 = Mlstack.peek state.stack 2 in
     Mlstack.popn state.stack (s - 3);
     Mlstack.push state.stack arg3;
     Mlstack.push state.stack arg2;
     Mlstack.push state.stack arg1;
    let extraArgs = state.extraArgs + 2 in
    let env = state.acc in
    interp_loop level { next with pc = ptr_of_value state.acc ;
                                           env = env ;
                                           extraArgs = extraArgs }
  | RETURN n ->
    Mlstack.popn state.stack n;
    if (state.extraArgs = 0) then
      (
        let pc = ptr_of_value (Mlstack.pop state.stack) in
        let env = Mlstack.pop state.stack in
        let extraArgs = int_of_value (Mlstack.pop state.stack) in
        interp_loop (level-1) { next with pc ; env ; extraArgs}
      )
    else (
      let pc = ptr_of_value state.acc in
      let env = state.acc in
      let extraArgs = state.extraArgs - 1 in
      interp_loop level { next with pc; env ; extraArgs}
    )
  | RESTART ->
    let blk = env_of_closure state.env in
    let n = Array.length blk - 1 in
    Format.printf "--> NARGS = %d" n;
    for i = n downto 1 do
      Mlstack.push state.stack blk.(i)
    done;
    let extraArgs = state.extraArgs + n in
    let env = blk.(0) in
    interp_loop level { next with env ; extraArgs}
  | GRAB n ->
    if state.extraArgs >= n then
      let extraArgs = state.extraArgs - n in
      interp_loop level { next with extraArgs = extraArgs }
    else
      let a = Array.make (state.extraArgs + 2) (state.acc) in
      for i = 1 to state.extraArgs + 1 do
        a.(i) <- Mlstack.pop state.stack
      done;
      nb_blocks := !nb_blocks + state.extraArgs + 3;
      let acc = Closure (Ptr (state.pc - 1) , a) in
      (* set_field state.acc 1 state.env; *)
      (* for i = 0 to state.extraArgs do *)
      (*   set_field state.acc (i+2) (Mlstack.pop state.stack) *)
      (* done; *)
      let sp = state.trapSp + state.extraArgs + 1 in
      let pc = ptr_of_value (Mlstack.pop state.stack) in
      let env = Mlstack.pop state.stack in
      let extraArgs = int_of_value (Mlstack.pop state.stack) in
      interp_loop level { next with acc ; pc ; trapSp = sp;  env ; extraArgs }
  | CLOSURE (n,ptr) ->
    nb_blocks := !nb_blocks + n + 1;
    let a = Array.make n (state.acc) in
    for i = 1 to n - 1 do
      a.(i) <- Mlstack.pop state.stack;
    done;
    interp_loop level { next with acc = Closure(Ptr ptr,a) }
  | CLOSUREREC (f, v, ptr, t)   ->
    (* f = number of functions
     * v = number of variables *)
    (* if v > 0 then Mlstack.push state.stack state.acc; *)
    let blk = Array.make v (state.acc) in
    for i = 1 to v - 1 do
      blk.(i) <- Mlstack.pop state.stack;
    done;
    nb_blocks := !nb_blocks +  f * 2 - 1 + v;
    let acc = Closure_rec (Ptr ptr,t,blk,0) in
    Mlstack.push state.stack acc;
    for i = 1 to Array.length t do
      Mlstack.push state.stack (Closure_rec(Ptr ptr,t,blk,i))
    done;
    interp_loop level {next with acc = acc }
  | OFFSETCLOSUREM2           ->
    let acc = offsetclosure state.env (-2) in
    interp_loop level { next with acc}
  | OFFSETCLOSURE0            ->
    let acc = offsetclosure state.env 0 in
    interp_loop level { next with acc}
  | OFFSETCLOSURE2            ->
    let acc = offsetclosure state.env 2 in
    interp_loop level { next with acc}
  | OFFSETCLOSURE n           ->
    let acc = offsetclosure state.env n in
    interp_loop level { next with acc}
  | PUSHOFFSETCLOSUREM2       ->
    Mlstack.push state.stack state.acc;
    let acc = offsetclosure state.env (-2) in
    interp_loop level { next with acc}
  | PUSHOFFSETCLOSURE0        ->
    Mlstack.push state.stack state.acc;
    let acc = offsetclosure state.env 0 in
    interp_loop level { next with acc}
  | PUSHOFFSETCLOSURE2        ->
    Mlstack.push state.stack state.acc;
    let acc = offsetclosure state.env 2 in
    interp_loop level { next with acc }
  | PUSHOFFSETCLOSURE n       ->
    Mlstack.push state.stack state.acc;
    let acc = offsetclosure state.env n
    in interp_loop level { next with acc }
  | GETGLOBAL n               ->
    let acc = state.global.(n) in
    interp_loop level { next with acc = acc }
  | PUSHGETGLOBAL n           ->
    Mlstack.push state.stack state.acc;
    let acc = state.global.(n) in
    interp_loop level { next with acc = acc }
  | GETGLOBALFIELD (n, p)     ->
    let acc = field state.global.(n) p in
    interp_loop level { next with acc = acc }
  | PUSHGETGLOBALFIELD (n, p) ->
    Mlstack.push state.stack state.acc;
    let acc = field state.global.(n) p in
    interp_loop level { next with acc = acc }
  | SETGLOBAL n               ->
    state.global.(n) <- state.acc ;
    interp_loop level { next with acc = Dummy }
  | ATOM0                     ->
    let blk = Block (0, [||]) in
    interp_loop level {next with acc = blk }
  | ATOM tag                  ->
    let blk = Block (tag, [||]) in
    interp_loop level { next with acc = blk }
  | PUSHATOM0                 ->
    Mlstack.push state.stack state.acc;
    let blk = Block (0 , [||]) in
    interp_loop level { next with acc = blk }
  | PUSHATOM tag              ->
    Mlstack.push state.stack state.acc;
    let blk = Block (0, [||]) in
    interp_loop level { next with acc = blk }
  | MAKEBLOCK (tag, sz)       ->
    nb_blocks := !nb_blocks + sz;
    let a = Array.make sz Dummy in
    let blk = Block (tag, a) in
    a.(0) <- state.acc;
    for i = 1 to sz - 1 do
      a.(i) <- Mlstack.pop state.stack;
    done;
    interp_loop level { next with acc = blk }
  | MAKEBLOCK1 tag            ->
    nb_blocks := !nb_blocks + 1;
    let a = Array.make 1 (state.acc) in
    let blk = Block (tag, a) in
    interp_loop level { next with acc = blk }
  | MAKEBLOCK2 tag            ->
    nb_blocks := !nb_blocks + 2;
    let a = Array.make 2 (state.acc) in
    let blk = Block (tag, a) in
    a.(1) <- Mlstack.pop state.stack;
    interp_loop level { next with acc = blk }
  | MAKEBLOCK3 tag            ->
    nb_blocks := !nb_blocks + 3;
    let a = Array.make 3 (state.acc) in
    let blk = Block (tag, a) in
    a.(1) <- Mlstack.pop state.stack;
    a.(2) <- Mlstack.pop state.stack;
    interp_loop level { next with acc = blk }
  | MAKEFLOATBLOCK n         ->
    nb_blocks := !nb_blocks + n;
    let a = Array.make n Dummy in
    let blk = Block (Obj.double_array_tag,a) in
    a.(0) <- state.acc;
    Mlstack.popn state.stack (n-1);
    interp_loop level { next with acc = blk }
  | GETFIELD0                 ->
    let acc = field state.acc 0 in
    interp_loop level { next with acc = acc }
  | GETFIELD1                 ->
    let acc = field state.acc 1 in
    interp_loop level { next with acc = acc }
  | GETFIELD2                 ->
    let acc = field state.acc 2 in
    interp_loop level { next with acc = acc }
  | GETFIELD3                 ->
    let acc = field state.acc 3 in
    interp_loop level { next with acc = acc }
  | GETFIELD n                ->
    let acc = field state.acc n in
    interp_loop level { next with acc = acc }
  | GETFLOATFIELD i ->
    let acc = Dummy in
    interp_loop level { next with acc = acc }
  | SETFIELD0                 ->
    set_field state.acc 0 (Mlstack.pop state.stack);
    let acc = Dummy in
    interp_loop level { next with acc = acc }
  | SETFIELD1                 ->
    set_field state.acc 1 (Mlstack.pop state.stack);
    let acc = Dummy in
    interp_loop level { next with acc = acc }
  | SETFIELD2                 ->
    set_field state.acc 2 (Mlstack.pop state.stack);
    let acc = Dummy in
    interp_loop level { next with acc = acc }
  | SETFIELD3                 ->
    set_field state.acc 3 (Mlstack.pop state.stack);
    let acc = Dummy in
    interp_loop level { next with acc = acc }
  | SETFIELD n                 ->
    set_field state.acc n (Mlstack.pop state.stack);
    let acc = Dummy in
    interp_loop level { next with acc = acc }
  | SETFLOATFIELD n           ->
    let acc = Dummy in
    interp_loop level { next with acc = acc }
  | VECTLENGTH                ->
    let acc = Dummy in
    interp_loop level { next with acc = acc }
  | GETVECTITEM               ->
    ignore @@ Mlstack.pop state.stack;
    let acc = Dummy in
    interp_loop level { next with acc = acc }
  | SETVECTITEM               ->
    ignore @@ Mlstack.pop state.stack;
    ignore @@ Mlstack.pop state.stack;
    let acc = Dummy in
    interp_loop level { next with acc = acc }
  | GETSTRINGCHAR             ->
    ignore @@ Mlstack.pop state.stack;
    let acc = Dummy in
    interp_loop level { next with acc = acc }
  | SETSTRINGCHAR             ->
    ignore @@ Mlstack.pop state.stack;
    ignore @@ Mlstack.pop state.stack;
    let acc = Dummy in
    interp_loop level { next with acc = acc }
  | BRANCH b -> interp_loop level { next with pc = b }
  | BRANCHIF ptr              ->
     begin
      match state.acc with
        | Dummy ->
          begin
          try
            interp_loop (level+1) { next with pc = ptr };
          with _ -> ();
            try
              interp_loop level next
            with _ -> ()
        end
      | Int 0 -> interp_loop level next
      | _ -> interp_loop (level+1) { next with pc = ptr }
    end
  | BRANCHIFNOT ptr           ->
    begin
      match state.acc with
      | Dummy ->
        begin
          try
            interp_loop (level+1) { next with pc = ptr }
          with _ -> ();
            try
              interp_loop level next
            with _ -> ();
      end
      | Int 0 -> interp_loop (level+1) { next with pc = ptr }
      | _ -> interp_loop level next
      (* | _ -> failwith "wrong accumulator" *)
    end
  | SWITCH (n, ptrs)          ->
    (* Probleme ici  *)
    begin
    match state.acc with
      | Int v ->
        interp_loop level { next with pc = ptrs.(v) }
      | Block (tag,b) ->  interp_loop level { next with pc = ptrs.(tag + (n land 0xFFFF)) }
      | _ -> failwith "?"
  end
  | BOOLNOT                   ->
    let acc = match state.acc with
      | Int 0 -> Int 1
      | Int 1 -> Int 0
      | _ -> failwith "not a bool"
    in
    interp_loop level { next with acc }
  | PUSHTRAP ptr              ->
    Mlstack.push state.stack (Int state.extraArgs);
    Mlstack.push state.stack state.env;
    Mlstack.push state.stack (Int state.trapSp);
    Mlstack.push state.stack (Ptr ptr);
    interp_loop level { next with trapSp = Mlstack.length state.stack }
  | POPTRAP                   ->
    ignore @@ Mlstack.pop state.stack;
    let trapSp = int_of_value (Mlstack.pop state.stack) in
    ignore @@ Mlstack.pop state.stack;
    ignore @@ Mlstack.pop state.stack;
    interp_loop level { next with trapSp = trapSp }
  | C_CALL1 idx ->
    let acc = Dummy in
    if prims.(idx) = "begin_loop" then
      Printf.printf " >> BEGINNING LOOP << \n"
    else if prims.(idx) = "end_loop" then
      Printf.printf " >> ENDING LOOP << \n";
    interp_loop level { next with acc = acc}
  | C_CALL2 idx ->
    let peek =  Mlstack.peek state.stack in
    begin
      match state.acc, peek 0 with
        Float f, Float f2 ->
        Format.printf "acc = %f / peek 0 = %F, %b diff = %F \n" f f2 (f >= f2) (f2 -. f);
        let acc =
          begin
            match idx with
              1 -> Float (f *. f2)
            | 2 -> Int ( if f2 -. f < 0.0001 then 1 else 0 )
            | 3 -> Float (f +. f2)
            | 4 -> Float (f -. f2)
            | 5 -> Float (f /. f2)
            | _ ->
              Dummy
          end
        in
        ignore @@ Mlstack.pop state.stack;
        interp_loop level { next with acc }
      | _ -> ignore @@ Mlstack.pop state.stack;
        interp_loop level { next with acc = Dummy }
    end
  | C_CALL3 idx ->
    let acc = Dummy in
    ignore @@ Mlstack.pop state.stack;
    ignore @@ Mlstack.pop state.stack;
    interp_loop level { next with acc }
  | C_CALL4 idx ->
    let acc = Dummy in
    ignore @@ Mlstack.pop state.stack;
    ignore @@ Mlstack.pop state.stack;
    ignore @@ Mlstack.pop state.stack;
    interp_loop level { next with acc }
  | C_CALL5 idx ->
    let acc = Dummy in
    ignore @@ Mlstack.pop state.stack;
    ignore @@ Mlstack.pop state.stack;
    ignore @@ Mlstack.pop state.stack;
    ignore @@ Mlstack.pop state.stack;
    interp_loop level { next with acc }
  | C_CALLN (narg, idx)       ->
    let acc = Dummy in
    Mlstack.push state.stack state.acc;
    Mlstack.popn state.stack narg;
    interp_loop level { next with acc }
  | RAISE | RERAISE | RAISE_NOTRACE -> raise Exit
  | CHECK_SIGNALS             ->
    interp_loop level next
  | CONST0                    -> interp_loop level { next with acc = Int 0 }
  | CONST1                    -> interp_loop level { next with acc = Int 1 }
  | CONST2                    -> interp_loop level { next with acc = Int 2 }
  | CONST3                    -> interp_loop level { next with acc = Int 3 }
  | CONSTINT n                -> interp_loop level { next with acc = Int n }
  | PUSHCONST0                ->
    Mlstack.push state.stack state.acc;
    interp_loop level { next with acc = Int 0 }
  | PUSHCONST1                ->
    Mlstack.push state.stack state.acc;
    interp_loop level { next with acc = Int 1 }
  | PUSHCONST2                ->
    Mlstack.push state.stack state.acc;
    interp_loop level { next with acc = Int 2 }
  | PUSHCONST3                ->
    Mlstack.push state.stack state.acc;
    interp_loop level { next with acc = Int 3 }
  | PUSHCONSTINT n            ->
    Mlstack.push state.stack state.acc;
    interp_loop level { next with acc = Int n }
  | NEGINT                    ->
    let acc = int_op (fun x y -> -x ) state.acc (Int 0) in
    interp_loop level { next with acc = acc }
  | ADDINT                    ->
    let v = (Mlstack.pop state.stack) in
    let acc = int_op (fun x y -> x+y ) state.acc v in
    interp_loop level { next with acc }
  | SUBINT                    ->
    let v = (Mlstack.pop state.stack) in
    let acc = int_op (fun x y -> x-y ) state.acc v in
    interp_loop level { next with acc }
  | MULINT                    ->
    let v = (Mlstack.pop state.stack) in
    let acc = int_op (fun x y -> x*y ) state.acc v in
    interp_loop level { next with acc }
  | DIVINT                    ->
    let v = (Mlstack.pop state.stack) in
    let acc = int_op (fun x y -> x/y ) state.acc v in
    interp_loop level { next with acc }
  | MODINT                    ->
    let v = (Mlstack.pop state.stack) in
    let acc = int_op (fun x y -> x mod y ) state.acc v in
    interp_loop level { next with acc }
  | ANDINT                    ->
     let v = (Mlstack.pop state.stack) in
    let acc = int_op (fun x y -> x land y ) state.acc v in
    interp_loop level { next with acc }
  | ORINT                     ->
    let v = (Mlstack.pop state.stack) in
    let acc = int_op (fun x y -> x lor y ) state.acc v in
    interp_loop level { next with acc }
  | XORINT                    ->
      let v = (Mlstack.pop state.stack) in
      let acc = int_op (fun x y -> x lxor y ) state.acc v in
      interp_loop level { next with acc }
  | LSLINT                    ->
    let v = (Mlstack.pop state.stack) in
    let acc = int_op (fun x y -> x lsl y ) state.acc v in
    interp_loop level { next with acc }
  | LSRINT                    ->
    let v = (Mlstack.pop state.stack) in
    let acc = int_op (fun x y -> x lsr y ) state.acc v in
    interp_loop level { next with acc }
  | ASRINT                    ->
    let v = (Mlstack.pop state.stack) in
    let acc = int_op (fun x y -> x asr y ) state.acc v in
    interp_loop level { next with acc }
  | EQ                        ->
    let v = (Mlstack.pop state.stack) in
    let acc = int_op (fun x y -> if x = y then 1 else 0 ) state.acc v in
    interp_loop level { next with acc }
  | NEQ ->
      let v = (Mlstack.pop state.stack) in
      let acc = int_op (fun x y -> if x <> y then 1 else 0 ) state.acc v in
      interp_loop level { next with acc }
  | LTINT                     ->
    let v = (Mlstack.pop state.stack) in
    let acc = int_op (fun x y -> if x < y then 1 else 0) state.acc v in
    interp_loop level { next with acc }
  | LEINT                     ->
    let v = (Mlstack.pop state.stack) in
    let acc = int_op (fun x y -> if x <= y then 1 else 0 ) state.acc v in
    interp_loop level { next with acc }
  | GTINT                     ->
    let v = (Mlstack.pop state.stack) in
    let acc = int_op (fun x y -> if x > y then 1 else 0 ) state.acc v in
    interp_loop level { next with acc }
  | GEINT                     ->
    let v = (Mlstack.pop state.stack) in
    let acc = int_op (fun x y -> if x >= y then 1 else 0) state.acc v in
    interp_loop level { next with acc }
  | OFFSETINT n               ->
    let acc = int_op (fun x y -> x + y ) state.acc (Int n) in
    interp_loop level { next with acc }
  | OFFSETREF n               ->
    begin
      match state.acc with
      | Block (tag,t) -> t.(0) <- int_op (fun x y -> x + y) t.(0) (Int n)
      | _ -> failwith "not a block"
    end;
    interp_loop level next
  | ISINT                     ->
    let acc = match state.acc with
      | Int i -> Int 1
      | _ -> Int 0
    in
    interp_loop level { next with acc }
  | GETMETHOD                 ->
    let acc = Dummy in
    interp_loop level { next with acc }
  | BEQ (n, ptr)              ->
       begin
      match state.acc with
      | Dummy ->
        interp_loop (level+1) { next with pc = ptr };
        interp_loop level next
      | Int v -> if n = v then
          interp_loop (level+1) { next with pc = ptr }
        else
          interp_loop level next
      | _ -> failwith "wrong accumulator"
    end
  | BNEQ (n, ptr)             ->
   begin
      match state.acc with
      | Dummy ->
        interp_loop (level+1) { next with pc = ptr };
        interp_loop level next
      | Int v -> if n <> v then
          interp_loop (level+1) { next with pc = ptr }
        else
          interp_loop level next
      | _ -> failwith "wrong accumulator"
    end
  | BLTINT (n, ptr)           ->
       begin
      match state.acc with
      | Dummy ->
        interp_loop (level+1) { next with pc = ptr };
        interp_loop level next
      | Int v -> if n < v then
          interp_loop (level+1) { next with pc = ptr }
        else
          interp_loop level next
      | _ -> failwith "wrong accumulator"
    end
  | BLEINT (n, ptr)           ->
     begin
      match state.acc with
      | Dummy ->
        interp_loop (level+1) { next with pc = ptr };
        interp_loop level next
      | Int v -> if n <= v then
          interp_loop (level+1) { next with pc = ptr }
        else
          interp_loop level next
      | _ -> failwith "wrong accumulator"
    end
  | BGTINT (n, ptr)           ->
    begin
      match state.acc with
      | Dummy ->
        interp_loop (level+1) { next with pc = ptr };
        interp_loop level next
      | Int v -> if n > v then
          interp_loop (level+1) { next with pc = ptr }
        else
          interp_loop level next
      | _ -> failwith "wrong accumulator"
    end
  | BGEINT (n, ptr) ->
        begin
      match state.acc with
      | Dummy ->
        interp_loop (level+1) { next with pc = ptr };
        interp_loop level next
      | Int v -> if n >= v then
          interp_loop (level+1) { next with pc = ptr }
        else
          interp_loop level next
      | _ -> failwith "wrong accumulator"
    end
  | ULTINT ->
    let n = int_op (fun x y -> x + min_int) state.acc (Int 0) in
    let p = int_op (fun x y -> x + min_int) (Mlstack.pop state.stack) (Int 0) in
    let acc = int_op (fun x y -> if x < y then 1 else 0) n p in
    interp_loop level { next with acc }
  | UGEINT ->
    let n = int_op (fun x y -> x + min_int) state.acc (Int 0) in
    let p = int_op (fun x y -> x + min_int) (Mlstack.pop state.stack) (Int 0) in
    let acc = int_op (fun x y -> if x >= y then 1 else 0) n p in
    interp_loop level { next with acc }
  (* | BULTINT (n,ptr) -> *)
  (*   interp_loop level { next with pc = ptr }; *)
  (*   interp_loop level next *)
  (* | BUGEINT (n, ptr) -> *)
  (*   interp_loop level { next with pc = ptr }; *)
  (*   interp_loop level next *)
  | GETPUBMET (tag, cache)    -> failwith "todo getpubmet"
  | GETDYNMET                 -> failwith "todo getdynmet"
  | STOP                      -> ()
  | EVENT                     -> failwith "todo event"
  | BREAK                     -> failwith "todo break"
  | _ -> failwith "unknown instr"
  in
  interp_loop level state



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


let interp file =
  let bytefile = Bytefile.read file in
   let data = bytefile.Bytefile.data in
   let symb = bytefile.Bytefile.symb in
   let primitives = bytefile.Bytefile.prim in
   let bytecode = bytefile.Bytefile.code in
   let global = Array.map value_of_obytelib data in
  Printf.printf "\nGLOBAL =\n";
  Array.iteri (fun i s -> Format.printf "\t %d : %s\n" i (Value.to_string s)) data;
  OByteLib.Code.print data symb primitives stdout  bytecode;
  Array.iteri (fun i s -> Format.printf "\t %d : %s\n" i (string_of_value s)) global;
  interp_loop_0 0 bytecode (new_state global) primitives;
  Printf.printf "Max heap size = %d\n" !max_heap;
  Printf.printf "Allocated %d blocks total in heap\n" !nb_blocks;
  Printf.printf "Max size of the stack = %d levels\n" !max_stack
