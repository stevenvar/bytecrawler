open OByteLib
open OByteLib.Instr

open State
    open Intrp


exception End_of_loop of int

exception Unsupported
let cyc =
  let csv = Csv.load ~separator:';' "wcet/cyc.csv" in
  let csv = List.map (fun l -> match l with [x;y] -> (x, int_of_string y) | _ -> raise (Invalid_argument "csv")) csv in
  ref csv

let cycles inst switch nbloops =
  if not switch then 0 else (
    match inst with
    (* | C_CALL1 _ -> 0 *)
    | C_CALL1 x -> (Printf.printf "CCALL : %d" x); raise (Unsupported)
    | _ ->
      let suffix = match nbloops with
          None -> ""
        | Some x -> "_"^(string_of_int x) in
      let nb = try List.assoc ((string_of_instr inst)^suffix) !cyc with
          Not_found ->
          Format.fprintf Format.std_formatter "Not found : %s \n" ((string_of_instr inst)^suffix) ;
                 try List.assoc ((string_of_instr inst)^"_4B"^suffix) !cyc with
                   Not_found -> List.assoc ((string_of_instr inst)^"_2B"^suffix) !cyc
      in
      Format.fprintf Format.std_formatter "%s %i \n" ((string_of_instr inst)^suffix) nb;
      if nb = -1 then (
        raise Unsupported)
      else nb)

(* pas tail rec ... *)
let count_loop_0 level bytecode state prims :int =
let rec count_loop level state switch cpt =
  let inst = bytecode.(state.pc) in
  let next = { state with pc = state.pc + 1 } in
  match inst with
  | ACC0 -> let i = Mlstack.peek state.stack 0 in
    count_loop level { next with acc = i } switch (cpt + cycles inst switch None)
  | ACC1 -> let i = Mlstack.peek state.stack 1 in
    count_loop level { next with acc = i } switch (cpt + cycles inst switch None)
  | ACC2 -> let i = Mlstack.peek state.stack 2 in
    count_loop level { next with acc = i } switch (cpt + cycles inst switch None)
  | ACC3 -> let i = Mlstack.peek state.stack 3 in
    count_loop level { next with acc = i } switch (cpt + cycles inst switch None)
  | ACC4 -> let i = Mlstack.peek state.stack 4 in
    count_loop level { next with acc = i } switch (cpt + cycles inst switch None)
  | ACC5 -> let i = Mlstack.peek state.stack 5 in
    count_loop level { next with acc = i } switch (cpt + cycles inst switch None)
  | ACC6 -> let i = Mlstack.peek state.stack 6 in
    count_loop level { next with acc = i } switch (cpt + cycles inst switch None)
  | ACC7 -> let i = Mlstack.peek state.stack 7 in
    count_loop level { next with acc = i } switch (cpt + cycles inst switch None)
  | ACC n -> let i = Mlstack.peek state.stack n in
    count_loop level { next with acc = i } switch (cpt + cycles inst switch None)
  | PUSH -> Mlstack.push state.stack state.acc;
    count_loop level next switch (cpt + cycles inst switch None)
  | PUSHACC0 ->
    Mlstack.push state.stack state.acc;
    let i = Mlstack.peek state.stack 0 in
    count_loop level { next with acc = i } switch (cpt + cycles inst switch None)
  | PUSHACC1 ->
    Mlstack.push state.stack state.acc;
    let i = Mlstack.peek state.stack 1 in
    count_loop level { next with acc = i } switch (cpt + cycles inst switch None)
  | PUSHACC2 ->
    Mlstack.push state.stack state.acc;
    let i = Mlstack.peek state.stack 2 in
    count_loop level { next with acc = i } switch (cpt + cycles inst switch None)
  | PUSHACC3 ->
    Mlstack.push state.stack state.acc;
    let i = Mlstack.peek state.stack 3 in
    count_loop level { next with acc = i } switch (cpt + cycles inst switch None)
  | PUSHACC4 ->
    Mlstack.push state.stack state.acc;
    let i = Mlstack.peek state.stack 4 in
    count_loop level { next with acc = i } switch (cpt + cycles inst switch None)
  | PUSHACC5 ->
    Mlstack.push state.stack state.acc;
    let i = Mlstack.peek state.stack 5 in
    count_loop level { next with acc = i } switch (cpt + cycles inst switch None)
  | PUSHACC6 ->
    Mlstack.push state.stack state.acc;
    let i = Mlstack.peek state.stack 6 in
    count_loop level { next with acc = i } switch (cpt + cycles inst switch None)
  | PUSHACC7 ->
    Mlstack.push state.stack state.acc;
    let i = Mlstack.peek state.stack 7 in
    count_loop level { next with acc = i } switch (cpt + cycles inst switch None)
  | PUSHACC n ->
    Mlstack.push state.stack state.acc;
    let i = Mlstack.peek state.stack n in
    count_loop level { next with acc = i } switch (cpt + cycles inst switch None)
  | POP n -> Mlstack.popn state.stack n;
    count_loop level next switch (cpt + cycles inst switch None)
  | ASSIGN n -> Mlstack.set state.stack n state.acc;
    count_loop level { next with acc = Dummy } switch (cpt + cycles inst switch None)
  | ENVACC1                   ->
    count_loop level { next with acc = field state.env 1 } switch (cpt + cycles inst switch None)
  | ENVACC2                   ->
    count_loop level { next with acc = field state.env 2 } switch (cpt + cycles inst switch None)
  | ENVACC3                   ->
    count_loop level { next with acc = field state.env 3 } switch (cpt + cycles inst switch None)
  | ENVACC4                   ->
    count_loop level { next with acc = field state.env 4 } switch (cpt + cycles inst switch None)
  | ENVACC n                   ->
    count_loop level { next with acc = field state.env n } switch (cpt + cycles inst switch None)
  | PUSHENVACC1               ->
    Mlstack.push state.stack state.acc;
    let acc = field state.env 1 in
    count_loop level { next with acc = acc } switch (cpt + cycles inst switch None)
  | PUSHENVACC2               ->
    Mlstack.push state.stack state.acc;
    count_loop level { next with acc = field state.env 2 } switch (cpt + cycles inst switch None)
  | PUSHENVACC3               ->
    Mlstack.push state.stack state.acc;
    count_loop level { next with acc = field state.env 3 } switch (cpt + cycles inst switch None)
  | PUSHENVACC4               ->
    Mlstack.push state.stack state.acc;
    count_loop level { next with acc = field state.env 4 } switch (cpt + cycles inst switch None)
  | PUSHENVACC n               ->
    Mlstack.push state.stack state.acc;
    count_loop level { next with acc = field state.env n } switch (cpt + cycles inst switch None)
  | PUSH_RETADDR ptr ->
    Mlstack.push state.stack (Int state.extraArgs);
    Mlstack.push state.stack state.env;
    Mlstack.push state.stack (Ptr ptr);
    count_loop level next switch (cpt + cycles inst switch None)
  | APPLY n ->
    count_loop (level+1) { next with pc = ptr_of_value state.acc ;
                                           env = state.acc ;
                                           extraArgs = n - 1 } switch (cpt + cycles inst switch None)
  | APPLY1 ->
    let arg = Mlstack.pop state.stack in
    Mlstack.push state.stack (Int state.extraArgs);
    Mlstack.push state.stack state.env;
    Mlstack.push state.stack (Ptr (state.pc + 1));
    Mlstack.push state.stack arg;
    count_loop (level+1) { next with pc = ptr_of_value state.acc ;
                                           env = state.acc ;
                                           extraArgs = 0 } switch (cpt + cycles inst switch None)
  | APPLY2 ->
    let arg1 = Mlstack.pop state.stack in
    let arg2 = Mlstack.pop state.stack in
    Mlstack.push state.stack (Int state.extraArgs);
    Mlstack.push state.stack state.env;
    Mlstack.push state.stack (Ptr (state.pc + 1));
    Mlstack.push state.stack arg2;
    Mlstack.push state.stack arg1;
    count_loop level { next with pc = ptr_of_value state.acc ;
                                           env = state.acc ;
                                           extraArgs = 1 } switch (cpt + cycles inst switch None)
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
    count_loop level { next with pc = ptr_of_value state.acc ;
                                           env = state.acc ;
                                           extraArgs = 2 } switch (cpt + cycles inst switch None)
  | APPTERM (n, s)  ->
    for i = 0 to n - 1 do
      let arg = Mlstack.peek state.stack (n- i - 1) in
      Mlstack.set state.stack (s - i - 1) arg
    done;
    Mlstack.popn state.stack (s - n);
    let extraArgs = state.extraArgs + (n - 1) in
    let env = state.acc in
    count_loop level { next with pc = ptr_of_value state.acc ;
                                           env = env ;
                                           extraArgs = extraArgs } switch (cpt + cycles inst switch (Some n))
  | APPTERM1 n ->
    let arg = Mlstack.top state.stack in
    Mlstack.popn state.stack n;
    (* Format.printf "---> %d <<<---" (int_of_value arg); *)
    Mlstack.push state.stack arg;
    let env = state.acc in
    count_loop level { next with pc = ptr_of_value state.acc ;
                                           env = env} switch (cpt + cycles inst switch None)
  | APPTERM2 n ->

    let arg1 = Mlstack.pop state.stack in
    let arg2 = Mlstack.pop state.stack  in
    Mlstack.popn state.stack (n-2);
    Mlstack.push state.stack arg2;
    Mlstack.push state.stack arg1;
    let env = state.acc in
    let extraArgs = state.extraArgs + 1 in
    count_loop level { next with pc = ptr_of_value state.acc;
                                 env = env ;
                                 extraArgs = extraArgs } switch (cpt + cycles inst switch None)
  | APPTERM3 n ->
    let arg1 = Mlstack.peek state.stack 0 in
    let arg2 = Mlstack.peek state.stack 1 in
    let arg3 = Mlstack.peek state.stack 2 in
    Mlstack.popn state.stack n;
    Mlstack.push state.stack arg3;
    Mlstack.push state.stack arg2;
    Mlstack.push state.stack arg1;
    let extraArgs = state.extraArgs + 2 in
    let env = state.acc in
    count_loop level { next with pc = ptr_of_value state.acc ;
                                 env = env ;
                                           extraArgs = extraArgs } switch (cpt + cycles inst switch None)
  | RETURN n ->
    Mlstack.popn state.stack n;
    if (state.extraArgs = 0) then
      (
        let pc = ptr_of_value (Mlstack.pop state.stack) in
        let env = Mlstack.pop state.stack in
        let extraArgs = int_of_value (Mlstack.pop state.stack) in
        count_loop (level-1) { next with pc ; env ; extraArgs} switch (cpt + cycles inst switch None)
      )
    else (
      let pc = ptr_of_value state.acc in
      let env = state.acc in
      let extraArgs = state.extraArgs - 1 in
      count_loop level { next with pc; env ; extraArgs} switch (cpt + cycles inst switch None)
    )
  | RESTART ->
    (* nargs = Wosize_val(env) -1  *)
    let blk = env_of_closure state.env in
    let n = Array.length blk - 1 in
    for i = n downto 1 do
      Mlstack.push state.stack blk.(i)
    done;
    let extraArgs = state.extraArgs + n in
    let env = blk.(0) in
    count_loop level { next with env ; extraArgs} switch (cpt + cycles inst switch (Some n))
  | GRAB n ->
    if state.extraArgs >= n then
      let extraArgs = state.extraArgs - n in
      count_loop level { next with extraArgs = extraArgs } switch (cpt + cycles inst switch (None))
    else
      let a = Array.make (state.extraArgs + 2) (state.acc) in
      for i = 1 to state.extraArgs + 1 do
        a.(i) <- Mlstack.pop state.stack
      done;
      let acc = Closure (Ptr (state.pc - 1) , a) in
      (* set_field state.acc 1 state.env; *)
      (* for i = 0 to state.extraArgs do *)
      (*   set_field state.acc (i+2) (Mlstack.pop state.stack) *)
      (* done; *)
      let sp = state.trapSp + state.extraArgs + 1 in
      let pc = ptr_of_value (Mlstack.pop state.stack) in
      let env = Mlstack.pop state.stack in
      let extraArgs = int_of_value (Mlstack.pop state.stack) in
      count_loop level { next with acc ; pc ; trapSp = sp;  env ; extraArgs } switch (cpt + cycles inst switch (Some n))
  | CLOSURE (n,ptr) ->
    let a = Array.make n (state.acc) in
    for i = 1 to n - 1 do
      a.(i) <- Mlstack.pop state.stack;
    done;
    count_loop level { next with acc = Closure(Ptr ptr,a) } switch (cpt + cycles inst switch (Some n))
  | CLOSUREREC (f, v, ptr, t)   ->
    (* f = number of functions
     * v = number of variables *)
    (* if v > 0 then Mlstack.push state.stack state.acc; *)
    let blk = Array.make v (state.acc) in
    for i = 1 to v - 1 do
      blk.(i) <- Mlstack.pop state.stack;
    done;
    let acc = Closure_rec (Ptr ptr,t,blk,0) in
    Mlstack.push state.stack acc;
    for i = 1 to Array.length t do
      Mlstack.push state.stack (Block (0,[||]));
    done;
    count_loop level {next with acc = acc } switch (cpt + cycles inst switch (Some f))
  | OFFSETCLOSUREM2           ->
    let acc = offsetclosure state.env (-2) in
    count_loop level { next with acc} switch (cpt + cycles inst switch None)
  | OFFSETCLOSURE0            ->
    let acc = offsetclosure state.env 0 in
    count_loop level { next with acc} switch (cpt + cycles inst switch None)
  | OFFSETCLOSURE2            ->
    let acc = offsetclosure state.env 2 in
    count_loop level { next with acc} switch (cpt + cycles inst switch None)
  | OFFSETCLOSURE n           ->
    let acc = offsetclosure state.env n in
    count_loop level { next with acc} switch (cpt + cycles inst switch None)
  | PUSHOFFSETCLOSUREM2       ->
    Mlstack.push state.stack state.acc;
    let acc = offsetclosure state.env (-2) in
    count_loop level { next with acc} switch (cpt + cycles inst switch None)
  | PUSHOFFSETCLOSURE0        ->
    Mlstack.push state.stack state.acc;
    let acc = offsetclosure state.env 0 in
    count_loop level { next with acc} switch (cpt + cycles inst switch None)
  | PUSHOFFSETCLOSURE2        ->
    Mlstack.push state.stack state.acc;
    let acc = offsetclosure state.env 2 in
    count_loop level { next with acc } switch (cpt + cycles inst switch None)
  | PUSHOFFSETCLOSURE n       ->
    Mlstack.push state.stack state.acc;
    let acc = offsetclosure state.env n
    in count_loop level { next with acc } switch (cpt + cycles inst switch None)
  | GETGLOBAL n               ->
    let acc = state.global.(n) in
    count_loop level { next with acc = acc } switch (cpt + cycles inst switch None)
  | PUSHGETGLOBAL n           ->
    Mlstack.push state.stack state.acc;
    let acc = state.global.(n) in
    count_loop level { next with acc = acc } switch (cpt + cycles inst switch None)
  | GETGLOBALFIELD (n, p)     ->
    let acc = field state.global.(n) p in
    count_loop level { next with acc = acc } switch (cpt + cycles inst switch None)
  | PUSHGETGLOBALFIELD (n, p) ->
    Mlstack.push state.stack state.acc;
    let acc = field state.global.(n) p in
    count_loop level { next with acc = acc } switch (cpt + cycles inst switch None)
  | SETGLOBAL n               ->
    state.global.(n) <- state.acc ;
    count_loop level { next with acc = Dummy } switch (cpt + cycles inst switch None)
  | ATOM0                     ->
    let blk = Block (0, [||]) in
    count_loop level {next with acc = blk } switch (cpt + cycles inst switch None)
  | ATOM tag                  ->
    let blk = Block (tag, [||]) in
    count_loop level { next with acc = blk } switch (cpt + cycles inst switch None)
  | PUSHATOM0                 ->
    Mlstack.push state.stack state.acc;
    let blk = Block (0 , [||]) in
    count_loop level { next with acc = blk } switch (cpt + cycles inst switch None)
  | PUSHATOM tag              ->
    Mlstack.push state.stack state.acc;
    let blk = Block (0, [||]) in
    count_loop level { next with acc = blk } switch (cpt + cycles inst switch None)
  | MAKEBLOCK (tag, sz)       ->
    let a = Array.make sz Dummy in
    let blk = Block (tag, a) in
    a.(0) <- state.acc;
    for i = 1 to sz - 1 do
      a.(i) <- Mlstack.pop state.stack;
    done;
    count_loop level { next with acc = blk } switch (cpt + cycles inst switch (Some sz))
  | MAKEBLOCK1 tag            ->
    let a = Array.make 1 (state.acc) in
    let blk = Block (tag, a) in
    count_loop level { next with acc = blk } switch (cpt + cycles inst switch None)
  | MAKEBLOCK2 tag            ->
    let a = Array.make 2 (state.acc) in
    let blk = Block (tag, a) in
    a.(1) <- Mlstack.pop state.stack;
    count_loop level { next with acc = blk } switch (cpt + cycles inst switch None)
  | MAKEBLOCK3 tag            ->
    let a = Array.make 3 (state.acc) in
    let blk = Block (tag, a) in
    a.(1) <- Mlstack.pop state.stack;
    a.(2) <- Mlstack.pop state.stack;
    count_loop level { next with acc = blk } switch (cpt + cycles inst switch None)
  | MAKEFLOATBLOCK n         ->
    let a = Array.make n Dummy in
    let blk = Block (Obj.double_array_tag,a) in
    a.(0) <- state.acc;
    Mlstack.popn state.stack (n-1);
    count_loop level { next with acc = blk } switch (cpt + cycles inst switch None)
  | GETFIELD0                 ->
    let acc = field state.acc 0 in
    count_loop level { next with acc = acc } switch (cpt + cycles inst switch None)
  | GETFIELD1                 ->
    let acc = field state.acc 1 in
    count_loop level { next with acc = acc } switch (cpt + cycles inst switch None)
  | GETFIELD2                 ->
    let acc = field state.acc 2 in
    count_loop level { next with acc = acc } switch (cpt + cycles inst switch None)
  | GETFIELD3                 ->
    let acc = field state.acc 3 in
    count_loop level { next with acc = acc } switch (cpt + cycles inst switch None)
  | GETFIELD n                ->
    let acc = field state.acc n in
    count_loop level { next with acc = acc } switch (cpt + cycles inst switch None)
  | GETFLOATFIELD i ->
    let acc = Dummy in
    count_loop level { next with acc = acc } switch (cpt + cycles inst switch None)
  | SETFIELD0                 ->
    set_field state.acc 0 (Mlstack.pop state.stack);
    let acc = Dummy in
    count_loop level { next with acc = acc } switch (cpt + cycles inst switch None)
  | SETFIELD1                 ->
    set_field state.acc 1 (Mlstack.pop state.stack);
    let acc = Dummy in
    count_loop level { next with acc = acc } switch (cpt + cycles inst switch None)
  | SETFIELD2                 ->
    set_field state.acc 2 (Mlstack.pop state.stack);
    let acc = Dummy in
    count_loop level { next with acc = acc } switch (cpt + cycles inst switch None)
  | SETFIELD3                 ->
    set_field state.acc 3 (Mlstack.pop state.stack);
    let acc = Dummy in
    count_loop level { next with acc = acc } switch (cpt + cycles inst switch None)
  | SETFIELD n                 ->
    set_field state.acc n (Mlstack.pop state.stack);
    let acc = Dummy in
    count_loop level { next with acc = acc } switch (cpt + cycles inst switch None)
  | SETFLOATFIELD n           ->
    let acc = Dummy in
    count_loop level { next with acc = acc } switch (cpt + cycles inst switch None)
  | VECTLENGTH                ->
    let acc = Dummy in
    count_loop level { next with acc = acc } switch (cpt + cycles inst switch None)
  | GETVECTITEM               ->
    ignore @@ Mlstack.pop state.stack;
    let acc = Dummy in
    count_loop level { next with acc = acc } switch (cpt + cycles inst switch None)
  | SETVECTITEM               ->
    ignore @@ Mlstack.pop state.stack;
    ignore @@ Mlstack.pop state.stack;
    let acc = Dummy in
    count_loop level { next with acc = acc } switch (cpt + cycles inst switch None)
  | GETSTRINGCHAR             ->
    ignore @@ Mlstack.pop state.stack;
    let acc = Dummy in
    count_loop level { next with acc = acc } switch (cpt + cycles inst switch None)
  | SETSTRINGCHAR             ->
    ignore @@ Mlstack.pop state.stack;
    ignore @@ Mlstack.pop state.stack;
    let acc = Dummy in
    count_loop level { next with acc = acc } switch (cpt + cycles inst switch None)
  | BRANCH b -> count_loop level { next with pc = b }  switch (cpt + cycles inst switch None)
  | BRANCHIF ptr              ->
     begin
      match state.acc with
        | Dummy ->
          max (count_loop (level+1) { next with pc = ptr } switch (cpt + cycles inst switch None))
            (count_loop level next switch (cpt + cycles inst switch None))
      | Int 0 -> count_loop level next switch (cpt + cycles inst switch None)
      | _ -> count_loop (level+1) { next with pc = ptr } switch (cpt + cycles inst switch None)
    end
  | BRANCHIFNOT ptr           ->
    begin
      match state.acc with
      | Dummy ->
        max (count_loop (level+1) { next with pc = ptr } switch (cpt + cycles inst switch None))
              (count_loop level next switch (cpt + cycles inst switch None))
      | Int 0 -> count_loop (level+1) { next with pc = ptr } switch (cpt + cycles inst switch None)
      | _ -> count_loop level next switch (cpt + cycles inst switch None)
    end
  | SWITCH(n, ptrs)          ->
    (* Probleme ici  *)
    begin
    match state.acc with
      | Int v ->
        count_loop level { next with pc = ptrs.(v) } switch (cpt + cycles inst switch None)
      | Block (tag,b) ->  count_loop level { next with pc = ptrs.(tag + (n land 0xFFFF)) } switch (cpt + cycles inst switch None)
      | _ -> failwith "?"
  end
  | BOOLNOT                   ->
    let acc = match state.acc with
      | Int 0 -> Int 1
      | Int 1 -> Int 0
      | _ -> failwith "not a bool"
    in
    count_loop level { next with acc } switch (cpt + cycles inst switch None)
  | PUSHTRAP ptr              ->
    Mlstack.push state.stack (Int state.extraArgs);
    Mlstack.push state.stack state.env;
    Mlstack.push state.stack (Int state.trapSp);
    Mlstack.push state.stack (Ptr ptr);
    count_loop level { next with trapSp = Mlstack.length state.stack } switch (cpt + cycles inst switch None)
  | POPTRAP                   ->
    ignore @@ Mlstack.pop state.stack;
    let trapSp = int_of_value (Mlstack.pop state.stack) in
    ignore @@ Mlstack.pop state.stack;
    ignore @@ Mlstack.pop state.stack;
    count_loop level { next with trapSp = trapSp } switch (cpt + cycles inst switch None)
  | C_CALL1 idx ->
    let acc = Dummy in
    if prims.(idx) = "begin_loop" then
      count_loop level { next with acc = acc} true (cpt + cycles inst switch None)
    else if prims.(idx) = "end_loop" then
      cpt
    else
      count_loop level { next with acc = acc} switch (cpt + cycles inst switch None)
  | C_CALL2 idx ->
    let acc = Dummy in
    ignore @@ Mlstack.pop state.stack;
    count_loop level { next with acc } switch (cpt + cycles inst switch None)
  | C_CALL3 idx ->
    let acc = Dummy in
    ignore @@ Mlstack.pop state.stack;
    ignore @@ Mlstack.pop state.stack;
    count_loop level { next with acc } switch (cpt + cycles inst switch None)
  | C_CALL4 idx ->
    let acc = Dummy in
    ignore @@ Mlstack.pop state.stack;
    ignore @@ Mlstack.pop state.stack;
    ignore @@ Mlstack.pop state.stack;
    count_loop level { next with acc } switch (cpt + cycles inst switch None)
  | C_CALL5 idx ->
    let acc = Dummy in
    ignore @@ Mlstack.pop state.stack;
    ignore @@ Mlstack.pop state.stack;
    ignore @@ Mlstack.pop state.stack;
    ignore @@ Mlstack.pop state.stack;
    count_loop level { next with acc } switch (cpt + cycles inst switch None)
  | C_CALLN (narg, idx)       ->
    let acc = Dummy in
    Mlstack.push state.stack state.acc;
    Mlstack.popn state.stack narg;
    count_loop level { next with acc } switch (cpt + cycles inst switch None)
  | RAISE | RERAISE | RAISE_NOTRACE -> raise Exit
  | CHECK_SIGNALS             ->
    count_loop level next switch (cpt + cycles inst switch None)
  | CONST0                    -> count_loop level { next with acc = Int 0 } switch (cpt + cycles inst switch None)
  | CONST1                    -> count_loop level { next with acc = Int 1 } switch (cpt + cycles inst switch None)
  | CONST2                    -> count_loop level { next with acc = Int 2 } switch (cpt + cycles inst switch None)
  | CONST3                    -> count_loop level { next with acc = Int 3 } switch (cpt + cycles inst switch None)
  | CONSTINT n                -> count_loop level { next with acc = Int n } switch (cpt + cycles inst switch None)
  | PUSHCONST0                ->
    Mlstack.push state.stack state.acc;
    count_loop level { next with acc = Int 0 } switch (cpt + cycles inst switch None)
  | PUSHCONST1                ->
    Mlstack.push state.stack state.acc;
    count_loop level { next with acc = Int 1 } switch (cpt + cycles inst switch None)
  | PUSHCONST2                ->
    Mlstack.push state.stack state.acc;
    count_loop level { next with acc = Int 2 } switch (cpt + cycles inst switch None)
  | PUSHCONST3                ->
    Mlstack.push state.stack state.acc;
    count_loop level { next with acc = Int 3 } switch (cpt + cycles inst switch None)
  | PUSHCONSTINT n            ->
    Mlstack.push state.stack state.acc;
    count_loop level { next with acc = Int n } switch (cpt + cycles inst switch None)
  | NEGINT                    ->
    let acc = int_op (fun x y -> -x ) state.acc (Int 0) in
    count_loop level { next with acc = acc } switch (cpt + cycles inst switch None)
  | ADDINT                    ->
    let v = (Mlstack.pop state.stack) in
    let acc = int_op (fun x y -> x+y ) state.acc v in
    count_loop level { next with acc } switch (cpt + cycles inst switch None)
  | SUBINT                    ->
    let v = (Mlstack.pop state.stack) in
    let acc = int_op (fun x y -> x-y ) state.acc v in
    count_loop level { next with acc } switch (cpt + cycles inst switch None)
  | MULINT                    ->
    let v = (Mlstack.pop state.stack) in
    let acc = int_op (fun x y -> x*y ) state.acc v in
    count_loop level { next with acc } switch (cpt + cycles inst switch None)
  | DIVINT                    ->
    let v = (Mlstack.pop state.stack) in
    let acc = int_op (fun x y -> x/y ) state.acc v in
    count_loop level { next with acc } switch (cpt + cycles inst switch None)
  | MODINT                    ->
    let v = (Mlstack.pop state.stack) in
    let acc = int_op (fun x y -> x mod y ) state.acc v in
    count_loop level { next with acc } switch (cpt + cycles inst switch None)
  | ANDINT                    ->
     let v = (Mlstack.pop state.stack) in
    let acc = int_op (fun x y -> x land y ) state.acc v in
    count_loop level { next with acc } switch (cpt + cycles inst switch None)
  | ORINT                     ->
    let v = (Mlstack.pop state.stack) in
    let acc = int_op (fun x y -> x lor y ) state.acc v in
    count_loop level { next with acc } switch (cpt + cycles inst switch None)
  | XORINT                    ->
      let v = (Mlstack.pop state.stack) in
      let acc = int_op (fun x y -> x lxor y ) state.acc v in
      count_loop level { next with acc } switch (cpt + cycles inst switch None)
  | LSLINT                    ->
    let v = (Mlstack.pop state.stack) in
    let acc = int_op (fun x y -> x lsl y ) state.acc v in
    count_loop level { next with acc } switch (cpt + cycles inst switch None)
  | LSRINT                    ->
    let v = (Mlstack.pop state.stack) in
    let acc = int_op (fun x y -> x lsr y ) state.acc v in
    count_loop level { next with acc } switch (cpt + cycles inst switch None)
  | ASRINT                    ->
    let v = (Mlstack.pop state.stack) in
    let acc = int_op (fun x y -> x asr y ) state.acc v in
    count_loop level { next with acc } switch (cpt + cycles inst switch None)
  | EQ                        ->
    let v = (Mlstack.pop state.stack) in
    let acc = int_op (fun x y -> if x = y then 1 else 0 ) state.acc v in
    count_loop level { next with acc } switch (cpt + cycles inst switch None)
  | NEQ ->
      let v = (Mlstack.pop state.stack) in
      let acc = int_op (fun x y -> if x <> y then 1 else 0 ) state.acc v in
      count_loop level { next with acc } switch (cpt + cycles inst switch None)
  | LTINT                     ->
    let v = (Mlstack.pop state.stack) in
    let acc = int_op (fun x y -> if x < y then 1 else 0) state.acc v in
    count_loop level { next with acc } switch (cpt + cycles inst switch None)
  | LEINT                     ->
    let v = (Mlstack.pop state.stack) in
    let acc = int_op (fun x y -> if x <= y then 1 else 0 ) state.acc v in
    count_loop level { next with acc } switch (cpt + cycles inst switch None)
  | GTINT                     ->
    let v = (Mlstack.pop state.stack) in
    let acc = int_op (fun x y -> if x > y then 1 else 0 ) state.acc v in
    count_loop level { next with acc } switch (cpt + cycles inst switch None)
  | GEINT                     ->
    let v = (Mlstack.pop state.stack) in
    let acc = int_op (fun x y -> if x >= y then 1 else 0) state.acc v in
    count_loop level { next with acc } switch (cpt + cycles inst switch None)
  | OFFSETINT n               ->
    let acc = int_op (fun x y -> x + y ) state.acc (Int n) in
    count_loop level { next with acc } switch (cpt + cycles inst switch None)
  | OFFSETREF n               ->
    begin
      match state.acc with
      | Block (tag,t) -> t.(0) <- int_op (fun x y -> x + y) t.(0) (Int n)
      | _ -> failwith "not a block"
    end;
    count_loop level next switch (cpt + cycles inst switch None)
  | ISINT                     ->
    let acc = match state.acc with
      | Int i -> Int 1
      | _ -> Int 0
    in
    count_loop level { next with acc } switch (cpt + cycles inst switch None)
  | GETMETHOD                 ->
    let acc = Dummy in
    count_loop level { next with acc } switch (cpt + cycles inst switch None)
  | BEQ (n, ptr)              ->
       begin
      match state.acc with
        | Dummy ->
          max (count_loop (level+1) { next with pc = ptr }  switch (cpt + cycles inst switch None))
            (count_loop level next  switch (cpt + cycles inst switch None))
      | Int v -> if n = v then
          count_loop (level+1) { next with pc = ptr } switch (cpt + cycles inst switch None)
        else
          count_loop level next switch (cpt + cycles inst switch None)
      | _ -> failwith "wrong accumulator"
    end
  | BNEQ (n, ptr)             ->
   begin
      match state.acc with
      | Dummy ->
        max (count_loop (level+1) { next with pc = ptr } switch (cpt + cycles inst switch None))
        (count_loop level next switch (cpt + cycles inst switch None))
      | Int v -> if n <> v then
          count_loop (level+1) { next with pc = ptr } switch (cpt + cycles inst switch None)
        else
          count_loop level next switch (cpt + cycles inst switch None)
      | _ -> failwith "wrong accumulator"
    end
  | BLTINT (n, ptr)           ->
       begin
      match state.acc with
      | Dummy ->
        max (count_loop (level+1) { next with pc = ptr } switch (cpt + cycles inst switch None))
        (count_loop level next switch (cpt + cycles inst switch None))
      | Int v -> if n < v then
          count_loop (level+1) { next with pc = ptr } switch (cpt + cycles inst switch None)
        else
          count_loop level next switch (cpt + cycles inst switch None)
      | _ -> failwith "wrong accumulator"
    end
  | BLEINT (n, ptr)           ->
     begin
      match state.acc with
        | Dummy ->
          max (count_loop (level+1) { next with pc = ptr } switch (cpt + cycles inst switch None))
            (count_loop level next switch (cpt + cycles inst switch None))
      | Int v -> if n <= v then
          count_loop (level+1) { next with pc = ptr } switch (cpt + cycles inst switch None)
        else
          count_loop level next switch (cpt + cycles inst switch None)
      | _ -> failwith "wrong accumulator"
    end
  | BGTINT (n, ptr)           ->
    begin
      match state.acc with
      | Dummy ->
        max (count_loop (level+1) { next with pc = ptr } switch (cpt + cycles inst switch None))
        (count_loop level next switch (cpt + cycles inst switch None))
      | Int v -> if n > v then
          count_loop (level+1) { next with pc = ptr } switch (cpt + cycles inst switch None)
        else
          count_loop level next switch (cpt + cycles inst switch None)
      | _ -> failwith "wrong accumulator"
    end
  | BGEINT (n, ptr) ->
        begin
      match state.acc with
        | Dummy ->
          max (count_loop (level+1) { next with pc = ptr } switch (cpt + cycles inst switch None))
            (count_loop level next switch (cpt + cycles inst switch None))
      | Int v -> if n >= v then
          count_loop (level+1) { next with pc = ptr } switch (cpt + cycles inst switch None)
        else
          count_loop level next switch (cpt + cycles inst switch None)
      | _ -> failwith "wrong accumulator"
    end
  | ULTINT ->
    let n = int_op (fun x y -> x + min_int) state.acc (Int 0) in
    let p = int_op (fun x y -> x + min_int) (Mlstack.pop state.stack) (Int 0) in
    let acc = int_op (fun x y -> if x < y then 1 else 0) n p in
    count_loop level { next with acc } switch (cpt + cycles inst switch None)
  | UGEINT ->
    let n = int_op (fun x y -> x + min_int) state.acc (Int 0) in
    let p = int_op (fun x y -> x + min_int) (Mlstack.pop state.stack) (Int 0) in
    let acc = int_op (fun x y -> if x >= y then 1 else 0) n p in
    count_loop level { next with acc } switch (cpt + cycles inst switch None)
  (* | BULTINT (n,ptr) -> *)
  (*   count_loop level { next with pc = ptr }; *)
  (*   count_loop level next *)
  (* | BUGEINT (n, ptr) -> *)
  (*   count_loop level { next with pc = ptr }; *)
  (*   count_loop level next *)
  | GETPUBMET (tag, cache)    -> failwith "todo getpubmet"
  | GETDYNMET                 -> failwith "todo getdynmet"
  | STOP                      -> 0
  | EVENT                     -> failwith "todo event"
  | BREAK                     -> failwith "todo break"
  | _ -> failwith "unknown instr"
  in count_loop level state false 0

let cpt = ref 0



let count_cycles f =
  let bytefile = Bytefile.read f in
  let data = bytefile.Bytefile.data in
  let primitives = bytefile.Bytefile.prim in
  let bytecode = bytefile.Bytefile.code in
  let global = Array.map value_of_obytelib data in
  Format.printf "Cycles : %d \n"  @@ count_loop_0 0 bytecode (new_state global) primitives
