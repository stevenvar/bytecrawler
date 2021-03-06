open OByteLib
open OByteLib.Instr

open State
open Intrp



exception End_of_loop of int

let instr_costs = ref "wcet/cyc.csv"

let prim_costs = ref "wcet/primitives.csv"

let cyc = ref []

let prims = ref []


exception Unsupported
let init_cyc () =
  let csv = Csv.load ~separator:';' !instr_costs in
  let csv = List.map (fun l -> match l with [x;y] -> (x, int_of_string y) | _ -> raise (Invalid_argument "csv")) csv in
  cyc := csv

let init_prims () =
  let csv = Csv.load ~separator:';' !prim_costs in
  let csv = List.map (fun l -> match l with [x;y] -> (x, int_of_string y) | _ -> raise (Invalid_argument "csv")) csv in
  prims := csv

let freq = ref 0.

let level = ref 0

let string_param p =
  match p with
  | None -> "_"
  | Some d -> string_of_int d

let print_tabs () =
  for i = 1 to !level do
    Printf.printf "\t";
  done


let cost instr primitives nb start param =
    if not start then 0 else (
           match instr with
    | C_CALL1 i | C_CALL2 i | C_CALL3 i ->
       let name = primitives.(i) in
       begin try
           let n = List.assoc name !prims in
           print_tabs (); Printf.printf "Call to %s : %d\n" name n;
           n
             with Not_found ->
               failwith ("I don't know the cost of primitive "^name)
       end
    | _ ->
       let suffix =
         match nb with
         | None -> ""
         | Some x -> "_"^(string_of_int x) in
       let name = (string_of_instr instr)^suffix in
       let nb =(try
                   match param with
                   | None -> List.assoc ((string_of_instr instr)^suffix) !cyc
                   | Some p ->
                      if p >= -0x80 && p < 0x80 then List.assoc ((string_of_instr instr)^"_1B"^suffix) !cyc
                      else if p >= -0x8000 && p < 0x8000 then List.assoc ((string_of_instr instr)^"_2B"^suffix) !cyc
                      else List.assoc ((string_of_instr instr)^"_4B"^suffix) !cyc
                 with Not_found ->  Format.fprintf Format.std_formatter "Not found : %s %s \n"  ((string_of_instr instr)) (string_param param);
                                    failwith "!?")
       in
       print_tabs (); Printf.printf "%s - (%i cycles) \n" name nb;
       if nb = -1 then (
         raise Unsupported)
       else nb)


let params instr =
  match instr with
  (* | GRAB n -> Some n *)
  (* | CLOSURE (n,ptr) -> Some n *)
  | _ -> None

let eval_primitive_1 =
  let cpt = ref 0 in
  fun name param ->
  Printf.printf "eval primitive %s with param = %s\n" name (string_of_value param);
  match name with
  | "caml_fresh_oo_id" ->
     let n = !cpt in
     incr cpt;
     Int n
  | "caml_obj_dup" ->
     begin match param with
     | Block (n,v) -> Block (n, Array.copy v)
     | _ -> param
     end
  | _ -> Dummy

let eval_primitive_2 name param1 param2 =
  Printf.printf "eval primitive %s with param1 = %s , param2 = %s \n" name (string_of_value param1)
    (string_of_value param2);
  match name with
  | "caml_make_vect" -> begin
      match param1 with
      | Int n -> Block(n, Array.make n param2)
      | _ -> Dummy
    end
  | "caml_array_get_addr" | "caml_array_unsafe_get" -> begin
     match param1, param2 with
     | Block (n,v), Int d -> v.(d)
     | _ -> Dummy
    end
  | _ -> Dummy

let rec eval bytecode primitives state start =
  let instr = bytecode.(state.pc) in
  let next = { state with pc = state.pc + 1 } in
  (* print_state state !level bytecode; *)
  (* print_tabs (); *)
  (* print_endline (Instr.to_string instr); *)
  let c = cost instr primitives (params instr) start in
   match instr with
    | ACC0 -> let i = Mlstack.peek state.stack 0 in
       c None + eval bytecode primitives { next with acc = i } start
    | ACC1 -> let i = Mlstack.peek state.stack 1 in
      c None + eval bytecode primitives { next with acc = i } start
    | ACC2 -> let i = Mlstack.peek state.stack 2 in
      c None + eval bytecode primitives { next with acc = i } start
    | ACC3 -> let i = Mlstack.peek state.stack 3 in
      c None + eval bytecode primitives { next with acc = i } start
    | ACC4 -> let i = Mlstack.peek state.stack 4 in
      c None + eval bytecode primitives { next with acc = i } start
    | ACC5 -> let i = Mlstack.peek state.stack 5 in
      c None + eval bytecode primitives { next with acc = i } start
    | ACC6 -> let i = Mlstack.peek state.stack 6 in
      c None + eval bytecode primitives { next with acc = i } start
    | ACC7 -> let i = Mlstack.peek state.stack 7 in
      c None + eval bytecode primitives { next with acc = i } start
    | ACC n -> let i = Mlstack.peek state.stack n in
      c None + eval bytecode primitives { next with acc = i } start
    | PUSH -> Mlstack.push state.stack state.acc;
      c None + eval bytecode primitives next start
    | PUSHACC0 ->
      Mlstack.push state.stack state.acc;
      let i = Mlstack.peek state.stack 0 in
      c None + eval bytecode primitives { next with acc = i } start
    | PUSHACC1 ->
      Mlstack.push state.stack state.acc;
      let i = Mlstack.peek state.stack 1 in
      c None + eval bytecode primitives { next with acc = i } start
    | PUSHACC2 ->
      Mlstack.push state.stack state.acc;
      let i = Mlstack.peek state.stack 2 in
      c None + eval bytecode primitives { next with acc = i } start
    | PUSHACC3 ->
      Mlstack.push state.stack state.acc;
      let i = Mlstack.peek state.stack 3 in
      c None + eval bytecode primitives { next with acc = i } start
    | PUSHACC4 ->
      Mlstack.push state.stack state.acc;
      let i = Mlstack.peek state.stack 4 in
      c None + eval bytecode primitives { next with acc = i } start
    | PUSHACC5 ->
      Mlstack.push state.stack state.acc;
      let i = Mlstack.peek state.stack 5 in
      c None + eval bytecode primitives { next with acc = i } start
    | PUSHACC6 ->
      Mlstack.push state.stack state.acc;
      let i = Mlstack.peek state.stack 6 in
      c None + eval bytecode primitives { next with acc = i } start
    | PUSHACC7 ->
      Mlstack.push state.stack state.acc;
      let i = Mlstack.peek state.stack 7 in
      c None + eval bytecode primitives { next with acc = i } start
    | PUSHACC n ->
      Mlstack.push state.stack state.acc;
      let i = Mlstack.peek state.stack n in
      c None + eval bytecode primitives { next with acc = i } start
    | POP n -> Mlstack.popn state.stack n;
      c None + eval bytecode primitives next start
    | ASSIGN n -> Mlstack.set state.stack n state.acc;
      c None + eval bytecode primitives next start
    | ENVACC1                   ->
      c None + eval bytecode primitives { next with acc = field state.env 1 } start
    | ENVACC2                   ->
      c None + eval bytecode primitives { next with acc = field state.env 2 } start
    | ENVACC3                   ->
      c None + eval bytecode primitives { next with acc = field state.env 3 } start
    | ENVACC4                   ->
      c None + eval bytecode primitives { next with acc = field state.env 4 } start
    | ENVACC n                   ->
      c None + eval bytecode primitives { next with acc = field state.env n } start
    | PUSHENVACC1               ->
      Mlstack.push state.stack state.acc;
      let acc = field state.env 1 in
      c None + eval bytecode primitives { next with acc = acc } start
    | PUSHENVACC2               ->
      Mlstack.push state.stack state.acc;
      c None + eval bytecode primitives { next with acc = field state.env 2 } start
    | PUSHENVACC3               ->
      Mlstack.push state.stack state.acc;
      c None + eval bytecode primitives { next with acc = field state.env 3 } start
    | PUSHENVACC4               ->
      Mlstack.push state.stack state.acc;
      c None + eval bytecode primitives { next with acc = field state.env 4 } start
    | PUSHENVACC n               ->
      Mlstack.push state.stack state.acc;
      c None + eval bytecode primitives { next with acc = field state.env n } start
    | PUSH_RETADDR ptr ->
      Mlstack.push state.stack (Int state.extraArgs);
      Mlstack.push state.stack state.env;
      Mlstack.push state.stack (Ptr ptr);
      c (Some ptr) + eval bytecode primitives next start
    | APPLY n ->
      c None + eval bytecode primitives { next with pc = ptr_of_value state.acc ;
                       env = state.acc ;
                       extraArgs = n - 1 } start
    | APPLY1 ->
      let arg = Mlstack.pop state.stack in
      Mlstack.push state.stack (Int state.extraArgs);
      Mlstack.push state.stack state.env;
      Mlstack.push state.stack (Ptr (state.pc + 1));
      Mlstack.push state.stack arg;
      c None + eval bytecode primitives { next with pc = ptr_of_value state.acc ;
                       env = state.acc ;
                       extraArgs = 0 } start
    | APPLY2 ->
      let arg1 = Mlstack.pop state.stack in
      let arg2 = Mlstack.pop state.stack in
      Mlstack.push state.stack (Int state.extraArgs);
      Mlstack.push state.stack state.env;
      Mlstack.push state.stack (Ptr (state.pc + 1));
      Mlstack.push state.stack arg2;
      Mlstack.push state.stack arg1;
      c None + eval bytecode primitives { next with pc = ptr_of_value state.acc ;
                                env = state.acc ;
                                extraArgs = 1 } start
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
      c None + eval bytecode primitives { next with pc = ptr_of_value state.acc ;
                                env = state.acc ;
                                extraArgs = 2 } start
    | APPTERM (n, s)  ->
      for i = 0 to n - 1 do
        let arg = Mlstack.peek state.stack (n- i - 1) in
        Mlstack.set state.stack (s - i - 1) arg
      done;
      Mlstack.popn state.stack (s - n);
      let extraArgs = state.extraArgs + (n - 1) in
      let env = state.acc in
      c None + eval bytecode primitives { next with pc = ptr_of_value state.acc ;
                                env = env ;
                                extraArgs = extraArgs } start
    | APPTERM1 n ->
      let arg = Mlstack.peek state.stack 0 in
      Mlstack.popn state.stack n;
      Mlstack.push state.stack arg;
      let env = state.acc in
      c None + eval bytecode primitives { next with pc = ptr_of_value state.acc ;
                                    env = env} start
    | APPTERM2 n ->
      (* Printf.printf "APPTERM2 %d" n; *)
      let arg1 = Mlstack.peek state.stack 0 in
      let arg2 = Mlstack.peek state.stack 1 in
      Mlstack.popn state.stack n;
      Mlstack.push state.stack arg2;
      Mlstack.push state.stack arg1;
      let env = state.acc in
      let extraArgs = state.extraArgs + 1 in
      c None + eval bytecode primitives { next with pc = ptr_of_value state.acc;
                                    env = env ; extraArgs =
                                                  extraArgs } start
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
      c None + eval bytecode primitives { next with pc = ptr_of_value state.acc ;
                                    env = env ;
                                    extraArgs = extraArgs } start
    | RETURN n ->
      Mlstack.popn state.stack n;
      if (state.extraArgs = 0) then
        (
          let pc = ptr_of_value (Mlstack.pop state.stack) in
          let env = Mlstack.pop state.stack in
          let extraArgs = int_of_value (Mlstack.pop state.stack) in
          c None + eval bytecode primitives { next with pc ; env ; extraArgs} start
        )
      else (
        let pc = ptr_of_value state.acc in
        let env = state.acc in
        let extraArgs = state.extraArgs - 1 in
        c None + eval bytecode primitives { next with pc; env ; extraArgs} start
      )
    | RESTART ->
      let blk = env_of_closure state.env in
      let n = Array.length blk - 1 in
      (* Printf.printf "--> NARGS = %d" n; *)
      for i = n downto 1 do
        Mlstack.push state.stack blk.(i)
      done;
      let extraArgs = state.extraArgs + n in
      let env = blk.(0) in
      c None + eval bytecode primitives { next with env ; extraArgs} start
    | GRAB n ->
      if state.extraArgs >= n then
        let extraArgs = state.extraArgs - n in
        c None + eval bytecode primitives { next with extraArgs = extraArgs } start
      else
        (* failwith "alloc"; *)
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
        c None + eval bytecode primitives { next with acc ; pc ; trapSp = sp;  env ; extraArgs } start
    | CLOSURE (n,ptr) ->
      nb_blocks := !nb_blocks + n + 1;
      let a = Array.make n (state.acc) in
      for i = 1 to n - 1 do
        a.(i) <- Mlstack.pop state.stack;
      done;
      c (Some n) + eval bytecode primitives { next with acc = Closure(Ptr ptr,a) } start
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
      c None + eval bytecode primitives {next with acc = acc } start
    | OFFSETCLOSUREM2           ->
      let acc = offsetclosure state.env (-2) in
      c None + eval bytecode primitives { next with acc} start
    | OFFSETCLOSURE0            ->
      let acc = offsetclosure state.env 0 in
      c None + eval bytecode primitives { next with acc} start
    | OFFSETCLOSURE2            ->
      let acc = offsetclosure state.env 2 in
      c None + eval bytecode primitives { next with acc} start
    | OFFSETCLOSURE n           ->
      let acc = offsetclosure state.env n in
      c (Some n) + eval bytecode primitives { next with acc} start
    | PUSHOFFSETCLOSUREM2       ->
      Mlstack.push state.stack state.acc;
      let acc = offsetclosure state.env (-2) in
      c None + eval bytecode primitives { next with acc} start
    | PUSHOFFSETCLOSURE0        ->
      Mlstack.push state.stack state.acc;
      let acc = offsetclosure state.env 0 in
      c None + eval bytecode primitives { next with acc} start
    | PUSHOFFSETCLOSURE2        ->
      Mlstack.push state.stack state.acc;
      let acc = offsetclosure state.env 2 in
      c None + eval bytecode primitives { next with acc } start
    | PUSHOFFSETCLOSURE n       ->
      Mlstack.push state.stack state.acc;
      let acc = offsetclosure state.env n
      in c (Some n) + eval bytecode primitives { next with acc } start
    | GETGLOBAL n               ->
      let acc = state.global.(n) in
      c (Some n) + eval bytecode primitives { next with acc = acc } start
    | PUSHGETGLOBAL n           ->
      Mlstack.push state.stack state.acc;
      let acc = state.global.(n) in
      c (Some n) + eval bytecode primitives { next with acc = acc } start
    | GETGLOBALFIELD (n, p)     ->
      let acc = field state.global.(n) p in
      c (Some n) + eval bytecode primitives { next with acc = acc } start
    | PUSHGETGLOBALFIELD (n, p) ->
      Mlstack.push state.stack state.acc;
      let acc = field state.global.(n) p in
      c (Some n ) + eval bytecode primitives { next with acc = acc } start
    | SETGLOBAL n               ->
      state.global.(n) <- state.acc ;
      c (Some n) + eval bytecode primitives next start
    | ATOM0                     ->
      let blk = Block (0, [||]) in
      c None + eval bytecode primitives {next with acc = blk } start
    | ATOM tag                  ->
      let blk = Block (tag, [||]) in
      c None + eval bytecode primitives { next with acc = blk } start
    | PUSHATOM0                 ->
      Mlstack.push state.stack state.acc;
      let blk = Block (0 , [||]) in
      c None + eval bytecode primitives { next with acc = blk } start
    | PUSHATOM tag              ->
      Mlstack.push state.stack state.acc;
      let blk = Block (0, [||]) in
      c None + eval bytecode primitives { next with acc = blk } start
    | MAKEBLOCK (tag, sz)       ->
      nb_blocks := !nb_blocks + sz;
      let a = Array.make sz Dummy in
      let blk = Block (tag, a) in
      a.(0) <- state.acc;
      for i = 1 to sz - 1 do
        a.(i) <- Mlstack.pop state.stack;
      done;
      c None + eval bytecode primitives { next with acc = blk } start
    | MAKEBLOCK1 tag            ->
      nb_blocks := !nb_blocks + 1;
      let a = Array.make 1 (state.acc) in
      let blk = Block (tag, a) in
      c None + eval bytecode primitives { next with acc = blk } start
    | MAKEBLOCK2 tag            ->
      nb_blocks := !nb_blocks + 2;
      let a = Array.make 2 (state.acc) in
      let blk = Block (tag, a) in
      a.(1) <- Mlstack.pop state.stack;
      c None + eval bytecode primitives { next with acc = blk } start
    | MAKEBLOCK3 tag            ->
      nb_blocks := !nb_blocks + 3;
      let a = Array.make 3 (state.acc) in
      let blk = Block (tag, a) in
      a.(1) <- Mlstack.pop state.stack;
      a.(2) <- Mlstack.pop state.stack;
      c None + eval bytecode primitives { next with acc = blk } start
    | MAKEFLOATBLOCK n         ->
      nb_blocks := !nb_blocks + n;
      let a = Array.make n Dummy in
      let blk = Block (Obj.double_array_tag,a) in
      a.(0) <- state.acc;
      Mlstack.popn state.stack (n-1);
      c None + eval bytecode primitives { next with acc = blk } start
    | GETFIELD0                 ->
      let acc = field state.acc 0 in
      c None + eval bytecode primitives { next with acc = acc } start
    | GETFIELD1                 ->
      let acc = field state.acc 1 in
      c None + eval bytecode primitives { next with acc = acc } start
    | GETFIELD2                 ->
      let acc = field state.acc 2 in
      c None + eval bytecode primitives { next with acc = acc } start
    | GETFIELD3                 ->
      let acc = field state.acc 3 in
      c None + eval bytecode primitives { next with acc = acc } start
    | GETFIELD n                ->
      let acc = field state.acc n in
      c None + eval bytecode primitives { next with acc = acc } start
    | GETFLOATFIELD i ->
      let acc = Dummy in
      c None + eval bytecode primitives { next with acc = acc } start
    | SETFIELD0                 ->
      set_field state.acc 0 (Mlstack.pop state.stack);
      let acc = Dummy in
      c None + eval bytecode primitives { next with acc = acc } start
    | SETFIELD1                 ->
      set_field state.acc 1 (Mlstack.pop state.stack);
      let acc = Dummy in
      c None + eval bytecode primitives { next with acc = acc } start
    | SETFIELD2                 ->
      set_field state.acc 2 (Mlstack.pop state.stack);
      let acc = Dummy in
      c None + eval bytecode primitives { next with acc = acc } start
    | SETFIELD3                 ->
      set_field state.acc 3 (Mlstack.pop state.stack);
      let acc = Dummy in
      c None + eval bytecode primitives { next with acc = acc } start
    | SETFIELD n                 ->
      set_field state.acc n (Mlstack.pop state.stack);
      let acc = Dummy in
      c None + eval bytecode primitives { next with acc = acc } start
    | SETFLOATFIELD n           ->
      let acc = Dummy in
      c None + eval bytecode primitives { next with acc = acc } start
    | VECTLENGTH                ->
      let acc = match state.acc with Block(n,v) -> Int n | _ -> Dummy  in
      c None + eval bytecode primitives { next with acc = acc } start
    | GETVECTITEM               ->
       let param = Mlstack.pop state.stack in
       let acc = match state.acc, param with
         | Block(n,v), Int d  -> v.(d)
         | _ -> Dummy
       in
      c None + eval bytecode primitives { next with acc = acc } start
    | SETVECTITEM               ->
      let param1 =  Mlstack.pop state.stack in
      let param2 =  Mlstack.pop state.stack in
      let acc = match state.acc, param1 with
        | Block (n,v) , Int d1 -> (v.(d1) <- param2); Unit
        | _ -> Dummy
      in
      c None + eval bytecode primitives { next with acc = acc } start
    | GETBYTESCHAR             ->
      ignore @@ Mlstack.pop state.stack;
      let acc = Dummy in
      c None + eval bytecode primitives { next with acc = acc } start
    | SETBYTESCHAR             ->
      ignore @@ Mlstack.pop state.stack;
      ignore @@ Mlstack.pop state.stack;
      let acc = Dummy in
      c None + eval bytecode primitives { next with acc = acc } start
    | BRANCH b -> c (Some b) + eval bytecode primitives { next with pc = b } start
    | BRANCHIF ptr              ->
      begin
        match state.acc with
        | Int 0 -> c (Some ptr) + eval bytecode primitives next start
        | Int _ -> c (Some ptr) + eval bytecode primitives { next with pc = ptr } start
        | Dummy ->
           let stack = Mlstack.load state.stack in
           print_string "begin of then branch \n";
           incr level;
           let p1 =  (eval bytecode primitives {next with pc = ptr} start) in
           print_string "end of then branch \n";
           decr level;
           Mlstack.store state.stack stack;
           print_string "begin of else branch \n";
           let p2 = eval bytecode primitives next start in
           c (Some ptr) + max p1 p2
        | _ -> failwith "BRANCHIF : not an int/?"
      end
    | BRANCHIFNOT ptr           ->
      begin
        match state.acc with
        | Dummy ->
           let stack = Mlstack.load state.stack in
           print_string "begin of then branch \n";
           incr level;
           let p1 = eval bytecode primitives {next with pc = ptr} start in
           print_string "end of then branch \n";
           decr level;
           Mlstack.store state.stack stack;
           print_string "begin of else branch \n";
           let p2 = eval bytecode primitives next start in
           print_string "end of else branch \n";
           c (Some ptr) +  max p1 p2
        | Int 0 -> c (Some ptr) + eval bytecode primitives { next with pc = ptr } start
        | _ -> c (Some ptr) + eval bytecode primitives next start
      end
    | SWITCH (n, ptrs)          ->
      begin
        match state.acc with
        | Int v ->
          c (Some n) + eval bytecode primitives { next with pc = ptrs.(v) } start
        | Block (tag,b) ->  c (Some n) + eval bytecode primitives { next with pc = ptrs.(tag + (n land 0xFFFF)) } start
        | _ -> failwith "?"
      end
    | BOOLNOT                   ->
      let acc = match state.acc with
        | Int 0 -> Int 1
        | Int 1 -> Int 0
        | Dummy -> Dummy
        | _ -> print_state state 0 bytecode; failwith "not a bool"
      in
      c None + eval bytecode primitives { next with acc } start
    | PUSHTRAP ptr              ->
      Mlstack.push state.stack (Int state.extraArgs);
      Mlstack.push state.stack state.env;
      Mlstack.push state.stack (Int state.trapSp);
      Mlstack.push state.stack (Ptr ptr);
      c None + eval bytecode primitives { next with trapSp = Mlstack.length state.stack } start
    | POPTRAP                   ->
      ignore @@ Mlstack.pop state.stack;
      let trapSp = int_of_value (Mlstack.pop state.stack) in
      ignore @@ Mlstack.pop state.stack;
      ignore @@ Mlstack.pop state.stack;
      c None + eval bytecode primitives { next with trapSp = trapSp } start
    | C_CALL1 idx ->
      let acc = eval_primitive_1 primitives.(idx) state.acc in
      if primitives.(idx) = "begin_loop" then
        begin
          Printf.printf " >> BEGINNING LOOP << \n";
          c None + eval bytecode primitives {next with acc} true
        end
      else if primitives.(idx) = "end_loop" then
        begin
          Printf.printf " >> ENDING LOOP << \n";
          c None + 0
        end
      else
        c None + eval bytecode primitives {next with acc} start
    | C_CALL2 idx ->
      (* let peek =  Mlstack.peek state.stack in *)
       let name = primitives.(idx) in
       let param1 = state.acc in
       let param2 = Mlstack.pop state.stack in
       let acc = eval_primitive_2 name param1 param2 in
          c None + eval bytecode primitives { next with acc } start
    | C_CALL3 idx ->
      let acc = Dummy in
      ignore @@ Mlstack.pop state.stack;
      ignore @@ Mlstack.pop state.stack;
      c None + eval bytecode primitives { next with acc } start
    | C_CALL4 idx ->
      let acc = Dummy in
      ignore @@ Mlstack.pop state.stack;
      ignore @@ Mlstack.pop state.stack;
      ignore @@ Mlstack.pop state.stack;
      c None + eval bytecode primitives { next with acc } start
    | C_CALL5 idx ->
      let acc = Dummy in
      ignore @@ Mlstack.pop state.stack;
      ignore @@ Mlstack.pop state.stack;
      ignore @@ Mlstack.pop state.stack;
      ignore @@ Mlstack.pop state.stack;
      c None + eval bytecode primitives { next with acc } start
    | C_CALLN (narg, idx)       ->
      let acc = Dummy in
      Mlstack.push state.stack state.acc;
      Mlstack.popn state.stack narg;
      c None + eval bytecode primitives { next with acc } start
    | RAISE | RERAISE | RAISE_NOTRACE -> raise Exit
    | CHECK_SIGNALS             ->
      c None + eval bytecode primitives next start
    | CONST0                    -> c None + eval bytecode primitives { next with acc = Int 0 } start
    | CONST1                    -> c None + eval bytecode primitives { next with acc = Int 1 } start
    | CONST2                    -> c None + eval bytecode primitives { next with acc = Int 2 } start
    | CONST3                    -> c None + eval bytecode primitives { next with acc = Int 3 } start
    | CONSTINT n                -> c (Some n) + eval bytecode primitives { next with acc = Int n } start
    | PUSHCONST0                ->
      Mlstack.push state.stack state.acc;
      c None + eval bytecode primitives { next with acc = Int 0 } start
    | PUSHCONST1                ->
      Mlstack.push state.stack state.acc;
      c None + eval bytecode primitives { next with acc = Int 1 } start
    | PUSHCONST2                ->
      Mlstack.push state.stack state.acc;
      c None + eval bytecode primitives { next with acc = Int 2 } start
    | PUSHCONST3                ->
      Mlstack.push state.stack state.acc;
      c None + eval bytecode primitives { next with acc = Int 3 } start
    | PUSHCONSTINT n            ->
      Mlstack.push state.stack state.acc;
      c (Some n) + eval bytecode primitives { next with acc = Int n } start
    | NEGINT                    ->
      let acc = int_op (fun x y -> -x ) state.acc (Int 0) in
      c None + eval bytecode primitives { next with acc = acc } start
    | ADDINT                    ->
      let v = (Mlstack.pop state.stack) in
      let acc = int_op (fun x y -> x+y ) state.acc v in
      c None + eval bytecode primitives { next with acc } start
    | SUBINT                    ->
      let v = (Mlstack.pop state.stack) in
      let acc = int_op (fun x y -> x-y ) state.acc v in
      c None + eval bytecode primitives { next with acc } start
    | MULINT                    ->
      let v = (Mlstack.pop state.stack) in
      let acc = int_op (fun x y -> x*y ) state.acc v in
      c None + eval bytecode primitives { next with acc } start
    | DIVINT                    ->
      let v = (Mlstack.pop state.stack) in
      let acc = int_op (fun x y -> x/y ) state.acc v in
      c None + eval bytecode primitives { next with acc } start
    | MODINT                    ->
      let v = (Mlstack.pop state.stack) in
      let acc = int_op (fun x y -> x mod y ) state.acc v in
      c None + eval bytecode primitives { next with acc } start
    | ANDINT                    ->
      let v = (Mlstack.pop state.stack) in
      let acc = int_op (fun x y -> x land y ) state.acc v in
      c None + eval bytecode primitives { next with acc } start
    | ORINT                     ->
      let v = (Mlstack.pop state.stack) in
      let acc = int_op (fun x y -> x lor y ) state.acc v in
      c None + eval bytecode primitives { next with acc } start
    | XORINT                    ->
      let v = (Mlstack.pop state.stack) in
      let acc = int_op (fun x y -> x lxor y ) state.acc v in
      c None + eval bytecode primitives { next with acc } start
    | LSLINT                    ->
      let v = (Mlstack.pop state.stack) in
      let acc = int_op (fun x y -> x lsl y ) state.acc v in
      c None + eval bytecode primitives { next with acc } start
    | LSRINT                    ->
      let v = (Mlstack.pop state.stack) in
      let acc = int_op (fun x y -> x lsr y ) state.acc v in
      c None + eval bytecode primitives { next with acc } start
    | ASRINT                    ->
      let v = (Mlstack.pop state.stack) in
      let acc = int_op (fun x y -> x asr y ) state.acc v in
      c None + eval bytecode primitives { next with acc } start
    | EQ                        ->
      let v = (Mlstack.pop state.stack) in
      let acc = int_op (fun x y -> if x = y then 1 else 0 ) state.acc v in
      c None + eval bytecode primitives { next with acc } start
    | NEQ ->
      let v = (Mlstack.pop state.stack) in
      let acc = int_op (fun x y -> if x <> y then 1 else 0 ) state.acc v in
      c None + eval bytecode primitives { next with acc } start
    | LTINT                     ->
      let v = (Mlstack.pop state.stack) in
      let acc = int_op (fun x y -> if x < y then 1 else 0) state.acc v in
      c None + eval bytecode primitives { next with acc } start
    | LEINT                     ->
      let v = (Mlstack.pop state.stack) in
      let acc = int_op (fun x y -> if x <= y then 1 else 0 ) state.acc v in
      c None + eval bytecode primitives { next with acc } start
    | GTINT                     ->
      let v = (Mlstack.pop state.stack) in
      let acc = int_op (fun x y -> if x > y then 1 else 0 ) state.acc v in
      c None + eval bytecode primitives { next with acc } start
    | GEINT                     ->
      let v = (Mlstack.pop state.stack) in
      let acc = int_op (fun x y -> if x >= y then 1 else 0) state.acc v in
      c None + eval bytecode primitives { next with acc } start
    | OFFSETINT n               ->
      let acc = int_op (fun x y -> x + y ) state.acc (Int n) in
      c (Some n) + eval bytecode primitives { next with acc } start
    | OFFSETREF n               ->
      begin
        match state.acc with
        | Block (tag,t) -> t.(0) <- int_op (fun x y -> x + y) t.(0) (Int n)
        | _ -> failwith "not a block"
      end;
      c None + eval bytecode primitives next start
    | ISINT                     ->
      let acc = match state.acc with
        | Int i -> Int 1
        | _ -> Int 0
      in
      c None + eval bytecode primitives { next with acc } start
    | GETMETHOD                 ->
      let acc = Dummy in
      c None + eval bytecode primitives { next with acc } start
    | BEQ (n, ptr)              ->
       failwith "beq";
      begin
        match state.acc with
        | Dummy ->
          c None + eval bytecode primitives  { next with pc = ptr } start;
          c None + eval bytecode primitives next start
        | Int v -> if n = v then
            c None + eval bytecode primitives  { next with pc = ptr } start
          else
            c None + eval bytecode primitives next start
        | _ -> failwith "wrong accumulator"
      end
    | BNEQ (n, ptr)             ->
      begin
        match state.acc with
        | Dummy ->
           let stack = Mlstack.load state.stack in
           print_string "begin of then branch \n";
           incr level;
           let p1 = eval bytecode primitives {next with pc = ptr} start in
           print_string "end of then branch \n";
           decr level;
           Mlstack.store state.stack stack;
           print_string "begin of else branch \n";
           let p2 = eval bytecode primitives next start in
           print_string "end of else branch \n";
           c (Some ptr) +  max p1 p2
          (* c (Some ptr) + eval bytecode primitives  { next with pc = ptr } start; *)
          (* c (Some ptr) + eval bytecode primitives next start *)
        | Int v -> if n <> v then
            c (Some ptr) + eval bytecode primitives  { next with pc = ptr } start
          else
            c (Some ptr) + eval bytecode primitives next start
        | _ -> failwith "wrong accumulator"
      end
    | BLTINT (n, ptr)           ->
       failwith "bltint";
      begin
        match state.acc with
        | Dummy ->
          c None + eval bytecode primitives  { next with pc = ptr } start;
          c None + eval bytecode primitives next start
        | Int v -> if n < v then
            c None + eval bytecode primitives  { next with pc = ptr } start
          else
            c None + eval bytecode primitives next start
        | _ -> failwith "wrong accumulator"
      end
    | BLEINT (n, ptr)           ->
       begin
        match state.acc with
        | Dummy ->
          let stack = Mlstack.load state.stack in
           print_string "begin of then branch \n";
           incr level;
           let p1 = eval bytecode primitives {next with pc = ptr} start in
           print_string "end of then branch \n";
           decr level;
           Mlstack.store state.stack stack;
           print_string "begin of else branch \n";
           let p2 = eval bytecode primitives next start in
           print_string "end of else branch \n";
           c (Some ptr) +  max p1 p2
        | Int v -> if n <= v then
            c (Some ptr) + eval bytecode primitives  { next with pc = ptr } start
          else
            c (Some ptr) + eval bytecode primitives next start
        | _ -> failwith "wrong accumulator"
      end
    | BGTINT (n, ptr)           ->
      begin
        match state.acc with
        | Dummy ->

          let stack = Mlstack.load state.stack in
           print_string "begin of then branch \n";
           incr level;
           let p1 = eval bytecode primitives {next with pc = ptr} start in
           print_string "end of then branch \n";
           (* decr level; *)
           Mlstack.store state.stack stack;
           print_string "begin of else branch \n";
           let p2 = eval bytecode primitives next start in
           print_string "end of else branch \n";
           c (Some n) +  max p1 p2
        | Int v -> if n > v then
            c (Some n) + eval bytecode primitives  { next with pc = ptr } start
          else
            c (Some n) + eval bytecode primitives next start
        | _ -> failwith "wrong accumulator"
      end
    | BGEINT (n, ptr) ->
       failwith "bgeint";
      begin
        match state.acc with
        | Dummy ->
          c None + eval bytecode primitives  { next with pc = ptr } start;
          c None + eval bytecode primitives next start
        | Int v -> if n >= v then
            c None + eval bytecode primitives  { next with pc = ptr } start
          else
            c None + eval bytecode primitives next start
        | _ -> failwith "wrong accumulator"
      end
    | ULTINT ->
      let n = int_op (fun x y -> x + min_int) state.acc (Int 0) in
      let p = int_op (fun x y -> x + min_int) (Mlstack.pop state.stack) (Int 0) in
      let acc = int_op (fun x y -> if x < y then 1 else 0) n p in
      c None + eval bytecode primitives { next with acc } start
    | UGEINT ->
      let n = int_op (fun x y -> x + min_int) state.acc (Int 0) in
      let p = int_op (fun x y -> x + min_int) (Mlstack.pop state.stack) (Int 0) in
      let acc = int_op (fun x y -> if x >= y then 1 else 0) n p in
      c None + eval bytecode primitives { next with acc } start
    (* | BULTINT (n,ptr) -> *)
    (*   c None + eval bytecode primitives { next with pc = ptr } start; *)
    (*   c None + eval bytecode primitives next *)
    (* | BUGEINT (n, ptr) -> *)
    (*   c None + eval bytecode primitives { next with pc = ptr }; *)
    (*   c None + eval bytecode primitives next *)
    | GETPUBMET (tag, cache)    -> failwith "todo getpubmet"
    | GETDYNMET                 -> failwith "todo getdynmet"
    | STOP                      -> 0
    | EVENT                     -> failwith "todo event"
    | BREAK                     -> failwith "todo break"
    | _ -> failwith "unknown instr"




(* (\* pas tail rec ... *\)
 * let count_loop_0 level bytecode state prims :int =
 * let rec count_loop level state switch cpt =
 *   print_state state level bytecode;
 *   let inst = bytecode.(state.pc) in
 *   let next = { state with pc = state.pc None + 1 } in
 *   match inst with
 *   | ACC0 -> let i = Mlstack.peek state.stack 0 in
 *     count_loop level { next with acc = i } switch (cpt + cost inst switch None)
 *   | ACC1 -> let i = Mlstack.peek state.stack 1 in
 *     count_loop level { next with acc = i } switch (cpt + cost inst switch None)
 *   | ACC2 -> let i = Mlstack.peek state.stack 2 in
 *     count_loop level { next with acc = i } switch (cpt + cost inst switch None)
 *   | ACC3 -> let i = Mlstack.peek state.stack 3 in
 *     count_loop level { next with acc = i } switch (cpt + cost inst switch None)
 *   | ACC4 -> let i = Mlstack.peek state.stack 4 in
 *     count_loop level { next with acc = i } switch (cpt + cost inst switch None)
 *   | ACC5 -> let i = Mlstack.peek state.stack 5 in
 *     count_loop level { next with acc = i } switch (cpt + cost inst switch None)
 *   | ACC6 -> let i = Mlstack.peek state.stack 6 in
 *     count_loop level { next with acc = i } switch (cpt + cost inst switch None)
 *   | ACC7 -> let i = Mlstack.peek state.stack 7 in
 *     count_loop level { next with acc = i } switch (cpt + cost inst switch None)
 *   | ACC n -> let i = Mlstack.peek state.stack n in
 *     count_loop level { next with acc = i } switch (cpt + cost inst switch None)
 *   | PUSH -> Mlstack.push state.stack state.acc;
 *     count_loop level next switch (cpt + cost inst switch None)
 *   | PUSHACC0 ->
 *     Mlstack.push state.stack state.acc;
 *     let i = Mlstack.peek state.stack 0 in
 *     count_loop level { next with acc = i } switch (cpt + cost inst switch None)
 *   | PUSHACC1 ->
 *     Mlstack.push state.stack state.acc;
 *     let i = Mlstack.peek state.stack 1 in
 *     count_loop level { next with acc = i } switch (cpt + cost inst switch None)
 *   | PUSHACC2 ->
 *     Mlstack.push state.stack state.acc;
 *     let i = Mlstack.peek state.stack 2 in
 *     count_loop level { next with acc = i } switch (cpt + cost inst switch None)
 *   | PUSHACC3 ->
 *     Mlstack.push state.stack state.acc;
 *     let i = Mlstack.peek state.stack 3 in
 *     count_loop level { next with acc = i } switch (cpt + cost inst switch None)
 *   | PUSHACC4 ->
 *     Mlstack.push state.stack state.acc;
 *     let i = Mlstack.peek state.stack 4 in
 *     count_loop level { next with acc = i } switch (cpt + cost inst switch None)
 *   | PUSHACC5 ->
 *     Mlstack.push state.stack state.acc;
 *     let i = Mlstack.peek state.stack 5 in
 *     count_loop level { next with acc = i } switch (cpt + cost inst switch None)
 *   | PUSHACC6 ->
 *     Mlstack.push state.stack state.acc;
 *     let i = Mlstack.peek state.stack 6 in
 *     count_loop level { next with acc = i } switch (cpt + cost inst switch None)
 *   | PUSHACC7 ->
 *     Mlstack.push state.stack state.acc;
 *     let i = Mlstack.peek state.stack 7 in
 *     count_loop level { next with acc = i } switch (cpt + cost inst switch None)
 *   | PUSHACC n ->
 *     Mlstack.push state.stack state.acc;
 *     let i = Mlstack.peek state.stack n in
 *     count_loop level { next with acc = i } switch (cpt + cost inst switch None)
 *   | POP n -> Mlstack.popn state.stack n;
 *     count_loop level next switch (cpt + cost inst switch None)
 *   | ASSIGN n -> Mlstack.set state.stack n state.acc;
 *     count_loop level { next with acc = Dummy } switch (cpt + cost inst switch None)
 *   | ENVACC1                   ->
 *     count_loop level { next with acc = field state.env 1 } switch (cpt + cost inst switch None)
 *   | ENVACC2                   ->
 *     count_loop level { next with acc = field state.env 2 } switch (cpt + cost inst switch None)
 *   | ENVACC3                   ->
 *     count_loop level { next with acc = field state.env 3 } switch (cpt + cost inst switch None)
 *   | ENVACC4                   ->
 *     count_loop level { next with acc = field state.env 4 } switch (cpt + cost inst switch None)
 *   | ENVACC n                   ->
 *     count_loop level { next with acc = field state.env n } switch (cpt + cost inst switch None)
 *   | PUSHENVACC1               ->
 *     Mlstack.push state.stack state.acc;
 *     let acc = field state.env 1 in
 *     count_loop level { next with acc = acc } switch (cpt + cost inst switch None)
 *   | PUSHENVACC2               ->
 *     Mlstack.push state.stack state.acc;
 *     count_loop level { next with acc = field state.env 2 } switch (cpt + cost inst switch None)
 *   | PUSHENVACC3               ->
 *     Mlstack.push state.stack state.acc;
 *     count_loop level { next with acc = field state.env 3 } switch (cpt + cost inst switch None)
 *   | PUSHENVACC4               ->
 *     Mlstack.push state.stack state.acc;
 *     count_loop level { next with acc = field state.env 4 } switch (cpt + cost inst switch None)
 *   | PUSHENVACC n               ->
 *     Mlstack.push state.stack state.acc;
 *     count_loop level { next with acc = field state.env n } switch (cpt + cost inst switch None)
 *   | PUSH_RETADDR ptr ->
 *     Mlstack.push state.stack (Int state.extraArgs);
 *     Mlstack.push state.stack state.env;
 *     Mlstack.push state.stack (Ptr ptr);
 *     count_loop level next switch (cpt + cost inst switch None)
 *   | APPLY n ->
 *     count_loop (level+1) { next with pc = ptr_of_value state.acc ;
 *                                            env = state.acc ;
 *                                            extraArgs = n - 1 } switch (cpt + cost inst switch None)
 *   | APPLY1 ->
 *     let arg = Mlstack.pop state.stack in
 *     Mlstack.push state.stack (Int state.extraArgs);
 *     Mlstack.push state.stack state.env;
 *     Mlstack.push state.stack (Ptr (state.pc None + 1));
 *     Mlstack.push state.stack arg;
 *     count_loop (level+1) { next with pc = ptr_of_value state.acc ;
 *                                            env = state.acc ;
 *                                            extraArgs = 0 } switch (cpt + cost inst switch None)
 *   | APPLY2 ->
 *     let arg1 = Mlstack.pop state.stack in
 *     let arg2 = Mlstack.pop state.stack in
 *     Mlstack.push state.stack (Int state.extraArgs);
 *     Mlstack.push state.stack state.env;
 *     Mlstack.push state.stack (Ptr (state.pc None + 1));
 *     Mlstack.push state.stack arg2;
 *     Mlstack.push state.stack arg1;
 *     count_loop level { next with pc = ptr_of_value state.acc ;
 *                                            env = state.acc ;
 *                                            extraArgs = 1 } switch (cpt + cost inst switch None)
 *   | APPLY3 ->
 *     let arg1 = Mlstack.pop state.stack in
 *     let arg2 = Mlstack.pop state.stack in
 *     let arg3 = Mlstack.pop state.stack in
 *     Mlstack.push state.stack (Int state.extraArgs);
 *     Mlstack.push state.stack state.env;
 *     Mlstack.push state.stack (Ptr (state.pc None + 1) );
 *     Mlstack.push state.stack arg3;
 *     Mlstack.push state.stack arg2;
 *     Mlstack.push state.stack arg1;
 *     count_loop level { next with pc = ptr_of_value state.acc ;
 *                                            env = state.acc ;
 *                                            extraArgs = 2 } switch (cpt + cost inst switch None)
 *   | APPTERM (n, s)  ->
 *     for i = 0 to n - 1 do
 *       let arg = Mlstack.peek state.stack (n- i - 1) in
 *       Mlstack.set state.stack (s - i - 1) arg
 *     done;
 *     Mlstack.popn state.stack (s - n);
 *     let extraArgs = state.extraArgs + (n - 1) in
 *     let env = state.acc in
 *     count_loop level { next with pc = ptr_of_value state.acc ;
 *                                            env = env ;
 *                                            extraArgs = extraArgs } switch (cpt + cost inst switch (Some n))
 *   | APPTERM1 n ->
 *     let arg = Mlstack.top state.stack in
 *     Mlstack.popn state.stack n;
 *     (\* Printf.printf "---> %d <<<---" (int_of_value arg); *\)
 *     Mlstack.push state.stack arg;
 *     let env = state.acc in
 *     count_loop level { next with pc = ptr_of_value state.acc ;
 *                                            env = env} switch (cpt + cost inst switch None)
 *   | APPTERM2 n ->
 *
 *     let arg1 = Mlstack.pop state.stack in
 *     let arg2 = Mlstack.pop state.stack  in
 *     Mlstack.popn state.stack (n-2);
 *     Mlstack.push state.stack arg2;
 *     Mlstack.push state.stack arg1;
 *     let env = state.acc in
 *     let extraArgs = state.extraArgs + 1 in
 *     count_loop level { next with pc = ptr_of_value state.acc;
 *                                  env = env ;
 *                                  extraArgs = extraArgs } switch (cpt + cost inst switch None)
 *   | APPTERM3 n ->
 *     let arg1 = Mlstack.peek state.stack 0 in
 *     let arg2 = Mlstack.peek state.stack 1 in
 *     let arg3 = Mlstack.peek state.stack 2 in
 *     Mlstack.popn state.stack n;
 *     Mlstack.push state.stack arg3;
 *     Mlstack.push state.stack arg2;
 *     Mlstack.push state.stack arg1;
 *     let extraArgs = state.extraArgs + 2 in
 *     let env = state.acc in
 *     count_loop level { next with pc = ptr_of_value state.acc ;
 *                                  env = env ;
 *                                            extraArgs = extraArgs } switch (cpt + cost inst switch None)
 *   | RETURN n ->
 *     Mlstack.popn state.stack n;
 *     if (state.extraArgs = 0) then
 *       (
 *         let pc = ptr_of_value (Mlstack.pop state.stack) in
 *         let env = Mlstack.pop state.stack in
 *         let extraArgs = int_of_value (Mlstack.pop state.stack) in
 *         count_loop (level-1) { next with pc ; env ; extraArgs} switch (cpt + cost inst switch None)
 *       )
 *     else (
 *       let pc = ptr_of_value state.acc in
 *       let env = state.acc in
 *       let extraArgs = state.extraArgs - 1 in
 *       count_loop level { next with pc; env ; extraArgs} switch (cpt + cost inst switch None)
 *     )
 *   | RESTART ->
 *     (\* nargs = Wosize_val(env) -1  *\)
 *     let blk = env_of_closure state.env in
 *     let n = Array.length blk - 1 in
 *     for i = n downto 1 do
 *       Mlstack.push state.stack blk.(i)
 *     done;
 *     let extraArgs = state.extraArgs + n in
 *     let env = blk.(0) in
 *     count_loop level { next with env ; extraArgs} switch (cpt + cost inst switch (Some n))
 *   | GRAB n ->
 *     if state.extraArgs >= n then
 *       let extraArgs = state.extraArgs - n in
 *       count_loop level { next with extraArgs = extraArgs } switch (cpt + cost inst switch (None))
 *     else
 *       let a = Array.make (state.extraArgs + 2) (state.acc) in
 *       for i = 1 to state.extraArgs + 1 do
 *         a.(i) <- Mlstack.pop state.stack
 *       done;
 *       let acc = Closure (Ptr (state.pc - 1) , a) in
 *       (\* set_field state.acc 1 state.env; *\)
 *       (\* for i = 0 to state.extraArgs do *\)
 *       (\*   set_field state.acc (i+2) (Mlstack.pop state.stack) *\)
 *       (\* done; *\)
 *       let sp = state.trapSp + state.extraArgs + 1 in
 *       let pc = ptr_of_value (Mlstack.pop state.stack) in
 *       let env = Mlstack.pop state.stack in
 *       let extraArgs = int_of_value (Mlstack.pop state.stack) in
 *       count_loop level { next with acc ; pc ; trapSp = sp;  env ; extraArgs } switch (cpt + cost inst switch (Some n))
 *   | CLOSURE (n,ptr) ->
 *     let a = Array.make n (state.acc) in
 *     for i = 1 to n - 1 do
 *       a.(i) <- Mlstack.pop state.stack;
 *     done;
 *     count_loop level { next with acc = Closure(Ptr ptr,a) } switch (cpt + cost inst switch (Some n))
 *   | CLOSUREREC (f, v, ptr, t)   ->
 *     (\* f = number of functions
 *      * v = number of variables *\)
 *     (\* if v > 0 then Mlstack.push state.stack state.acc; *\)
 *     let blk = Array.make v (state.acc) in
 *     for i = 1 to v - 1 do
 *       blk.(i) <- Mlstack.pop state.stack;
 *     done;
 *     let acc = Closure_rec (Ptr ptr,t,blk,0) in
 *     Mlstack.push state.stack acc;
 *     for i = 1 to Array.length t do
 *       Mlstack.push state.stack (Block (0,[||]));
 *     done;
 *     count_loop level {next with acc = acc } switch (cpt + cost inst switch (Some f))
 *   | OFFSETCLOSUREM2           ->
 *     let acc = offsetclosure state.env (-2) in
 *     count_loop level { next with acc} switch (cpt + cost inst switch None)
 *   | OFFSETCLOSURE0            ->
 *     let acc = offsetclosure state.env 0 in
 *     count_loop level { next with acc} switch (cpt + cost inst switch None)
 *   | OFFSETCLOSURE2            ->
 *     let acc = offsetclosure state.env 2 in
 *     count_loop level { next with acc} switch (cpt + cost inst switch None)
 *   | OFFSETCLOSURE n           ->
 *     let acc = offsetclosure state.env n in
 *     count_loop level { next with acc} switch (cpt + cost inst switch None)
 *   | PUSHOFFSETCLOSUREM2       ->
 *     Mlstack.push state.stack state.acc;
 *     let acc = offsetclosure state.env (-2) in
 *     count_loop level { next with acc} switch (cpt + cost inst switch None)
 *   | PUSHOFFSETCLOSURE0        ->
 *     Mlstack.push state.stack state.acc;
 *     let acc = offsetclosure state.env 0 in
 *     count_loop level { next with acc} switch (cpt + cost inst switch None)
 *   | PUSHOFFSETCLOSURE2        ->
 *     Mlstack.push state.stack state.acc;
 *     let acc = offsetclosure state.env 2 in
 *     count_loop level { next with acc } switch (cpt + cost inst switch None)
 *   | PUSHOFFSETCLOSURE n       ->
 *     Mlstack.push state.stack state.acc;
 *     let acc = offsetclosure state.env n
 *     in count_loop level { next with acc } switch (cpt + cost inst switch None)
 *   | GETGLOBAL n               ->
 *     let acc = state.global.(n) in
 *     count_loop level { next with acc = acc } switch (cpt + cost inst switch None)
 *   | PUSHGETGLOBAL n           ->
 *     Mlstack.push state.stack state.acc;
 *     let acc = state.global.(n) in
 *     count_loop level { next with acc = acc } switch (cpt + cost inst switch None)
 *   | GETGLOBALFIELD (n, p)     ->
 *     let acc = field state.global.(n) p in
 *     count_loop level { next with acc = acc } switch (cpt + cost inst switch None)
 *   | PUSHGETGLOBALFIELD (n, p) ->
 *     Mlstack.push state.stack state.acc;
 *     let acc = field state.global.(n) p in
 *     count_loop level { next with acc = acc } switch (cpt + cost inst switch None)
 *   | SETGLOBAL n               ->
 *     state.global.(n) <- state.acc ;
 *     count_loop level { next with acc = Dummy } switch (cpt + cost inst switch None)
 *   | ATOM0                     ->
 *     let blk = Block (0, [||]) in
 *     count_loop level {next with acc = blk } switch (cpt + cost inst switch None)
 *   | ATOM tag                  ->
 *     let blk = Block (tag, [||]) in
 *     count_loop level { next with acc = blk } switch (cpt + cost inst switch None)
 *   | PUSHATOM0                 ->
 *     Mlstack.push state.stack state.acc;
 *     let blk = Block (0 , [||]) in
 *     count_loop level { next with acc = blk } switch (cpt + cost inst switch None)
 *   | PUSHATOM tag              ->
 *     Mlstack.push state.stack state.acc;
 *     let blk = Block (0, [||]) in
 *     count_loop level { next with acc = blk } switch (cpt + cost inst switch None)
 *   | MAKEBLOCK (tag, sz)       ->
 *     let a = Array.make sz Dummy in
 *     let blk = Block (tag, a) in
 *     a.(0) <- state.acc;
 *     for i = 1 to sz - 1 do
 *       a.(i) <- Mlstack.pop state.stack;
 *     done;
 *     count_loop level { next with acc = blk } switch (cpt + cost inst switch (Some sz))
 *   | MAKEBLOCK1 tag            ->
 *     let a = Array.make 1 (state.acc) in
 *     let blk = Block (tag, a) in
 *     count_loop level { next with acc = blk } switch (cpt + cost inst switch None)
 *   | MAKEBLOCK2 tag            ->
 *     let a = Array.make 2 (state.acc) in
 *     let blk = Block (tag, a) in
 *     a.(1) <- Mlstack.pop state.stack;
 *     count_loop level { next with acc = blk } switch (cpt + cost inst switch None)
 *   | MAKEBLOCK3 tag            ->
 *     let a = Array.make 3 (state.acc) in
 *     let blk = Block (tag, a) in
 *     a.(1) <- Mlstack.pop state.stack;
 *     a.(2) <- Mlstack.pop state.stack;
 *     count_loop level { next with acc = blk } switch (cpt + cost inst switch None)
 *   | MAKEFLOATBLOCK n         ->
 *     let a = Array.make n Dummy in
 *     let blk = Block (Obj.double_array_tag,a) in
 *     a.(0) <- state.acc;
 *     Mlstack.popn state.stack (n-1);
 *     count_loop level { next with acc = blk } switch (cpt + cost inst switch None)
 *   | GETFIELD0                 ->
 *     let acc = field state.acc 0 in
 *     count_loop level { next with acc = acc } switch (cpt + cost inst switch None)
 *   | GETFIELD1                 ->
 *     let acc = field state.acc 1 in
 *     count_loop level { next with acc = acc } switch (cpt + cost inst switch None)
 *   | GETFIELD2                 ->
 *     let acc = field state.acc 2 in
 *     count_loop level { next with acc = acc } switch (cpt + cost inst switch None)
 *   | GETFIELD3                 ->
 *     let acc = field state.acc 3 in
 *     count_loop level { next with acc = acc } switch (cpt + cost inst switch None)
 *   | GETFIELD n                ->
 *     let acc = field state.acc n in
 *     count_loop level { next with acc = acc } switch (cpt + cost inst switch None)
 *   | GETFLOATFIELD i ->
 *     let acc = Dummy in
 *     count_loop level { next with acc = acc } switch (cpt + cost inst switch None)
 *   | SETFIELD0                 ->
 *     set_field state.acc 0 (Mlstack.pop state.stack);
 *     let acc = Dummy in
 *     count_loop level { next with acc = acc } switch (cpt + cost inst switch None)
 *   | SETFIELD1                 ->
 *     set_field state.acc 1 (Mlstack.pop state.stack);
 *     let acc = Dummy in
 *     count_loop level { next with acc = acc } switch (cpt + cost inst switch None)
 *   | SETFIELD2                 ->
 *     set_field state.acc 2 (Mlstack.pop state.stack);
 *     let acc = Dummy in
 *     count_loop level { next with acc = acc } switch (cpt + cost inst switch None)
 *   | SETFIELD3                 ->
 *     set_field state.acc 3 (Mlstack.pop state.stack);
 *     let acc = Dummy in
 *     count_loop level { next with acc = acc } switch (cpt + cost inst switch None)
 *   | SETFIELD n                 ->
 *     set_field state.acc n (Mlstack.pop state.stack);
 *     let acc = Dummy in
 *     count_loop level { next with acc = acc } switch (cpt + cost inst switch None)
 *   | SETFLOATFIELD n           ->
 *     let acc = Dummy in
 *     count_loop level { next with acc = acc } switch (cpt + cost inst switch None)
 *   | VECTLENGTH                ->
 *     let acc = Dummy in
 *     count_loop level { next with acc = acc } switch (cpt + cost inst switch None)
 *   | GETVECTITEM               ->
 *     ignore @@ Mlstack.pop state.stack;
 *     let acc = Dummy in
 *     count_loop level { next with acc = acc } switch (cpt + cost inst switch None)
 *   | SETVECTITEM               ->
 *     ignore @@ Mlstack.pop state.stack;
 *     ignore @@ Mlstack.pop state.stack;
 *     let acc = Dummy in
 *     count_loop level { next with acc = acc } switch (cpt + cost inst switch None)
 *   | GETSTRINGCHAR             ->
 *     ignore @@ Mlstack.pop state.stack;
 *     let acc = Dummy in
 *     count_loop level { next with acc = acc } switch (cpt + cost inst switch None)
 *   | SETSTRINGCHAR             ->
 *     ignore @@ Mlstack.pop state.stack;
 *     ignore @@ Mlstack.pop state.stack;
 *     let acc = Dummy in
 *     count_loop level { next with acc = acc } switch (cpt + cost inst switch None)
 *   | BRANCH b -> count_loop level { next with pc = b }  switch (cpt + cost inst switch None)
 *   | BRANCHIF ptr              ->
 *      begin
 *       match state.acc with
 *         | Dummy ->
 *           max (count_loop (level+1) { next with pc = ptr } switch (cpt + cost inst switch None))
 *             (count_loop level next switch (cpt + cost inst switch None))
 *       | Int 0 -> count_loop level next switch (cpt + cost inst switch None)
 *       | _ -> count_loop (level+1) { next with pc = ptr } switch (cpt + cost inst switch None)
 *     end
 *   | BRANCHIFNOT ptr           ->
 *     begin
 *       match state.acc with
 *       | Dummy ->
 *         max (count_loop (level+1) { next with pc = ptr } switch (cpt + cost inst switch None))
 *               (count_loop level next switch (cpt + cost inst switch None))
 *       | Int 0 -> count_loop (level+1) { next with pc = ptr } switch (cpt + cost inst switch None)
 *       | _ -> count_loop level next switch (cpt + cost inst switch None)
 *     end
 *   | SWITCH(n, ptrs)          ->
 *     (\* Probleme ici  *\)
 *     begin
 *     match state.acc with
 *       | Int v ->
 *         count_loop level { next with pc = ptrs.(v) } switch (cpt + cost inst switch None)
 *       | Block (tag,b) ->  count_loop level { next with pc = ptrs.(tag + (n land 0xFFFF)) } switch (cpt + cost inst switch None)
 *       | _ -> failwith "?"
 *   end
 *   | BOOLNOT                   ->
 *     let acc = match state.acc with
 *       | Int 0 -> Int 1
 *       | Int 1 -> Int 0
 *       | _ -> failwith "not a bool"
 *     in
 *     count_loop level { next with acc } switch (cpt + cost inst switch None)
 *   | PUSHTRAP ptr              ->
 *     Mlstack.push state.stack (Int state.extraArgs);
 *     Mlstack.push state.stack state.env;
 *     Mlstack.push state.stack (Int state.trapSp);
 *     Mlstack.push state.stack (Ptr ptr);
 *     count_loop level { next with trapSp = Mlstack.length state.stack } switch (cpt + cost inst switch None)
 *   | POPTRAP                   ->
 *     ignore @@ Mlstack.pop state.stack;
 *     let trapSp = int_of_value (Mlstack.pop state.stack) in
 *     ignore @@ Mlstack.pop state.stack;
 *     ignore @@ Mlstack.pop state.stack;
 *     count_loop level { next with trapSp = trapSp } switch (cpt + cost inst switch None)
 *   | C_CALL1 idx ->
 *     let acc = Dummy in
 *     if prims.(idx) = "begin_loop" then
 *       count_loop level { next with acc = acc} true (cpt + cost inst switch None)
 *     else if prims.(idx) = "end_loop" then
 *       cpt
 *     else
 *       count_loop level { next with acc = acc} switch (cpt + cost inst switch None)
 *   | C_CALL2 idx ->
 *     let acc = Dummy in
 *     ignore @@ Mlstack.pop state.stack;
 *     count_loop level { next with acc } switch (cpt + cost inst switch None)
 *   | C_CALL3 idx ->
 *     let acc = Dummy in
 *     ignore @@ Mlstack.pop state.stack;
 *     ignore @@ Mlstack.pop state.stack;
 *     count_loop level { next with acc } switch (cpt + cost inst switch None)
 *   | C_CALL4 idx ->
 *     let acc = Dummy in
 *     ignore @@ Mlstack.pop state.stack;
 *     ignore @@ Mlstack.pop state.stack;
 *     ignore @@ Mlstack.pop state.stack;
 *     count_loop level { next with acc } switch (cpt + cost inst switch None)
 *   | C_CALL5 idx ->
 *     let acc = Dummy in
 *     ignore @@ Mlstack.pop state.stack;
 *     ignore @@ Mlstack.pop state.stack;
 *     ignore @@ Mlstack.pop state.stack;
 *     ignore @@ Mlstack.pop state.stack;
 *     count_loop level { next with acc } switch (cpt + cost inst switch None)
 *   | C_CALLN (narg, idx)       ->
 *     let acc = Dummy in
 *     Mlstack.push state.stack state.acc;
 *     Mlstack.popn state.stack narg;
 *     count_loop level { next with acc } switch (cpt + cost inst switch None)
 *   | RAISE | RERAISE | RAISE_NOTRACE -> raise Exit
 *   | CHECK_SIGNALS             ->
 *     count_loop level next switch (cpt + cost inst switch None)
 *   | CONST0                    -> count_loop level { next with acc = Int 0 } switch (cpt + cost inst switch None)
 *   | CONST1                    -> count_loop level { next with acc = Int 1 } switch (cpt + cost inst switch None)
 *   | CONST2                    -> count_loop level { next with acc = Int 2 } switch (cpt + cost inst switch None)
 *   | CONST3                    -> count_loop level { next with acc = Int 3 } switch (cpt + cost inst switch None)
 *   | CONSTINT n                -> count_loop level { next with acc = Int n } switch (cpt + cost inst switch None)
 *   | PUSHCONST0                ->
 *     Mlstack.push state.stack state.acc;
 *     count_loop level { next with acc = Int 0 } switch (cpt + cost inst switch None)
 *   | PUSHCONST1                ->
 *     Mlstack.push state.stack state.acc;
 *     count_loop level { next with acc = Int 1 } switch (cpt + cost inst switch None)
 *   | PUSHCONST2                ->
 *     Mlstack.push state.stack state.acc;
 *     count_loop level { next with acc = Int 2 } switch (cpt + cost inst switch None)
 *   | PUSHCONST3                ->
 *     Mlstack.push state.stack state.acc;
 *     count_loop level { next with acc = Int 3 } switch (cpt + cost inst switch None)
 *   | PUSHCONSTINT n            ->
 *     Mlstack.push state.stack state.acc;
 *     count_loop level { next with acc = Int n } switch (cpt + cost inst switch None)
 *   | NEGINT                    ->
 *     let acc = int_op (fun x y -> -x ) state.acc (Int 0) in
 *     count_loop level { next with acc = acc } switch (cpt + cost inst switch None)
 *   | ADDINT                    ->
 *     let v = (Mlstack.pop state.stack) in
 *     let acc = int_op (fun x y -> x+y ) state.acc v in
 *     count_loop level { next with acc } switch (cpt + cost inst switch None)
 *   | SUBINT                    ->
 *     let v = (Mlstack.pop state.stack) in
 *     let acc = int_op (fun x y -> x-y ) state.acc v in
 *     count_loop level { next with acc } switch (cpt + cost inst switch None)
 *   | MULINT                    ->
 *     let v = (Mlstack.pop state.stack) in
 *     let acc = int_op (fun x y -> x*y ) state.acc v in
 *     count_loop level { next with acc } switch (cpt + cost inst switch None)
 *   | DIVINT                    ->
 *     let v = (Mlstack.pop state.stack) in
 *     let acc = int_op (fun x y -> x/y ) state.acc v in
 *     count_loop level { next with acc } switch (cpt + cost inst switch None)
 *   | MODINT                    ->
 *     let v = (Mlstack.pop state.stack) in
 *     let acc = int_op (fun x y -> x mod y ) state.acc v in
 *     count_loop level { next with acc } switch (cpt + cost inst switch None)
 *   | ANDINT                    ->
 *      let v = (Mlstack.pop state.stack) in
 *     let acc = int_op (fun x y -> x land y ) state.acc v in
 *     count_loop level { next with acc } switch (cpt + cost inst switch None)
 *   | ORINT                     ->
 *     let v = (Mlstack.pop state.stack) in
 *     let acc = int_op (fun x y -> x lor y ) state.acc v in
 *     count_loop level { next with acc } switch (cpt + cost inst switch None)
 *   | XORINT                    ->
 *       let v = (Mlstack.pop state.stack) in
 *       let acc = int_op (fun x y -> x lxor y ) state.acc v in
 *       count_loop level { next with acc } switch (cpt + cost inst switch None)
 *   | LSLINT                    ->
 *     let v = (Mlstack.pop state.stack) in
 *     let acc = int_op (fun x y -> x lsl y ) state.acc v in
 *     count_loop level { next with acc } switch (cpt + cost inst switch None)
 *   | LSRINT                    ->
 *     let v = (Mlstack.pop state.stack) in
 *     let acc = int_op (fun x y -> x lsr y ) state.acc v in
 *     count_loop level { next with acc } switch (cpt + cost inst switch None)
 *   | ASRINT                    ->
 *     let v = (Mlstack.pop state.stack) in
 *     let acc = int_op (fun x y -> x asr y ) state.acc v in
 *     count_loop level { next with acc } switch (cpt + cost inst switch None)
 *   | EQ                        ->
 *     let v = (Mlstack.pop state.stack) in
 *     let acc = int_op (fun x y -> if x = y then 1 else 0 ) state.acc v in
 *     count_loop level { next with acc } switch (cpt + cost inst switch None)
 *   | NEQ ->
 *       let v = (Mlstack.pop state.stack) in
 *       let acc = int_op (fun x y -> if x <> y then 1 else 0 ) state.acc v in
 *       count_loop level { next with acc } switch (cpt + cost inst switch None)
 *   | LTINT                     ->
 *     let v = (Mlstack.pop state.stack) in
 *     let acc = int_op (fun x y -> if x < y then 1 else 0) state.acc v in
 *     count_loop level { next with acc } switch (cpt + cost inst switch None)
 *   | LEINT                     ->
 *     let v = (Mlstack.pop state.stack) in
 *     let acc = int_op (fun x y -> if x <= y then 1 else 0 ) state.acc v in
 *     count_loop level { next with acc } switch (cpt + cost inst switch None)
 *   | GTINT                     ->
 *     let v = (Mlstack.pop state.stack) in
 *     let acc = int_op (fun x y -> if x > y then 1 else 0 ) state.acc v in
 *     count_loop level { next with acc } switch (cpt + cost inst switch None)
 *   | GEINT                     ->
 *     let v = (Mlstack.pop state.stack) in
 *     let acc = int_op (fun x y -> if x >= y then 1 else 0) state.acc v in
 *     count_loop level { next with acc } switch (cpt + cost inst switch None)
 *   | OFFSETINT n               ->
 *     let acc = int_op (fun x y -> x + y ) state.acc (Int n) in
 *     count_loop level { next with acc } switch (cpt + cost inst switch None)
 *   | OFFSETREF n               ->
 *     begin
 *       match state.acc with
 *       | Block (tag,t) -> t.(0) <- int_op (fun x y -> x + y) t.(0) (Int n)
 *       | _ -> failwith "not a block"
 *     end;
 *     count_loop level next switch (cpt + cost inst switch None)
 *   | ISINT                     ->
 *     let acc = match state.acc with
 *       | Int i -> Int 1
 *       | _ -> Int 0
 *     in
 *     count_loop level { next with acc } switch (cpt + cost inst switch None)
 *   | GETMETHOD                 ->
 *     let acc = Dummy in
 *     count_loop level { next with acc } switch (cpt + cost inst switch None)
 *   | BEQ (n, ptr)              ->
 *        begin
 *       match state.acc with
 *         | Dummy ->
 *           max (count_loop (level+1) { next with pc = ptr }  switch (cpt + cost inst switch None))
 *             (count_loop level next  switch (cpt + cost inst switch None))
 *       | Int v -> if n = v then
 *           count_loop (level+1) { next with pc = ptr } switch (cpt + cost inst switch None)
 *         else
 *           count_loop level next switch (cpt + cost inst switch None)
 *       | _ -> failwith "wrong accumulator"
 *     end
 *   | BNEQ (n, ptr)             ->
 *    begin
 *       match state.acc with
 *       | Dummy ->
 *         max (count_loop (level+1) { next with pc = ptr } switch (cpt + cost inst switch None))
 *         (count_loop level next switch (cpt + cost inst switch None))
 *       | Int v -> if n <> v then
 *           count_loop (level+1) { next with pc = ptr } switch (cpt + cost inst switch None)
 *         else
 *           count_loop level next switch (cpt + cost inst switch None)
 *       | _ -> failwith "wrong accumulator"
 *     end
 *   | BLTINT (n, ptr)           ->
 *        begin
 *       match state.acc with
 *       | Dummy ->
 *         max (count_loop (level+1) { next with pc = ptr } switch (cpt + cost inst switch None))
 *         (count_loop level next switch (cpt + cost inst switch None))
 *       | Int v -> if n < v then
 *           count_loop (level+1) { next with pc = ptr } switch (cpt + cost inst switch None)
 *         else
 *           count_loop level next switch (cpt + cost inst switch None)
 *       | _ -> failwith "wrong accumulator"
 *     end
 *   | BLEINT (n, ptr)           ->
 *      begin
 *       match state.acc with
 *         | Dummy ->
 *           max (count_loop (level+1) { next with pc = ptr } switch (cpt + cost inst switch None))
 *             (count_loop level next switch (cpt + cost inst switch None))
 *       | Int v -> if n <= v then
 *           count_loop (level+1) { next with pc = ptr } switch (cpt + cost inst switch None)
 *         else
 *           count_loop level next switch (cpt + cost inst switch None)
 *       | _ -> failwith "wrong accumulator"
 *     end
 *   | BGTINT (n, ptr)           ->
 *     begin
 *       match state.acc with
 *       | Dummy ->
 *         max (count_loop (level+1) { next with pc = ptr } switch (cpt + cost inst switch None))
 *         (count_loop level next switch (cpt + cost inst switch None))
 *       | Int v -> if n > v then
 *           count_loop (level+1) { next with pc = ptr } switch (cpt + cost inst switch None)
 *         else
 *           count_loop level next switch (cpt + cost inst switch None)
 *       | _ -> failwith "wrong accumulator"
 *     end
 *   | BGEINT (n, ptr) ->
 *         begin
 *       match state.acc with
 *         | Dummy ->
 *           max (count_loop (level+1) { next with pc = ptr } switch (cpt + cost inst switch None))
 *             (count_loop level next switch (cpt + cost inst switch None))
 *       | Int v -> if n >= v then
 *           count_loop (level+1) { next with pc = ptr } switch (cpt + cost inst switch None)
 *         else
 *           count_loop level next switch (cpt + cost inst switch None)
 *       | _ -> failwith "wrong accumulator"
 *     end
 *   | ULTINT ->
 *     let n = int_op (fun x y -> x + min_int) state.acc (Int 0) in
 *     let p = int_op (fun x y -> x + min_int) (Mlstack.pop state.stack) (Int 0) in
 *     let acc = int_op (fun x y -> if x < y then 1 else 0) n p in
 *     count_loop level { next with acc } switch (cpt + cost inst switch None)
 *   | UGEINT ->
 *     let n = int_op (fun x y -> x + min_int) state.acc (Int 0) in
 *     let p = int_op (fun x y -> x + min_int) (Mlstack.pop state.stack) (Int 0) in
 *     let acc = int_op (fun x y -> if x >= y then 1 else 0) n p in
 *     count_loop level { next with acc } switch (cpt + cost inst switch None)
 *   (\* | BULTINT (n,ptr) -> *\)
 *   (\*   count_loop level { next with pc = ptr }; *\)
 *   (\*   count_loop level next *\)
 *   (\* | BUGEINT (n, ptr) -> *\)
 *   (\*   count_loop level { next with pc = ptr }; *\)
 *   (\*   count_loop level next *\)
 *   | GETPUBMET (tag, cache)    -> failwith "todo getpubmet"
 *   | GETDYNMET                 -> failwith "todo getdynmet"
 *   | STOP                      -> 0
 *   | EVENT                     -> failwith "todo event"
 *   | BREAK                     -> failwith "todo break"
 *   | _ -> failwith "unknown instr"
 *   in count_loop level state false 0 *)

let cpt = ref 0



let count_cost f =
  let bytefile = Bytefile.read f in
  let data = bytefile.Bytefile.data in
  let primitives = bytefile.Bytefile.prim in
  let bytecode = bytefile.Bytefile.code in
  let global = Array.map value_of_obytelib data in
  init_prims ();
  init_cyc ();
  for i = 0 to Array.length primitives -1 do
    Printf.printf "PRIM %d : %s\n" i primitives.(i);
  done;
  let n = eval bytecode primitives (new_state global) false in
  Printf.printf "Cost : %d \n"  n;
  if !freq <> 0. then
    Printf.printf "= %f seconds \n" (float_of_int n /. (!freq *. 1_000_000.) )
