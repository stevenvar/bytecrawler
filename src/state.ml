open OByteLib
open OByteLib.Instr

open Mlstack

let string_of_instr = function
  | ACC0                      ->  "ACC0"
  | ACC1                      ->  "ACC1"
  | ACC2                      ->  "ACC2"
  | ACC3                      ->  "ACC3"
  | ACC4                      ->  "ACC4"
  | ACC5                      ->  "ACC5"
  | ACC6                      ->  "ACC6"
  | ACC7                      ->  "ACC7"
  | ACC n                     ->  "ACC"
  | PUSH                      ->  "PUSH"
  | PUSHACC0                  ->  "PUSHACC0"
  | PUSHACC1                  ->  "PUSHACC1"
  | PUSHACC2                  ->  "PUSHACC2"
  | PUSHACC3                  ->  "PUSHACC3"
  | PUSHACC4                  ->  "PUSHACC4"
  | PUSHACC5                  ->  "PUSHACC5"
  | PUSHACC6                  ->  "PUSHACC6"
  | PUSHACC7                  ->  "PUSHACC7"
  | PUSHACC n                 ->  "PUSHACC"
  | POP n                     ->  "POP"
  | ASSIGN n                  ->  "ASSIGN"
  | ENVACC1                   ->  "ENVACC1"
  | ENVACC2                   ->  "ENVACC2"
  | ENVACC3                   ->  "ENVACC3"
  | ENVACC4                   ->  "ENVACC4"
  | ENVACC n                  ->  "ENVACC"
  | PUSHENVACC1               ->  "PUSHENVACC1"
  | PUSHENVACC2               ->  "PUSHENVACC2"
  | PUSHENVACC3               ->  "PUSHENVACC3"
  | PUSHENVACC4               ->  "PUSHENVACC4"
  | PUSHENVACC n              ->  "PUSHENVACC"
  | PUSH_RETADDR ptr          ->  "PUSH_RETADDR"
  | APPLY n                   ->  "APPLY"
  | APPLY1                    ->  "APPLY1"
  | APPLY2                    ->  "APPLY2"
  | APPLY3                    ->  "APPLY3"
  | APPTERM (n, s)            ->  "APPTERM"
  | APPTERM1 s                ->  "APPTERM1"
  | APPTERM2 s                ->  "APPTERM2"
  | APPTERM3 s                ->  "APPTERM3"
  | RETURN n                  ->  "RETURN"
  | RESTART                   ->  "RESTART"
  | GRAB n                    ->  "GRAB"
  | CLOSURE (n, ptr)          ->  "CLOSURE"
  | CLOSUREREC (f, v, o, t)   ->  "CLOSUREREC"
  | OFFSETCLOSUREM2           ->  "OFFSETCLOSUREM2"
  | OFFSETCLOSURE0            ->  "OFFSETCLOSURE0"
  | OFFSETCLOSURE2            ->  "OFFSETCLOSURE2"
  | OFFSETCLOSURE n           ->  "OFFSETCLOSURE"
  | PUSHOFFSETCLOSUREM2       ->  "PUSHOFFSETCLOSUREM2"
  | PUSHOFFSETCLOSURE0        ->  "PUSHOFFSETCLOSURE0"
  | PUSHOFFSETCLOSURE2        ->  "PUSHOFFSETCLOSURE2"
  | PUSHOFFSETCLOSURE n       ->  "PUSHOFFSETCLOSURE"
  | GETGLOBAL n               ->  "GETGLOBAL"
  | PUSHGETGLOBAL n           ->  "PUSHGETGLOBAL"
  | GETGLOBALFIELD (n, p)     ->  "GETGLOBALFIELD"
  | PUSHGETGLOBALFIELD (n, p) ->  "PUSHGETGLOBALFIELD"
  | SETGLOBAL n               ->  "SETGLOBAL"
  | ATOM0                     ->  "ATOM0"
  | ATOM tag                  ->  "ATOM"
  | PUSHATOM0                 ->  "PUSHATOM0"
  | PUSHATOM tag              ->  "PUSHATOM"
  | MAKEBLOCK (tag, sz)       ->  "MAKEBLOCK"
  | MAKEBLOCK1 tag            ->  "MAKEBLOCK1"
  | MAKEBLOCK2 tag            ->  "MAKEBLOCK2"
  | MAKEBLOCK3 tag            ->  "MAKEBLOCK3"
  | MAKEFLOATBLOCK sz         ->  "MAKEFLOATBLOCK"
  | GETFIELD0                 ->  "GETFIELD0"
  | GETFIELD1                 ->  "GETFIELD1"
  | GETFIELD2                 ->  "GETFIELD2"
  | GETFIELD3                 ->  "GETFIELD3"
  | GETFIELD n                ->  "GETFIELD"
  | GETFLOATFIELD n           ->  "GETFLOATFIELD"
  | SETFIELD0                 ->  "SETFIELD0"
  | SETFIELD1                 ->  "SETFIELD1"
  | SETFIELD2                 ->  "SETFIELD2"
  | SETFIELD3                 ->  "SETFIELD3"
  | SETFIELD n                ->  "SETFIELD"
  | SETFLOATFIELD n           ->  "SETFLOATFIELD"
  | VECTLENGTH                ->  "VECTLENGTH"
  | GETVECTITEM               ->  "GETVECTITEM"
  | SETVECTITEM               ->  "SETVECTITEM"
  | GETSTRINGCHAR             ->  "GETSTRINGCHAR"
  | SETSTRINGCHAR             ->  "SETSTRINGCHAR"
  | BRANCH ptr                ->  "BRANCH"
  | BRANCHIF ptr              ->  "BRANCHIF"
  | BRANCHIFNOT ptr           ->  "BRANCHIFNOT"
  | SWITCH (n, ptrs)          ->  "SWITCH"
  | BOOLNOT                   ->  "BOOLNOT"
  | PUSHTRAP ptr              ->  "PUSHTRAP"
  | POPTRAP                   ->  "POPTRAP"
  | RAISE                     ->  "RAISE"
  | CHECK_SIGNALS             ->  "CHECK_SIGNALS"
  | C_CALL1 idx               ->  "C_CALL1"
  | C_CALL2 idx               ->  "C_CALL2"
  | C_CALL3 idx               ->  "C_CALL3"
  | C_CALL4 idx               ->  "C_CALL4"
  | C_CALL5 idx               ->  "C_CALL5"
  | C_CALLN (narg, idx)       ->  "C_CALLN"
  | CONST0                    ->  "CONST0"
  | CONST1                    ->  "CONST1"
  | CONST2                    ->  "CONST2"
  | CONST3                    ->  "CONST3"
  | CONSTINT n                ->  "CONSTINT"
  | PUSHCONST0                ->  "PUSHCONST0"
  | PUSHCONST1                ->  "PUSHCONST1"
  | PUSHCONST2                ->  "PUSHCONST2"
  | PUSHCONST3                ->  "PUSHCONST3"
  | PUSHCONSTINT n            ->  "PUSHCONSTINT"
  | NEGINT                    ->  "NEGINT"
  | ADDINT                    ->  "ADDINT"
  | SUBINT                    ->  "SUBINT"
  | MULINT                    ->  "MULINT"
  | DIVINT                    ->  "DIVINT"
  | MODINT                    ->  "MODINT"
  | ANDINT                    ->  "ANDINT"
  | ORINT                     ->  "ORINT"
  | XORINT                    ->  "XORINT"
  | LSLINT                    ->  "LSLINT"
  | LSRINT                    ->  "LSRINT"
  | ASRINT                    ->  "ASRINT"
  | EQ                        ->  "EQ"
  | NEQ                       ->  "NEQ"
  | LTINT                     ->  "LTINT"
  | LEINT                     ->  "LEINT"
  | GTINT                     ->  "GTINT"
  | GEINT                     ->  "GEINT"
  | OFFSETINT n               ->  "OFFSETINT"
  | OFFSETREF n               ->  "OFFSETREF"
  | ISINT                     ->  "ISINT"
  | GETMETHOD                 ->  "GETMETHOD"
  | BEQ (n, ptr)              ->  "BEQ"
  | BNEQ (n, ptr)             ->  "BNEQ"
  | BLTINT (n, ptr)           ->  "BLTINT"
  | BLEINT (n, ptr)           ->  "BLEINT"
  | BGTINT (n, ptr)           ->  "BGTINT"
  | BGEINT (n, ptr)           ->  "BGEINT"
  | ULTINT                    ->  "ULTINT"
  | UGEINT                    ->  "UGEINT"
  | BULTINT (n, ptr)          ->  "BULTINT"
  | BUGEINT (n, ptr)          ->  "BUGEINT"
  | GETPUBMET (tag, cache)    ->  "GETPUBMET"
  | GETDYNMET                 ->  "GETDYNMET"
  | STOP                      ->  "STOP"
  | EVENT                     ->  "EVENT"
  | BREAK                     ->  "BREAK"
  | RERAISE                   ->  "RERAISE"
  | RAISE_NOTRACE             ->  "RAISE_NOTRACE"

type value = Ptr of int
           | Dummy
           | Int of int
           | Unit
           | Float of float
           | String of string
           | Object of value array
           | Block of int * value array
           | Closure of value * value array
           | Closure_rec of value * int array * value array * int

type state = { mutable pc : int;
               mutable acc : value;
               stack : value Mlstack.t;
               mutable extraArgs : int ;
               mutable env : value;
               global : value array;
               mutable trapSp : int; }

let rec string_of_value v =
  let rec string_of_list l =
    match l with
    | [] -> ""
    | [x] -> string_of_value x
    | x::xs -> Format.asprintf "%s, %s" (string_of_value x) (string_of_list xs)
  in
  let string_of_array a = string_of_list (Array.to_list a) in
  match v with
  | Unit -> "()"
  | Dummy -> "?"
  | String s -> "\""^s^"\""
  | Int v -> string_of_int v
  | Float f -> string_of_float f
  | Ptr x -> "@"^string_of_int x
  | Object e ->
    let s = string_of_array e in
    Format.asprintf "(%s)" s
  | Block (tag,b) ->
    begin
      match Array.length b with
      | 0 -> Format.asprintf "[%d]" tag
      | _ ->
        let s = string_of_array b in
        Format.asprintf "[%d : %s]" tag s
    end
  | Closure (ptr,env) ->
    begin
    match Array.length env with
      0 -> Format.asprintf "{%s}" (string_of_value ptr)
      | _ -> let s = string_of_array env in
        Format.asprintf "{%s: %s}" (string_of_value ptr) s
  end
  | Closure_rec (ptr,t,env,i) ->
    let s = string_of_array env in
    Format.asprintf "<%s (%d) %s>" (string_of_value ptr) i s


let new_state data = { pc = 0;
                       acc = Unit;
                       stack = Mlstack.create ();
                       extraArgs = 0;
                       env = Unit;
                       global = data;
                       trapSp = 0 }

let print_state state level t =
  Format.fprintf Format.std_formatter
    "stack = %s
env = %s
acc= %s
extra_args = %d
_________________
%d:%s\n\n"
    (Mlstack.to_string state.stack string_of_value)
    (string_of_value state.env)
    (string_of_value state.acc)
    state.extraArgs
    state.pc
    (Instr.to_string t.(state.pc))

let size_of_value v =
  match v with
  | Closure (_,b) |
    Closure_rec (_,_,b,_) |
    Block (_,b) -> Array.length b
  | _ -> 0

let heap_size state =
  let rec block_allocation =
    function
    | String s -> failwith "cannot guess the memory allocation of a string"
    | Block (_,a) |
     Closure (_,a) |
     Closure_rec(_,_,a,_) -> 1 + Array.length a
    | Object a -> 0
    | _ -> 0
  in
  Mlstack.fold_left (fun acc x -> acc + block_allocation x) 0 state.stack

let rec ptr_of_value = function
  | Ptr x -> x
  | Closure (ptr,env) -> ptr_of_value ptr
  | Closure_rec (ptr,t,_,i) -> if i = 0 then ptr_of_value ptr else
      t.(i-1)
  | _ -> failwith "not a ptr"

let int_of_value = function
  | Int v -> v
  | x -> failwith @@ (string_of_value x)^" : not an int"

let env_of_closure env =
  match env with
  | Closure (_, blk) -> blk
  | _ -> failwith "not a closure"
