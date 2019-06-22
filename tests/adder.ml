open Wcet

let xor a b = if a then not b else b
type ('a, 'b) half_adder_state =
  {
  mutable half_adder_out_s: 'a ;
  mutable half_adder_out_c: 'b }
let half_adder_init a b =
  let s = xor a b in
  let c = a && b in { half_adder_out_s = s; half_adder_out_c = c }
let half_adder_step state a b =
  let s = xor a b in
  let c = a && b in
  state.half_adder_out_c <- c; state.half_adder_out_s <- s; ()
type ('a, 'b) fulladder_state =
  {
  mutable fulladder_out_s: 'a ;
  mutable fulladder_out_cout: 'b }
let fulladder_init a b cin =
  let x = xor a b in
  let s = xor x cin in
  let and1 = x && cin in
  let and2 = a && b in
  let cout = and1 || and2 in
  { fulladder_out_s = s; fulladder_out_cout = cout }
let fulladder_step state a b cin =
  let x = xor a b in
  let s = xor x cin in
  let and1 = x && cin in
  let and2 = a && b in
  let cout = and1 || and2 in
  state.fulladder_out_cout <- cout; state.fulladder_out_s <- s; ()
type ('a, 'b, 'c, 'd, 'e) twobits_adder_state =
  {
  mutable twobits_adder_fulladder2_state: 'a ;
  mutable twobits_adder_fulladder1_state: 'b ;
  mutable twobits_adder_out_s0: 'c ;
  mutable twobits_adder_out_s1: 'd ;
  mutable twobits_adder_out_c2: 'e }
let twobits_adder_init c0 a0 a1 b0 b1 =
  let fulladder1_state = fulladder_init a0 b0 c0 in
  let (s0, c1) =
    ((fulladder1_state.fulladder_out_s),
      (fulladder1_state.fulladder_out_cout)) in
  let fulladder2_state = fulladder_init a1 b1 c1 in
  let (s1, c2) =
    ((fulladder2_state.fulladder_out_s),
      (fulladder2_state.fulladder_out_cout)) in
  {
    twobits_adder_fulladder2_state = fulladder2_state;
    twobits_adder_fulladder1_state = fulladder1_state;
    twobits_adder_out_s0 = s0;
    twobits_adder_out_s1 = s1;
    twobits_adder_out_c2 = c2
  }
let twobits_adder_step state c0 a0 a1 b0 b1 =
  let () = fulladder_step state.twobits_adder_fulladder1_state a0 b0 c0 in
  let (s0, c1) =
    (((state.twobits_adder_fulladder1_state).fulladder_out_s),
      ((state.twobits_adder_fulladder1_state).fulladder_out_cout)) in
  let () = fulladder_step state.twobits_adder_fulladder2_state a1 b1 c1 in
  let (s1, c2) =
    (((state.twobits_adder_fulladder2_state).fulladder_out_s),
      ((state.twobits_adder_fulladder2_state).fulladder_out_cout)) in
  state.twobits_adder_out_c2 <- c2;
  state.twobits_adder_out_s1 <- s1;
  state.twobits_adder_out_s0 <- s0;
  ()
type ('a, 'b, 'c, 'd, 'e, 'f, 'g) fourbits_adder_state =
  {
  mutable fourbits_adder_twobits_adder2_state: 'a ;
  mutable fourbits_adder_twobits_adder1_state: 'b ;
  mutable fourbits_adder_out_s0: 'c ;
  mutable fourbits_adder_out_s1: 'd ;
  mutable fourbits_adder_out_s2: 'e ;
  mutable fourbits_adder_out_s3: 'f ;
  mutable fourbits_adder_out_c4: 'g }
let fourbits_adder_init c0 a0 a1 a2 a3 b0 b1 b2 b3 =
  let twobits_adder1_state = twobits_adder_init c0 a0 a1 b0 b1 in
  let (s0, s1, c2) =
    ((twobits_adder1_state.twobits_adder_out_s0),
      (twobits_adder1_state.twobits_adder_out_s1),
      (twobits_adder1_state.twobits_adder_out_c2)) in
  let twobits_adder2_state = twobits_adder_init c2 a2 a3 b2 b3 in
  let (s2, s3, c4) =
    ((twobits_adder2_state.twobits_adder_out_s0),
      (twobits_adder2_state.twobits_adder_out_s1),
      (twobits_adder2_state.twobits_adder_out_c2)) in
  {
    fourbits_adder_twobits_adder2_state = twobits_adder2_state;
    fourbits_adder_twobits_adder1_state = twobits_adder1_state;
    fourbits_adder_out_s0 = s0;
    fourbits_adder_out_s1 = s1;
    fourbits_adder_out_s2 = s2;
    fourbits_adder_out_s3 = s3;
    fourbits_adder_out_c4 = c4
  }
let fourbits_adder_step state c0 a0 a1 a2 a3 b0 b1 b2 b3 =
  let () =
    twobits_adder_step state.fourbits_adder_twobits_adder1_state c0 a0 a1 b0
      b1 in
  let (s0, s1, c2) =
    (((state.fourbits_adder_twobits_adder1_state).twobits_adder_out_s0),
      ((state.fourbits_adder_twobits_adder1_state).twobits_adder_out_s1),
      ((state.fourbits_adder_twobits_adder1_state).twobits_adder_out_c2)) in
  let () =
    twobits_adder_step state.fourbits_adder_twobits_adder2_state c2 a2 a3 b2
      b3 in
  let (s2, s3, c4) =
    (((state.fourbits_adder_twobits_adder2_state).twobits_adder_out_s0),
      ((state.fourbits_adder_twobits_adder2_state).twobits_adder_out_s1),
      ((state.fourbits_adder_twobits_adder2_state).twobits_adder_out_c2)) in
  state.fourbits_adder_out_c4 <- c4;
  state.fourbits_adder_out_s3 <- s3;
  state.fourbits_adder_out_s2 <- s2;
  state.fourbits_adder_out_s1 <- s1;
  state.fourbits_adder_out_s0 <- s0;
  ()
type ('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k) eightbits_adder_state =
  {
  mutable eightbits_adder_fourbits_adder2_state: 'a ;
  mutable eightbits_adder_fourbits_adder1_state: 'b ;
  mutable eightbits_adder_out_s0: 'c ;
  mutable eightbits_adder_out_s1: 'd ;
  mutable eightbits_adder_out_s2: 'e ;
  mutable eightbits_adder_out_s3: 'f ;
  mutable eightbits_adder_out_s4: 'g ;
  mutable eightbits_adder_out_s5: 'h ;
  mutable eightbits_adder_out_s6: 'i ;
  mutable eightbits_adder_out_s7: 'j ;
  mutable eightbits_adder_out_c8: 'k }
let eightbits_adder_init c0 a0 a1 a2 a3 a4 a5 a6 a7 b0 b1 b2 b3 b4 b5 b6 b7 =
  let fourbits_adder1_state = fourbits_adder_init c0 a0 a1 a2 a3 b0 b1 b2 b3 in
  let (s0, s1, s2, s3, c4) =
    ((fourbits_adder1_state.fourbits_adder_out_s0),
      (fourbits_adder1_state.fourbits_adder_out_s1),
      (fourbits_adder1_state.fourbits_adder_out_s2),
      (fourbits_adder1_state.fourbits_adder_out_s3),
      (fourbits_adder1_state.fourbits_adder_out_c4)) in
  let fourbits_adder2_state = fourbits_adder_init c4 a4 a5 a6 a7 b4 b5 b6 b7 in
  let (s4, s5, s6, s7, c8) =
    ((fourbits_adder2_state.fourbits_adder_out_s0),
      (fourbits_adder2_state.fourbits_adder_out_s1),
      (fourbits_adder2_state.fourbits_adder_out_s2),
      (fourbits_adder2_state.fourbits_adder_out_s3),
      (fourbits_adder2_state.fourbits_adder_out_c4)) in
  {
    eightbits_adder_fourbits_adder2_state = fourbits_adder2_state;
    eightbits_adder_fourbits_adder1_state = fourbits_adder1_state;
    eightbits_adder_out_s0 = s0;
    eightbits_adder_out_s1 = s1;
    eightbits_adder_out_s2 = s2;
    eightbits_adder_out_s3 = s3;
    eightbits_adder_out_s4 = s4;
    eightbits_adder_out_s5 = s5;
    eightbits_adder_out_s6 = s6;
    eightbits_adder_out_s7 = s7;
    eightbits_adder_out_c8 = c8
  }
let eightbits_adder_step state c0 a0 a1 a2 a3 a4 a5 a6 a7 b0 b1 b2 b3 b4 b5
  b6 b7 =
  let () =
    fourbits_adder_step state.eightbits_adder_fourbits_adder1_state c0 a0 a1
      a2 a3 b0 b1 b2 b3 in
  let (s0, s1, s2, s3, c4) =
    (((state.eightbits_adder_fourbits_adder1_state).fourbits_adder_out_s0),
      ((state.eightbits_adder_fourbits_adder1_state).fourbits_adder_out_s1),
      ((state.eightbits_adder_fourbits_adder1_state).fourbits_adder_out_s2),
      ((state.eightbits_adder_fourbits_adder1_state).fourbits_adder_out_s3),
      ((state.eightbits_adder_fourbits_adder1_state).fourbits_adder_out_c4)) in
  let () =
    fourbits_adder_step state.eightbits_adder_fourbits_adder2_state c4 a4 a5
      a6 a7 b4 b5 b6 b7 in
  let (s4, s5, s6, s7, c8) =
    (((state.eightbits_adder_fourbits_adder2_state).fourbits_adder_out_s0),
      ((state.eightbits_adder_fourbits_adder2_state).fourbits_adder_out_s1),
      ((state.eightbits_adder_fourbits_adder2_state).fourbits_adder_out_s2),
      ((state.eightbits_adder_fourbits_adder2_state).fourbits_adder_out_s3),
      ((state.eightbits_adder_fourbits_adder2_state).fourbits_adder_out_c4)) in
  state.eightbits_adder_out_c8 <- c8;
  state.eightbits_adder_out_s7 <- s7;
  state.eightbits_adder_out_s6 <- s6;
  state.eightbits_adder_out_s5 <- s5;
  state.eightbits_adder_out_s4 <- s4;
  state.eightbits_adder_out_s3 <- s3;
  state.eightbits_adder_out_s2 <- s2;
  state.eightbits_adder_out_s1 <- s1;
  state.eightbits_adder_out_s0 <- s0;
  ()
let _ =
  let b1 = rand_bool () in
  let st =
    fourbits_adder_init false true true true true true true true true in
  for i = 1 to 1 do
    (* for j = 0 to 10 do *)
      begin_loop ();
      fourbits_adder_step st b1 b1 b1 b1 b1 b1 b1 b1 b1;
      end_loop ()
    (* done *)
  done
