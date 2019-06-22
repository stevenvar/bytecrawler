type ('a, 'b) edge_state =
  {
  mutable edge_pre__e_aux1: 'a ;
  mutable edge_out_e: 'b }
let edge_init x =
  let _e_aux1 = true in
  let e = x && (not _e_aux1) in
  let pre__e_aux1 = x in { edge_pre__e_aux1 = pre__e_aux1; edge_out_e = e }
let edge_step state x =
  let _e_aux1 = state.edge_pre__e_aux1 in
  let e = x && (not _e_aux1) in
  let pre__e_aux1 = x in
  state.edge_out_e <- e; state.edge_pre__e_aux1 <- pre__e_aux1; ()
type ('a, 'b, 'c) read_bit_state =
  {
  mutable read_bit_edge1_state: 'a ;
  mutable read_bit_out_clk: 'b ;
  mutable read_bit_out_data: 'c }
let read_bit_init top bot =
  let edge1_state = edge_init top in
  let clk = edge1_state.edge_out_e in
  let data = if clk then bot else Obj.magic () in
  {
    read_bit_edge1_state = edge1_state;
    read_bit_out_clk = clk;
    read_bit_out_data = data
  }
let read_bit_step state top bot =
  let _edge1_state = edge_step state.read_bit_edge1_state top in
  let clk = (state.read_bit_edge1_state).edge_out_e in
  let data = if clk then bot else Obj.magic () in
  state.read_bit_out_data <- data; state.read_bit_out_clk <- clk; ()
type ('a, 'b) count_state =
  {
  mutable count_pre_cpt: 'a ;
  mutable count_out_cpt: 'b }
let count_init reset =
  let cpt = 0 in
  let pre_cpt = if cpt = reset then 0 else cpt + 1 in
  { count_pre_cpt = pre_cpt; count_out_cpt = cpt }
let count_step state reset =
  let cpt = state.count_pre_cpt in
  let pre_cpt = if cpt = reset then 0 else cpt + 1 in
  state.count_out_cpt <- cpt; state.count_pre_cpt <- pre_cpt; ()
type ('a, 'b, 'c, 'd, 'e, 'f) read_card_state =
  {
  mutable read_card_count2_state: 'a ;
  mutable read_card_read_bit1_state: 'b ;
  mutable read_card_out_cpt: 'c ;
  mutable read_card_out_data: 'd ;
  mutable read_card_out_clk: 'e ;
  mutable read_card_out_send: 'f }
let read_card_init top bot =
  let read_bit1_state = read_bit_init top bot in
  let clk = read_bit1_state.read_bit_out_clk in
  let data = read_bit1_state.read_bit_out_data in
  let count2_state = count_init (if clk then 7 else Obj.magic ()) in
  let cpt = count2_state.count_out_cpt in
  let send = if clk then cpt = 7 else false in
  {
    read_card_count2_state = count2_state;
    read_card_read_bit1_state = read_bit1_state;
    read_card_out_cpt = cpt;
    read_card_out_data = data;
    read_card_out_clk = clk;
    read_card_out_send = send
  }
let read_card_step state top bot =
  let _read_bit1_state =
    read_bit_step state.read_card_read_bit1_state top bot in
  let clk = (state.read_card_read_bit1_state).read_bit_out_clk in
  let data = (state.read_card_read_bit1_state).read_bit_out_data in
  let _count2_state =
    if clk then count_step state.read_card_count2_state (7) in
  let cpt = (state.read_card_count2_state).count_out_cpt in
  let send = if clk then cpt = 7 else false in
  state.read_card_out_send <- send;
  state.read_card_out_clk <- clk;
  if clk then state.read_card_out_data <- data;
  if clk then state.read_card_out_cpt <- cpt;
  ()

open Wcet


let () =

  let st = read_card_init true false in
  (* let n = Avr.millis () in *)
      let b1 = rand_bool () in
    let b2 = rand_bool () in
  let n = Avr.millis () in
  for i = 0 to 10000 do
     (* begin_loop (); *)
     read_card_step st b1 b2;
     (* end_loop(); *)
   done;
   let n' = Avr.millis () in
   Avr.Serial.write_int (n'-n)
