open Wcet

type ('a, 'b) thermo_on_state =
  {
  mutable thermo_on_pre_b: 'a ;
  mutable thermo_on_out_b: 'b }
let thermo_on_init p m =
  let b = true in
  let pre_b = if p && m then not b else b in
  { thermo_on_pre_b = pre_b; thermo_on_out_b = b }
let thermo_on_step state p m =
  let b = state.thermo_on_pre_b in
  let pre_b = if p && m then not b else b in
  state.thermo_on_out_b <- b; state.thermo_on_pre_b <- pre_b; ()
type ('a, 'b) set_wanted_temp_state =
  {
  mutable set_wanted_temp_pre_w: 'a ;
  mutable set_wanted_temp_out_w: 'b }
let set_wanted_temp_init p m =
  let w = 325 in
  let pre_w = if p then w + 5 else if m then w - 5 else w in
  { set_wanted_temp_pre_w = pre_w; set_wanted_temp_out_w = w }
let set_wanted_temp_step state p m =
  let w = state.set_wanted_temp_pre_w in
  let pre_w = if p then w + 5 else if m then w - 5 else w in
  state.set_wanted_temp_out_w <- w; state.set_wanted_temp_pre_w <- pre_w; ()
type ('a, 'b, 'c, 'd, 'e, 'f) thermo_state =
  {
  mutable thermo_set_wanted_temp2_state: 'a ;
  mutable thermo_thermo_on1_state: 'b ;
  mutable thermo_out_on: 'c ;
  mutable thermo_out_wanted: 'd ;
  mutable thermo_out_real: 'e ;
  mutable thermo_out_resistor: 'f }
let thermo_init plus minus real_temp =
  let thermo_on1_state = thermo_on_init plus minus in
  let on = thermo_on1_state.thermo_on_out_b in
  let set_wanted_temp2_state = set_wanted_temp_init plus minus in
  let wanted = set_wanted_temp2_state.set_wanted_temp_out_w in
  let real = real_temp in
  let heat = real < wanted in
  let resistor = if on then heat else false in
  {
    thermo_set_wanted_temp2_state = set_wanted_temp2_state;
    thermo_thermo_on1_state = thermo_on1_state;
    thermo_out_on = on;
    thermo_out_wanted = wanted;
    thermo_out_real = real;
    thermo_out_resistor = resistor
  }
let thermo_step state plus minus real_temp =
  let () = thermo_on_step state.thermo_thermo_on1_state plus minus in
  let on = (state.thermo_thermo_on1_state).thermo_on_out_b in
  let () =
    if (on = true) && true
    then set_wanted_temp_step state.thermo_set_wanted_temp2_state plus minus
    else () in
  let wanted = (state.thermo_set_wanted_temp2_state).set_wanted_temp_out_w in
  let real = if on then real_temp else Obj.magic () in
  let heat = real < wanted in
  let resistor = if on then heat else false in
  state.thermo_out_resistor <- resistor;
  state.thermo_out_real <- real;
  state.thermo_out_wanted <- wanted;
  state.thermo_out_on <- on;
  ()
let () =
     let _st = thermo_init true true 1 in
     while true do
       let plus = rand_bool () in
       let minus = rand_bool () in
       let real_temp = rand_int () in
       begin_loop ();
       thermo_step _st plus minus real_temp;
       end_loop ()
     done
