#include <stdint.h>
#include "values.h"
#include "gc.h"
val_t heap_ptr;
val_t heap_end;
val_t acc;
val_t *ocaml_stack;
static code_t pc;
static val_t env;
static val_t *sp;
static val_t trapSp;
static uint8_t extra_args;
val_t atom0_header;
val_t *ocaml_heap;
opcode_t* const ocaml_bytecode;
val_t *ocaml_global_data;
void * const ocaml_primitives[];

void *get_primitive(uint8_t prim_ind) {
  return (void *) pgm_read_word_near(ocaml_primitives + prim_ind);
}

void caml_raise_division_by_zero(void) {

  /* TODO */
}

char read_byte(void) {
  char c;
  c = pgm_read_byte_near(ocaml_bytecode + pc);
  pc ++;
  return c;
}

opcode_t read_opcode(void) {
  return (opcode_t) read_byte();
}


uint8_t read_uint8(void) {
  return (uint8_t) read_byte();
}

int8_t read_int8(void) {
  return (int8_t) read_byte();
}

uint16_t read_uint16(void) {
  uint8_t n1 = read_uint8();
  uint8_t n0 = read_uint8();
  return ((uint16_t) n1 << 8) | n0;
}

int16_t read_int16(void) {
  return (int16_t) read_uint16();
}

uint32_t read_uint32(void) {
  uint8_t n3 = read_uint8();
  uint8_t n2 = read_uint8();
  uint8_t n1 = read_uint8();
  uint8_t n0 = read_uint8();
  return ((uint32_t) n3 << 24) | ((uint32_t) n2 << 16) | ((uint32_t) n1 << 8) | n0;
}

int32_t read_int32(void) {
  return (int32_t) read_uint32();
}

code_t read_ptr_1B(void) {
  int8_t ofs = read_int8();
  return pc - 2 + ofs;
}

code_t read_ptr_2B(void) {
  int16_t ofs = read_int16();
  return pc - 3 + ofs;
}

code_t read_ptr_4B(void) {
  int32_t ofs = read_int32();
  return pc - 5 + ofs;
}

/******************************************************************************/

val_t peek(int n) {
  return sp[(val_t) n + 1];
}

void push(val_t x) {
  *sp = x;
  sp --;
}

val_t pop(void) {
  return *(++sp);
}

void pop_n(int n) {
  sp += n;
}

void acc0(){
  acc = peek(0);
}

void acc1(){
  acc = peek(1);
}

void acc2(){
  acc = peek(2);
}

void acc3(){
  acc = peek(3);
}

void acc4(){
  acc = peek(4);
}

void acc_(){
  acc = peek(read_uint8());
}

void push_(){
  push(acc);
}

void pushacc2(){
  push(acc);
  acc = peek(3);
}

void pushacc(){
  push(acc);
  acc = peek (read_uint8());
}

void pop_(){
  pop_n(read_uint8());
}

void assign(){
  sp[read_uint8() + 1] = acc;
  acc = Val_unit;
}

void envacc1(){
  acc = Field(env,1);
}

void envacc4(){
  acc = Field(env,4);
}

void envacc(){
  acc = Field(env,read_uint8());
}

void pushenvacc1(){
  push(acc);
  acc = Field(env, 1);
}


void pushenvacc(){
  push(acc);
  acc = Field(env, read_uint8());
}

void push_retaddr_1b(){
  push(Val_int(extra_args));
  push(env);
  push(read_ptr_1B());
}


void push_retaddr_2b(){
  push(Val_int(extra_args));
  push(env);
  push(read_ptr_2B());
}


void push_retaddr_4b(){
  push(Val_int(extra_args));
  push(env);
  push(read_ptr_4B());
}

void apply(){
  extra_args = read_uint8() - 1;
  pc = Codeptr_val(Code_val(acc));
  env = acc;
}

void apply1(){
  val_t arg1 = pop();
  push(Val_int(extra_args));
  push(env);
  push(Val_codeptr(pc));
  push(arg1);
  pc = Codeptr_val(Code_val(acc));
  env = acc;
  extra_args = 0;
}

void apply2(){
  val_t arg1 = pop();
  val_t arg2 = pop();
  push(Val_int(extra_args));
  push(env);
  push(Val_codeptr(pc));
  push(arg2);
  push(arg1);
  pc = Codeptr_val(Code_val(acc));
  env = acc;
  extra_args = 1;
}

void apply3(){
  val_t arg1 = pop();
  val_t arg2 = pop();
  val_t arg3 = pop();
  push(Val_int(extra_args));
  push(env);
  push(Val_codeptr(pc));
  push(arg3);
  push(arg2);
  push(arg1);
  pc = Codeptr_val(Code_val(acc));
  env = acc;
  extra_args = 2;
}

void appterm(){
  uint8_t nargs = read_uint8();
  uint8_t slotsize = read_uint8();
  val_t * newsp = sp + slotsize - nargs;
  for (int i = nargs ; i > 0; i --) {
    newsp[i] = sp[i];
  }
  sp = newsp;
  pc = Codeptr_val(Code_val(acc));
  env = acc;
  extra_args += nargs - 1;
}

void appterm1(){
  val_t arg = peek(0);
  pop_n(read_uint8() - 1);
  push(arg);
  pc = Codeptr_val(Code_val(acc));
  env = acc;
}

void appterm2(){
  val_t arg1 = peek(0);
  val_t arg2 = peek(1);
  pop_n(read_uint8() - 2);
  push(arg2);
  push(arg1);
  pc = Codeptr_val(Code_val(acc));
  env = acc;
  extra_args ++;
}

void appterm3(){
  val_t arg1 = peek(0);
  val_t arg2 = peek(1);
  val_t arg3 = peek(2);
  pop_n(read_uint8() - 3);
  push(arg3);
  push(arg2);
  push(arg1);
  pc = Codeptr_val(Code_val(acc));
  env = acc;
  extra_args += 2;
}

void return_(){
  pop_n(read_uint8());
  if (extra_args > 0){
    extra_args --;
    pc = Codeptr_val(Code_val(acc));
    env = acc;
  } else {
    pc = Codeptr_val(pop());
    env = pop();
    extra_args = Int_val(pop());
  }
}

void restart(){
  uint8_t nargs = Wosize_val(env) - 2;
  uint8_t i;
  sp -= nargs;
  for (i = 1; i <= nargs; i ++) sp[i] = Field(env, i + 1);
  env = Field(env,1);
  extra_args += nargs;
}

void grab(){
  uint8_t n = read_uint8();
  uint8_t i;
  if (extra_args >= n){
    extra_args -= n;
  } else {
    /* Alloc_small(acc, extra_args + 3, Closure_tag); */
    Code_val(acc) = Val_codeptr(pc - 3);
    Field(acc, 1) = env;
    for (i = 0 ; i < n; i ++) {
      Field(env, i + 1) = pop();
    }
    pc = Codeptr_val(pop());
    env = pop();
    extra_args = pop();
  }
}

void closure_1b(){
  uint8_t n = read_uint8();
  code_t ptr = read_ptr_1B() - 1;
  uint8_t i;
  if (n != 0){
    push(acc);
  }
  /* Alloc_small(acc, n + 1, Closure_tag); */

  Code_val(acc) = Val_int(ptr);
  for (i = 0; i < n; i ++){
    Field(acc, i + 1) = pop();
  }
}

void pushoffsetclosurem2(){
  push(acc);
  acc = env - 2 * sizeof(val_t);
}

void offsetclosurem2(){
  acc = env - 2 * sizeof(val_t);
}

void pushoffsetclosure0(){
  push(acc);
  acc = env;
}

void offsetclosure0(){
  acc = env;
}

void pushoffsetclosure2(){
  push(acc);
  acc = env + 2 * sizeof(val_t);
}

void offsetclosure2(){
  acc = env + 2 * sizeof(val_t);
}

void pushgetglobal(){
  push(acc);
  acc = ocaml_global_data[read_uint8()];
}

void getglobal(){
  acc = ocaml_global_data[read_uint8()];
}

void pushgetglobalfield(){
  push(acc);
  uint8_t n = read_uint8();
  uint8_t p = read_uint8();
  acc = Field(ocaml_global_data[n], p);
}

void getglobalfield(){
  uint8_t n = read_uint8();
  uint8_t p = read_uint8();
  acc = Field(ocaml_global_data[n], p);
}

void setglobal_1b(){
  ocaml_global_data[read_uint8()] = acc;
  acc = Val_unit;
}

void setglobal_2b(){
  ocaml_global_data[read_uint16()] = acc;
  acc = Val_unit;
}

void pushatom0(){
  push(acc);
  acc = Val_block(&((&atom0_header)[1]));
}

void atom0(){
  acc = Val_block(&((&atom0_header)[1]));
}

void makeblock_1b(){
  tag_t tag = read_uint8();
  uint8_t size = read_uint8();
  val_t block;
  /* Alloc_small(block, size, tag); */
  Field(block, 0) = acc;
  for (uint8_t i = 1; i < size; i ++) Field(block, i) = pop();
  acc = block;
}

void makeblock1(){
  tag_t tag = read_uint8();
  val_t block;
  /* Alloc_small(block, 1, tag); */
  Field(block, 0) = acc;
  acc = block;
}

void makeblock2(){
  tag_t tag = read_uint8();
  val_t block;
  /* Alloc_small(block, 2, tag); */
  Field(block, 0) = acc;
  Field(block, 1) = pop();
  acc = block;
}

void makeblock3(){
  tag_t tag = read_uint8();
  val_t block;
  /* Alloc_small(block, 3, tag); */
  Field(block, 0) = acc;
  Field(block, 1) = pop();
  Field(block, 2) = pop();
  acc = block;
}

void getfield0(){
  acc = Field(acc, 0);
}

void getfield1(){
  acc = Field(acc, 1);
}


void getfield2(){
  acc = Field(acc, 2);
}

void getfield3(){
  acc = Field(acc, 3);
}

void setfield0(){
  Field(acc, 0) = pop();
  acc = Val_unit;
}

void setfield1(){
  Field(acc, 1) = pop();
  acc = Val_unit;
}

void setfield2(){
  Field(acc, 2) = pop();
  acc = Val_unit;
}

void setfield3(){
  Field(acc, 3) = pop();
  acc = Val_unit;
}

void setfield() {
  Field(acc, read_uint8()) = pop();
  acc = Val_unit;
}

void vectlength(){
  acc = Wosize_val(acc);
}

void getvectitem(){
  acc = Field(acc, Int_val(pop()));
}

void setvectitem(){
  val_t ind = pop();
  val_t val = pop();
  Field(acc, Int_val(ind)) = val;
  acc = Val_unit;
}

void getstringchar(){
  acc = Val_int(StringField(acc, Int_val(pop())));
}

void branch_1b(){
  pc = read_ptr_1B();
}

void branch_2b(){
  pc = read_ptr_2B();
}

void branch_4b(){
  pc = read_ptr_4B();
}

void branchifnot_1b(){
  if (acc == Val_false){
    pc = read_ptr_1B();
  } else {
    pc += 1;
  }
}

void branchifnot_2b(){
  if (acc == Val_false){
    pc = read_ptr_2B();
  } else {
    pc += 1;
  }
}

void branchifnot_4b(){
  if (acc == Val_false){
    pc = read_ptr_4B();
  } else {
    pc += 1;
  }
}

void switch_1b(){
  if (Is_int(acc)){
    uint8_t ofs = Int_val(acc) + 2;
    pc += ofs;
    pc = read_ptr_1B() - ofs;
  } else {
    tag_t idx = Tag_val(acc);
    uint16_t n = read_uint8();
    uint16_t ofs = n + idx + 1;
    pc += ofs;
    pc = read_ptr_1B() - ofs - 1;
  }
}

void switch_2b(){
  if (Is_int(acc)){
    uint8_t ofs = Int_val(acc) + 2;
    pc += ofs;
    pc = read_ptr_2B() - ofs;
  } else {
    tag_t idx = Tag_val(acc);
    uint16_t n = read_uint8();
    uint16_t ofs = n + idx + 1;
    pc += ofs;
    pc = read_ptr_2B() - ofs - 1;
  }
}


void switch_4b(){
  if (Is_int(acc)){
    uint8_t ofs = Int_val(acc) + 2;
    pc += ofs;
    pc = read_ptr_4B() - ofs;
  } else {
    tag_t idx = Tag_val(acc);
    uint16_t n = read_uint8();
    uint16_t ofs = n + idx + 1;
    pc += ofs;
    pc = read_ptr_4B() - ofs - 1;
  }
}

void boolnot(){
  acc = acc ^ 2;
}

void pushtrap_1b(){
  push(Val_int(extra_args));
  push(trapSp);
  push(read_ptr_1B());
  trapSp = Val_int(sp - ocaml_stack);
}

void pushtrap_2b(){
  push(Val_int(extra_args));
  push(trapSp);
  push(read_ptr_2B());
  trapSp = Val_int(sp - ocaml_stack);
}


void pushtrap_4b(){
  push(Val_int(extra_args));
  push(trapSp);
  push(read_ptr_4B());
  trapSp = Val_int(sp - ocaml_stack);
}

void poptrap(){
  pop();
  trapSp = pop();
  pop();
  pop();
}

void raise(){
  if (trapSp == 0) {
    return Val_unit;
  } else {
    sp = ocaml_stack + Int_val(trapSp);
    pc = Codeptr_val(pop());
    trapSp = pop();
    env = pop();
    extra_args = Int_val(pop());
  }
}

void c_call1(){
  acc = ((val_t (*)(val_t)) (get_primitive(read_uint8())))(acc);
}


void c_call2(){
  acc = ((val_t (*)(val_t, val_t)) (get_primitive(read_uint8())))(acc, sp[1]);
  pop();
}

void c_call3(){
  acc = ((val_t (*)(val_t, val_t, val_t)) (get_primitive(read_uint8())))(acc, sp[1], sp[2]);
  pop();
  pop();
}

void c_call4(){
  acc = ((val_t (*)(val_t, val_t, val_t, val_t)) (get_primitive(read_uint8())))(acc, sp[1], sp[2], sp[3]);
  pop();
  pop();
  pop();
}

void c_call5(){
  acc = ((val_t (*)(val_t, val_t, val_t, val_t, val_t)) (get_primitive(read_uint8())))(acc, sp[1], sp[2], sp[3], sp[4]);
  pop();
  pop();
  pop();
  pop();
}

void c_calln(){
  uint8_t narg = read_uint8();
  uint8_t prim = read_uint8();
  push(acc);
  acc = ((val_t (*)(uint8_t, val_t *)) (get_primitive(prim)))(narg, sp + 1);
  pop_n(narg);
}

void const0(){
  acc = Val_int(0);
}

void const1(){
  acc = Val_int(0);
}

void const2(){
  acc = Val_int(0);
}

void const3(){
  acc = Val_int(0);
}

void constint_1b(){
  acc = Val_int(read_int8());
}

void constint_2b(){
  acc = Val_int(read_int16());
}

void constint_4b(){
  acc = Val_int(read_int32());
}

void pushconst0(){
  push(acc);
  acc = Val_int(0);
}

void pushconstint_1b(){
  push(acc);
  acc = Val_int(read_int8());
}


void pushconstint_2b(){
  push(acc);
  acc = Val_int(read_int16());
}


void pushconstint_4b(){
  push(acc);
  acc = Val_int(read_int32());
}

void negint(){
  acc = Val_int(- Int_val(acc));
}

void addint(){
  acc = Val_int((Int_val(acc) + Int_val(pop())));
}


void subint(){
  acc = Val_int((Int_val(acc) - Int_val(pop())));
}

void mulint(){
  acc = Val_int((Int_val(acc) * Int_val(pop())));
}

void divint(){
  int32_t divisor = Int_val(pop());
  if (divisor == 0){
    caml_raise_division_by_zero();
  }
  acc = Val_int(Int_val(acc) / divisor);
}

void modint(){
  int32_t divisor = Int_val(pop());
  if (divisor == 0){
    caml_raise_division_by_zero();
  }
  acc = Val_int(Int_val(acc) % divisor);
  /* TODO MACRO */
}

void andint(){
  acc = Val_int((Int_val(acc) & Int_val(pop())));
}

void orint(){
  acc = Val_int((Int_val(acc) | Int_val(pop())));
}

void xorint(){
  acc = Val_int(Int_val(acc) ^ Int_val(pop()));
}

void lslint(){
  acc = Val_int(Int_val(acc) << Int_val(pop()));
}

void lsrint(){
  acc = Val_int((uval_t)(Int_val(acc)) >> Int_val(pop()));
}

void asrint(){
  acc = Val_int(Int_val(acc) >> Int_val(pop()));
}

void eq(){
  acc = (acc == pop()) ? Val_int(1) : Val_int(0);
}


void neq(){
  acc = (acc == pop()) ? Val_int(0) : Val_int(1);
}

void ltint(){
  acc = (acc < pop()) ? Val_int(1) : Val_int(0);
}

void leint(){
  acc = (acc <= pop()) ? Val_int(1) : Val_int(0);
}

void gtint(){
  acc = (acc > pop()) ? Val_int(1) : Val_int(0);
}

void geint(){
  acc = (acc >= pop()) ? Val_int(1) : Val_int(0);
}

void ultint(){
  acc = ((uval_t) acc < (uval_t) pop()) ? Val_int(1) : Val_int(0);
}

void ugeint(){
  acc = ((uval_t) acc >= (uval_t) pop()) ? Val_int(1) : Val_int(0);
}

void offsetint_1b(){
  acc = Val_int(Int_val(acc) + read_int8());
}

void offsetint_2b(){
  acc = Val_int(Int_val(acc) + read_int16());
}

void offsetint_4b(){
  acc = Val_int(Int_val(acc) + read_int32());
}

void offsetref_1b(){
  Field(acc, 0) = Val_int(Int_val(Field(acc, 0)) + read_int8());
  acc = Val_unit;
}

void offsetref_2b(){
  Field(acc, 0) = Val_int(Int_val(Field(acc, 0)) + read_int16());
  acc = Val_unit;
}

void offsetref_4b(){
  Field(acc, 0) = Val_int(Int_val(Field(acc, 0)) + read_int32());
  acc = Val_unit;
}

void isint(){
  acc = Is_int(acc) ? Val_int(1) : Val_int(0);
}

void getmethod(){
  val_t x = peek(0);
  val_t y = Field(x, 0);
  acc = Field(y, Int_val(acc));
}

void beq_1b(){
  if (Val_int(read_int8()) == acc) {
    pc = read_ptr_1B() - 1;
  } else {
    pc += 1;
  }
}

void beq_2b(){
  if (Val_int(read_int16()) == acc) {
    pc = read_ptr_2B() - 1;
  } else {
    pc += 2;
  }
}

void beq_4b(){
  if (Val_int(read_int32()) == acc) {
    pc = read_ptr_4B() - 1;
  } else {
    pc += 4;
  }
}

void bneq_1b(){
  if (Val_int(read_int8()) != acc) {
    pc = read_ptr_1B() - 1;
  } else {
    pc += 2;
  }
}

void bltint_1b(){
  if (Val_int(read_int8()) != acc) {
    pc = read_ptr_1B() - 1;
  } else {
    pc += 1;
  }
}

void bltint_2b(){
  if (Val_int(read_int16()) < acc) {
    pc = read_ptr_2B() - 2;
  } else {
    pc += 2;
  }
}

void bltint_4b(){
  if (Val_int(read_int32()) < acc) {
    pc = read_ptr_4B() - 4;
  } else {
    pc += 4;
  }
}

void bleint_1b(){
  if (Val_int(read_int8()) <= acc) {
    pc = read_ptr_1B() - 1;
  } else {
    pc += 1;
  }
}


void bleint_2b(){
  if (Val_int(read_int16()) <= acc) {
    pc = read_ptr_2B() - 1;
  } else {
    pc += 2;
  }
}

void bleint_4b(){
  if (Val_int(read_int32()) <= acc) {
    pc = read_ptr_4B() - 4;
  } else {
    pc += 4;
  }

}

void bgtint_1b(){
  if (Val_int(read_int8()) > acc) {
    pc = read_ptr_1B() - 1;
  } else {
    pc += 1;
  }
}

void bgeint_1b(){
  if (Val_int(read_int8()) >= acc) {
    pc = read_ptr_1B() - 1;
  } else {
    pc += 1;
  }
}

void bultint_1b(){
  if ((uval_t) Val_int(read_int8()) < (uval_t) acc) {
    pc = read_ptr_1B() - 1;
  } else {
    pc += 1;
  }
}

void bugeint(){
  if ((uval_t) Val_int(read_int8()) >= (uval_t) acc) {
    pc = read_ptr_1B() - 1;
  } else {
    pc += 1;
  }
}

void getpubmet(){
  push(acc);
  acc = Val_int(read_uint32());
  val_t meths = Field(peek(0),0);
  int li = 3, hi = Field(meths,0), mi;
  while (li < hi){
    mi = ((li+hi) >> 1) | 1;
    if (acc < Field (meths,mi)) hi = mi-2;
    else li = mi;
  }
  acc = Field(meths,li-1);
}

void getdynmet(){
  val_t meths = Field(peek(0),0);
  int li = 3, hi = Field(meths,0), mi;
  while (li < hi){
    mi = ((li+hi) >> 1) | 1;
    if (acc < Field (meths,mi)) hi = mi-2;
    else li = mi;
  }
  acc = Field(meths,li-1);
}

val_t stop_(){
  return acc;
}


int main(){
  return 0;
}
