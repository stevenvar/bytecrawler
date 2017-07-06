#define OCAML_DIVINT
#define OCAML_STACK_WOSIZE             32
#define OCAML_HEAP_WOSIZE             128
#define OCAML_HEAP_INITIAL_WOSIZE       2
#define OCAML_STACK_INITIAL_WOSIZE      1
#define OCAML_GLOBDATA_NUMBER           0
#define OCAML_BYTECODE_BSIZE           43
#define OCAML_PRIMITIVE_NUMBER          3
#define OCAML_VIRTUAL_ARCH             32
#include <stdint.h>

#include "debug.h"
#include "values.h"
#include "gc.h"

/******************************************************************************/

/* Registers for the abstract machine:
   pc          the code pointer
   sp          the stack pointer (grows downward)
   acc         the accumulator
   env         heap-allocated environment
   trapSp      pointer to the current trap frame
   extra_args  number of extra arguments provided by the caller
*/

val_t atom0_header = Make_header(0, 0);

PROGMEM extern void * const ocaml_primitives[];

val_t ocaml_bytecode[];
val_t ocaml_stack[];
int serialEventRun(){return 0;}
val_t acc;
static code_t pc;
static val_t env;
static val_t *sp;
static val_t trapSp;
static uint8_t extra_args;


void caml_raise_stack_overflow(void) {
#ifdef DEBUG
  debug(444);
#endif
  assert(0);
  /* TODO */
}

/******************************************************************************/

void *get_primitive(uint8_t prim_ind) {
#ifdef __AVR__
  return (void *) pgm_read_word_near(ocaml_primitives + prim_ind);
#else
  return ocaml_primitives[prim_ind];
#endif
}

char read_byte(void) {
  char c;
#ifdef __AVR__
  c = pgm_read_byte_near(ocaml_bytecode + pc);
#else
  c = ocaml_bytecode[pc];
#endif
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
  return sp[(val_t) n];
}

void push(val_t x) {
  if(sp < ocaml_stack){
    caml_raise_stack_overflow();
  }
  else {
    *--sp = x;
  }
}

val_t pop(void) {
  return *(sp++);
}

void pop_n(int n) {
  sp += n;
}

/******************************************************************************/

void caml_raise_division_by_zero(void) {
  assert(0);
  /* TODO */
}

/******************************************************************************/

void interp_init(void) {
  sp = ocaml_stack + OCAML_STACK_WOSIZE - OCAML_STACK_INITIAL_WOSIZE;
  trapSp = Val_int(0);
  env = Val_unit;
  extra_args = 0;
  pc = 0;
}

/******************************************************************************/

val_t interp(void) {
  while (1) {
#ifdef __AVR__
    if (serialEventRun) {
      serialEventRun();
    }
#endif

    opcode_t opcode = read_opcode();
    /* sp pointe sur le dernier bloc écrit  */
    for(int i = 0; i <= 32; i ++){
      printf("stack[%d] = %d\n", i, Int_val(sp[i]));
    }
    printf("\n");

    printf("PC = %d\nINSTR=%d\nACC=%d\n\n", pc-1,opcode,Int_val(acc));

#ifdef DEBUG
    debug(pc-1);
#endif
    switch(OCAML_DIVINT){

#ifdef OCAML_ACC0
    case OCAML_ACC0 : {
      acc = peek(0);
      break;
    }
#endif

#ifdef OCAML_ACC1
    case OCAML_ACC1 : {
      acc = peek(1);
      break;
    }
#endif

#ifdef OCAML_ACC2
    case OCAML_ACC2 : {
      acc = peek(2);
      break;
    }
#endif

#ifdef OCAML_ACC3
    case OCAML_AC