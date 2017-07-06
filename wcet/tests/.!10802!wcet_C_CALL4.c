#define OCAML_C_CALL4 1
#define DEBUG

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
#define OCAML_STACK_WOSIZE             32
#define OCAML_HEAP_WOSIZE             128
#define OCAML_HEAP_INITIAL_WOSIZE       2
#define OCAML_STACK_INITIAL_WOSIZE      1
#define OCAML_GLOBDATA_NUMBER           0
#define OCAML_BYTECODE_BSIZE           43
#define OCAML_PRIMITIVE_NUMBER          3
#define OCAML_VIRTUAL_ARCH             32

val_t ocaml_heap[OCAML_HEAP_WOSIZE];
val_t ocaml_bytecode[OCAML_BYTECODE_BSIZE];
val_t ocaml_stack[OCAML_STACK_WOSIZE];
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
    switch(OCAML_C_CALL4){

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
    case OCAML_ACC3 : {
      acc = peek(3);
      break;
    }
#endif

#ifdef OCAML_ACC4
    case OCAML_ACC4 : {
      acc = peek(4);
      break;
    }
#endif

#ifdef OCAML_ACC5
    case OCAML_ACC5 : {
      acc = peek(5);
      break;
    }
#endif

#ifdef OCAML_ACC6
    case OCAML_ACC6 : {
      acc = peek(6);
      break;
    }
#endif

#ifdef OCAML_ACC7
    case OCAML_ACC7 : {
      acc = peek(7);
      break;
    }
#endif

#ifdef OCAML_ACC
    case OCAML_ACC : {
      acc = peek(read_uint8());
      break;
    }
#endif

#ifdef OCAML_PUSH
    case OCAML_PUSH : {
      push(acc);
      break;
    }
#endif

#ifdef OCAML_PUSHACC1
    case OCAML_PUSHACC1 : {
      push(acc);
      acc = peek(1);
      break;
    }
#endif

#ifdef OCAML_PUSHACC2
    case OCAML_PUSHACC2 : {
      push(acc);
      acc = peek(2);
      break;
    }
#endif

#ifdef OCAML_PUSHACC3
    case OCAML_PUSHACC3 : {
      push(acc);
      acc = peek(3);
      break;
    }
#endif

#ifdef OCAML_PUSHACC4
    case OCAML_PUSHACC4 : {
      push(acc);
      acc = peek(4);
      break;
    }
#endif

#ifdef OCAML_PUSHACC5
    case OCAML_PUSHACC5 : {
      push(acc);
      acc = peek(5);
      break;
    }
#endif

#ifdef OCAML_PUSHACC6
    case OCAML_PUSHACC6 : {
      push(acc);
      acc = peek(6);
      break;
    }
#endif

#ifdef OCAML_PUSHACC7
    case OCAML_PUSHACC7 : {
      push(acc);
      acc = peek(7);
      break;
    }
#endif

#ifdef OCAML_PUSHACC
    case OCAML_PUSHACC : {
      push(acc);
      acc = peek(read_uint8());
      break;
    }
#endif

#ifdef OCAML_POP
    case OCAML_POP : {
      pop_n(read_uint8());
      break;
    }
#endif

#ifdef OCAML_ASSIGN
    case OCAML_ASSIGN : {
      sp[read_uint8() + 1] = acc;
      acc = Val_unit;
      break;
    }
#endif

#ifdef OCAML_ENVACC1
    case OCAML_ENVACC1 : {
      acc = Field(env, 1);
      break;
    }
#endif

#ifdef OCAML_ENVACC2
    case OCAML_ENVACC2 : {
      acc = Field(env, 2);
      break;
    }
#endif

#ifdef OCAML_ENVACC3
    case OCAML_ENVACC3 : {
      acc = Field(env, 3);
      break;
    }
#endif

#ifdef OCAML_ENVACC4
    case OCAML_ENVACC4 : {
      acc = Field(env, 4);
      break;
    }
#endif

#ifdef OCAML_ENVACC
    case OCAML_ENVACC : {
      acc = Field(env, read_uint8());
      break;
    }
#endif

#ifdef OCAML_PUSHENVACC1
    case OCAML_PUSHENVACC1 : {
      push(acc);
      acc = Field(env, 1);
      break;
    }
#endif

#ifdef OCAML_PUSHENVACC2
    case OCAML_PUSHENVACC2 : {
      push(acc);
      acc = Field(env, 2);
      break;
    }
#endif

#ifdef OCAML_PUSHENVACC3
    case OCAML_PUSHENVACC3 : {
      push(acc);
      acc = Field(env, 3);
      break;
    }
#endif

#ifdef OCAML_PUSHENVACC4
    case OCAML_PUSHENVACC4 : {
      push(acc);
      acc = Field(env, 4);
      break;
    }
#endif

#ifdef OCAML_PUSHENVACC
    case OCAML_PUSHENVACC : {
      push(acc);
      acc = Field(env, read_uint8());
      break;
    }
#endif

#ifdef OCAML_PUSH_RETADDR_1B
    case OCAML_PUSH_RETADDR_1B : {
      push(Val_int(extra_args));
      push(env);
      push(read_ptr_1B());
      break;
    }
#endif

#ifdef OCAML_PUSH_RETADDR_2B
    case OCAML_PUSH_RETADDR_2B : {
      push(Val_int(extra_args));
      push(env);
      push(read_ptr_2B());
      break;
    }
#endif

#ifdef OCAML_PUSH_RETADDR_4B
    case OCAML_PUSH_RETADDR_4B : {
      push(Val_int(extra_args));
      push(env);
      push(read_ptr_4B());
      break;
    }
#endif

#ifdef OCAML_APPLY
    case OCAML_APPLY : {
      extra_args = read_uint8() - 1;
      pc = Codeptr_val(Code_val(acc));
      env = acc;
      break;
    }
#endif

#ifdef OCAML_APPLY1
    case OCAML_APPLY1 : {
      val_t arg1 = pop();
      push(Val_int(extra_args));
      push(env);
      push(Val_codeptr(pc));
      push(arg1);
      pc = Codeptr_val(Code_val(acc));
      env = acc;
      extra_args = 0;
      break;
    }
#endif

#ifdef OCAML_APPLY2
    case OCAML_APPLY2 : {
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
      break;
    }
#endif

#ifdef OCAML_APPLY3
    case OCAML_APPLY3 : {
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
      break;
    }
#endif

#ifdef OCAML_APPTERM
    case OCAML_APPTERM : {
      uint8_t nargs = read_uint8();
      uint8_t slotsize = read_uint8();
      val_t * newsp = sp + slotsize - nargs;
      for (int i = nargs ; i >= 0; i --) {
        newsp[i] = sp[i];
      }
      sp = newsp;
      pc = Codeptr_val(Code_val(acc));
      env = acc;
      extra_args += nargs - 1;
      break;
    }
#endif

#ifdef OCAML_APPTERM1
    case OCAML_APPTERM1 : {
      val_t arg = peek(0);
      pop_n(read_uint8());
      push(arg);
      pc = Codeptr_val(Code_val(acc));
      env = acc;
      break;
    }
#endif

#ifdef OCAML_APPTERM2
    case OCAML_APPTERM2 : {
      val_t arg1 = peek(0);
      val_t arg2 = peek(1);
      pop_n(read_uint8());
      push(arg2);
      push(arg1);
      pc = Codeptr_val(Code_val(acc));
      env = acc;
      extra_args ++;
      break;
    }
#endif

#ifdef OCAML_APPTERM3
    case OCAML_APPTERM3 : {
      val_t arg1 = peek(0);
      val_t arg2 = peek(1);
      val_t arg3 = peek(2);
      pop_n(read_uint8());
      push(arg3);
      push(arg2);
      push(arg1);
      pc = Codeptr_val(Code_val(acc));
      env = acc;
      extra_args += 2;
      break;
    }
#endif

#ifdef OCAML_RETURN
    case OCAML_RETURN : {
      uint8_t n = read_uint8();
      pop_n(n);
      if (extra_args > 0){
        extra_args --;
        pc = Codeptr_val(Code_val(acc));
        env = acc;
      } else {
        pc = Codeptr_val(pop());
        env = pop();
        extra_args = Int_val(pop());
      }
      break;
    }
#endif

#ifdef OCAML_RESTART
    case OCAML_RESTART : {
      uint8_t nargs = Wosize_val(env) - 2;
      uint8_t i;
      sp -= nargs;
      for (i = 1; i <= nargs; i ++) sp[i] = Field(env, i + 1);
      env = Field(env,1);
      extra_args += nargs;
      break;
    }
#endif

#ifdef OCAML_GRAB
    case OCAML_GRAB : {
      uint8_t n = read_uint8();
      uint8_t i;
      if (extra_args >= n){
        extra_args -= n;
      } else {
        Alloc_small(acc, extra_args + 3, Closure_tag);
        Code_val(acc) = Val_codeptr(pc - 3);
        Field(acc, 1) = env;
        for (i = 0 ; i < n; i ++) {
          Field(env, i + 1) = pop();
        }
        pc = Codeptr_val(pop());
        env = pop();
        extra_args = pop();
      }
      break;
    }
#endif

#ifdef OCAML_CLOSURE_1B
    case OCAML_CLOSURE_1B : {
      uint8_t n = read_uint8();
      code_t ptr = read_ptr_1B() - 1;
      uint8_t i;
      if (n != 0){
        push(acc);
      }
      Alloc_small(acc, n + 1, Closure_tag);
      Code_val(acc) = Val_int(ptr);
      for (i = 0; i < n; i ++){
        Field(acc, i + 1) = pop();
      }
      break;
    }
#endif

#ifdef OCAML_CLOSURE_2B
    case OCAML_CLOSURE_2B : {
      uint8_t n = read_uint8();
      code_t ptr = read_ptr_2B() - 1;
      uint8_t i;
      if (n != 0){
        push(acc);
      }
      Alloc_small(acc, n + 1, Closure_tag);
      Code_val(acc) = Val_int(ptr);
      for (i = 0; i < n; i ++){
        Field(acc, i + 1) = pop();
      }
      break;
    }
#endif

#ifdef OCAML_CLOSURE_4B
    case OCAML_CLOSURE_4B : {
      uint8_t n = read_uint8();
      code_t ptr = read_ptr_4B() - 1;
      uint8_t i;
      if (n != 0){
        push(acc);
      }
      Alloc_small(acc, n + 1, Closure_tag);
      Code_val(acc) = Val_int(ptr);
      for (i = 0; i < n; i ++){
        Field(acc, i + 1) = pop();
      }
      break;
    }
#endif

#ifdef OCAML_CLOSUREREC_1B
    case OCAML_CLOSUREREC_1B : {
       /* f = number of functions */
       /* v = number of variables */
      /* o = array of vars ?  */
      uint8_t f = read_uint8();
      uint8_t v = read_uint8();
      code_t o = read_ptr_1B() -2 ;
      int blksize = f * 2 - 1 + v;
      int i;
      if (v > 0) {
        push(acc);
      }
      Alloc_small(acc, blksize, Closure_tag);
      Field(acc, 0) = Val_int(o);
      for (i = 1; i < f; i ++) {
        Field(acc, 2 * i - 1) = Make_header(2 * i, Infix_tag);
        Field(acc, 2 * i) = read_ptr_1B() - i - 2;
      }
      for (; i < blksize ; i ++) {
        Field(acc, i + 2 * f - 1) = pop();
      }
      push(acc);
      break;
    }
#endif

#ifdef OCAML_CLOSUREREC_2B
    case OCAML_CLOSUREREC_2B : {
      uint8_t f = read_uint8();
      uint8_t v = read_uint8();
      code_t o = read_ptr_2B() - 2;
      int i;
      if (v > 0) {
        push(acc);
      }
      Alloc_small(acc, 2 * f - 1 + v, Closure_tag);
      Field(acc, 0) = o;
      for (i = 1; i < f; i ++) {
        Field(acc, 2 * i - 1) = Make_header(2 * i, Infix_tag);
        Field(acc, 2 * i) = read_ptr_2B() - 2 * i - 2;
      }
      for (; i < 2 * f - 1 + v ; i ++) {
        Field(acc, i + 2 * f - 1) = pop();
      }
       break;
    }
#endif

#ifdef OCAML_CLOSUREREC_4B
    case OCAML_CLOSUREREC_4B : {
      uint8_t f = read_uint8();
      uint8_t v = read_uint8();
      code_t o = read_ptr_4B() - 2;
      int i;
      if (v > 0) {
        push(acc);
      }
      Alloc_small(acc, 2 * f - 1 + v, Closure_tag);
      Field(acc, 0) = o;
      for (i = 1; i < f; i ++) {
        Field(acc, 2 * i - 1) = Make_header(2 * i, Infix_tag);
        Field(acc, 2 * i) = read_ptr_4B() - 4 * i - 2;
      }
      for (; i < 2 * f - 1 + v ; i ++) {
        Field(acc, i + 2 * f - 1) = pop();
      }
      break;
    }
#endif

#ifdef OCAML_PUSHOFFSETCLOSUREM2
    case OCAML_PUSHOFFSETCLOSUREM2 : {
      push(acc);
      /* fallthrough */
    }
#endif

#ifdef OCAML_OFFSETCLOSUREM2
    case OCAML_OFFSETCLOSUREM2 :
#endif

#if defined(OCAML_PUSHOFFSETCLOSUREM2) || defined(OCAML_OFFSETCLOSUREM2)
      {
        /* value + header */
        acc = env - 2 * sizeof(val_t);
        break;
      }
#endif

#ifdef OCAML_PUSHOFFSETCLOSURE0
    case OCAML_PUSHOFFSETCLOSURE0 : {
      push(acc);
      /* fallthrough */
    }
#endif

#ifdef OCAML_OFFSETCLOSURE0
    case OCAML_OFFSETCLOSURE0 :
#endif

#if defined(OCAML_PUSHOFFSETCLOSURE0) || defined(OCAML_OFFSETCLOSURE0)
      {
        acc = env;
        break;
      }
#endif

#ifdef OCAML_PUSHOFFSETCLOSURE2
    case OCAML_PUSHOFFSETCLOSURE2 : {
      push(acc);
      /* fallthrough */
    }
#endif

#ifdef OCAML_OFFSETCLOSURE2
    case OCAML_OFFSETCLOSURE2 :
#endif

#if defined(OCAML_PUSHOFFSETCLOSURE2) || defined(OCAML_OFFSETCLOSURE2)
      {
        acc = env + 2 * sizeof(val_t);
        break;
      }
#endif

#ifdef OCAML_PUSHOFFSETCLOSURE
    case OCAML_PUSHOFFSETCLOSURE : {
      push(acc);
      /* fallthrough */
    }
#endif

#ifdef OCAML_OFFSETCLOSURE
    case OCAML_OFFSETCLOSURE :
#endif

#if defined(OCAML_PUSHOFFSETCLOSURE) || defined(OCAML_OFFSETCLOSURE)
      {
        int n = read_int8();
        acc = env + n * sizeof(val_t);
        break;
      }
#endif

#ifdef OCAML_PUSHGETGLOBAL_1B
    case OCAML_PUSHGETGLOBAL_1B : {
      push(acc);
      /* fallthrough */
    }
#endif

#ifdef OCAML_GETGLOBAL_1B
    case OCAML_GETGLOBAL_1B :
#endif

#if defined(OCAML_PUSHGETGLOBAL_1B) || defined(OCAML_GETGLOBAL_1B)
      {
        acc = ocaml_global_data[read_uint8()];
        break;
      }
#endif

#ifdef OCAML_PUSHGETGLOBAL_2B
    case OCAML_PUSHGETGLOBAL_2B : {
      push(acc);
      /* fallthrough */
    }
#endif

#ifdef OCAML_GETGLOBAL_2B
    case OCAML_GETGLOBAL_2B :
#endif

#if defined(OCAML_PUSHGETGLOBAL_2B) || defined(OCAML_GETGLOBAL_2B)
      {
        acc = ocaml_global_data[read_uint16()];
        break;
      }
#endif

#ifdef OCAML_PUSHGETGLOBALFIELD_1B
    case OCAML_PUSHGETGLOBALFIELD_1B : {
      push(acc);
      /* fallthrough */
    }
#endif

#ifdef OCAML_GETGLOBALFIELD_1B
    case OCAML_GETGLOBALFIELD_1B :
#endif

#if defined(OCAML_PUSHGETGLOBALFIELD_1B) || defined(OCAML_GETGLOBALFIELD_1B)
      {
        uint8_t n = read