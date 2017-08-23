#include "debug.h"

#ifndef __AVR__

#include <stdio.h>
#include "simul.h"

void debug_init(void) {}

void debug(int n) {
  printf("debug(%d)\n", n);
}

void debug_blink(int led, int n) {
  printf("debug_blink(%d, %d)\n", led, n);
}

#else

void debug_init(void) {

}

void debug(int n) {
}

void debug_blink(int led, int n) {
}

void assert_failure(void) {
}

#endif
