#include "stdio.h"
#include "stddef.h"

uintptr_t read_int(void) {
  uintptr_t x;
  scanf("%lu", &x);
  return x;
}

void print_int(uintptr_t data) {
  printf("%lu\n", data);
}