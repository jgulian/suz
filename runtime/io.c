#include "stdio.h"
#include "stddef.h"
#include "stdlib.h"

uint8_t read_u8(void) {
  return getchar();
}

uintptr_t read_usize(void) {
  uintptr_t x;
  scanf("%lu", &x);
  return x;
}

intptr_t read_isize(void) {
  uintptr_t x;
  scanf("%ld", &x);
  return x;
}

double read_fsize(void) {
  double x;
  scanf("%lf", &x);
  return x;
}

void print_u8(uint8_t data) {
  putchar(data);
}

void print_usize(uintptr_t data) {
  printf("%lu\n", data);
}

void print_isize(intptr_t data) {
  printf("%ld\n", data);
}

void print_fsize(double data) {
  printf("%lf\n", data);
}

void newline(void) {
  puts("");
}

u_int8_t* alloc(uintptr_t size) {
  return malloc(size);
}

void dealloc(u_int8_t* ptr, uintptr_t size) {
  return free(ptr);
}