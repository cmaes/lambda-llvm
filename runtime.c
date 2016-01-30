#include <stdio.h>

extern double lambda_main();

extern double putchard(double x) {
  putchar( (char) x);
  return 0;
}

extern double printd(double x) {
  printf("%g\n", x);
  return 0;
}

int main (int argc, char **argv) {

  printf("Lambda value: %g\n", lambda_main());
}
