#include <stdio.h>

extern double lambda_main();

int main (int argc, char **argv) {

  printf("Lambda value: %g\n", lambda_main());
}
