#include <stdio.h>

extern double lambda_main();

int main (int argc, char **argv) {

  printf("Lambda value: %e\n", lambda_main());
}
