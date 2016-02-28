/**
 *
 */

#include <msp430f1232.h>
#include <stdint.h>

#include <<app>.h>

/* This function is called by the driver when an interrupt is serviced. */
void user_isr() {
  // ...
}

int main() {
  app_setup();

  app_start();

  // Clear the timer and enable timer interrupt
  __enable_interrupt();

  // infinite loop
  while (1) {}

  return 0;
}
