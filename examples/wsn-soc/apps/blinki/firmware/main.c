/**
 *
 */

#include <msp430f1232.h>
#include <stdint.h>

#include <blinki.h>

int main() {
  // Stop watchdog timer
  WDTCTL = WDTPW + WDTHOLD;

  app_setup();

  // set 1Hz, assume 9.6MHz Clk_i from LPC11U34
  app_set_periode(4799999);

  // Clear the timer and enable timer interrupt
  __enable_interrupt();

  // infinite loop
  while (1) {
    LPM3;
  }

  return 0;
}
