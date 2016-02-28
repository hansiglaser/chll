/**
 *
 */

#include <msp430f1232.h>
#include <stdint.h>

#include <extadcsimple.h>

/* This function is called by the driver when an interrupt is serviced. */
void user_isr() {
  uint16_t SensorValue = app_get_value();
  // ...
}

int main() {
  // Stop watchdog timer
  WDTCTL = WDTPW + WDTHOLD;

  app_setup();

  app_start();

  // Clear the timer and enable timer interrupt
  __enable_interrupt();

  // infinite loop
  while (1) {
    LPM3;
  }

  return 0;
}
