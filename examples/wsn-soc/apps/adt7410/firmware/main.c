/**
 *
 */

#include <msp430f1232.h>
#include <stdint.h>

#include <adt7410.h>

/* This function is called by the driver when an interrupt is serviced. */
void user_isr() {
  // check if this interrupt was due to an I2C error
  uint16_t I2CErrors = get_i2c_errors();
  if (I2CErrors != 0) {
    ack_i2c_error();
    // disable and enable the app. to leave "error" state
    app_stop();
    app_start();
    return;
  }
  // handle new value
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
