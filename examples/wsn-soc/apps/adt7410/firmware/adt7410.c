/**
 *
 */

#include <msp430f1232.h>

#include "cfgintf.h"
#include "paramintf.h"
#include "reconfmodule.h"
#include "adt7410.h"
#ifdef WRAPAPP
#  include "adt7410-wrapapp-driver.h"
#else
#  include "adt7410-driver.h"
#endif

void app_setup() {
  P3OUT = 0x00;
  init_app_adt7410();
}

void app_start() {
  P3OUT |= (1 << ARRAY_MAP_RECONFMODULEIN_S_ENABLE_I);
}

void app_stop() {
  P3OUT &= ~(1 << ARRAY_MAP_RECONFMODULEIN_S_ENABLE_I);
}

void app_set_threshold(uint16_t threshold) {
  ParamWrite(PARAM_THRESHOLD_I_ADDR, threshold);
}

void app_set_periode(uint16_t periode) {
  ParamWrite(PARAM_PERIODCOUNTERPRESET_I_ADDR, periode);
}

uint16_t app_get_value() {
  return ParamRead(PARAM_SENSORVALUE_O_ADDR);
}

#pragma vector = VECTOR_RECONFMODULEIRQS_S_0
__interrupt void app_isr (void) {
  // call user's ISR function
  user_isr();
}

/* These functions should be in reconfmodule.c. Unfortunately, there we don't
 * have access to the constants PARAM_*_ADDR, because they are defined in
 * <app>-driver.h. Most of the constants defined there are global for the whole
 * reconf.module and should be put in a separate (common) file. Currently we
 * accept this not-very-clean design to avoid delays.
 */
uint16_t get_i2c_errors() {
  return ParamRead(PARAM_I2C_ERRORS_ADDR);
}

void ack_i2c_error() {
  ParamWrite(PARAM_I2C_ERRACKPARAM_ADDR, 0x0001);
  ParamWrite(PARAM_I2C_ERRACKPARAM_ADDR, 0x0000);
}
