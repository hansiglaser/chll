/**
 *
 */

#include <msp430f1232.h>

#include "cfgintf.h"
#include "paramintf.h"
#include "reconfmodule.h"
#include "slowadt7410.h"
#ifdef WRAPAPP
#  include "slowadt7410-wrapapp-driver.h"
#else
#  include "slowadt7410-driver.h"
#endif

#ifndef SIMULATION
// delay between start of conversion command and query command, must be >
// 240ms, the value is given in clock cycles
#define I2C_DELAY (2600000UL)
#endif

void app_setup() {
  P3OUT = 0x00;
  init_app_slowadt7410();
#ifndef SIMULATION
  // For simulation we use a shorter value to reduce simulation time. The value
  // is given as default value in chll/sripts/setup.tcl as 24000, which at a
  // clock frequency of 100kHz provides 240ms delay.
  // On chip we use e.g. 10MHz and therefore need a value of 2400000, which
  // is defined above as SPI_DELAY.
  ParamWrite(PARAM_WAITCOUNTERPRESETL_I_ADDR,I2C_DELAY & 0x0000FFFF);
  ParamWrite(PARAM_WAITCOUNTERPRESETH_I_ADDR,I2C_DELAY >> 16);
#endif
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

void app_set_periode(uint32_t periode) {
  ParamWrite(PARAM_PERIODCOUNTERPRESETH_I_ADDR, periode >> 16);
  ParamWrite(PARAM_PERIODCOUNTERPRESETL_I_ADDR, periode & 0x0000FFFFUL);
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
