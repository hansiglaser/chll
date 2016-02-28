/**
 *
 */

#include <msp430f1232.h>

#include "cfgintf.h"
#include "paramintf.h"
#include "reconfmodule.h"
#include "adt7310.h"
#ifdef WRAPAPP
#  include "adt7310-wrapapp-driver.h"
#else
#  include "adt7310-driver.h"
#endif

#ifndef SIMULATION
// delay between start of conversion command and query command, must be >
// 240ms, the value is given in clock cycles
#define SPI_DELAY (2600000UL)
#endif

void app_setup() {
  P3OUT = 0x00;
  init_app_adt7310();
#ifndef SIMULATION
  // For simulation we use a shorter value to reduce simulation time. The value
  // is given as default value in chll/sripts/setup.tcl as 24000, which at a
  // clock frequency of 100kHz provides 240ms delay.
  // On chip we use e.g. 10MHz and therefore need a value of 2400000, which
  // is defined above as SPI_DELAY.
  ParamWrite(PARAM_SPICOUNTERPRESETL_I_ADDR,SPI_DELAY & 0x0000FFFF);
  ParamWrite(PARAM_SPICOUNTERPRESETH_I_ADDR,SPI_DELAY >> 16);
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

void app_set_periode(uint16_t periode) {
  ParamWrite(PARAM_PERIODCOUNTERPRESET_I_ADDR, periode);
}

uint16_t app_get_value() {
  return ParamRead(PARAM_SENSORVALUE_O_ADDR);
}

#ifndef SKIP_APP_ISR
#pragma vector = VECTOR_RECONFMODULEIRQS_S_0
__interrupt void app_isr (void) {
  // call user's ISR function
  if (user_isr()) {
    LPM3_EXIT;
  }
}
#endif
