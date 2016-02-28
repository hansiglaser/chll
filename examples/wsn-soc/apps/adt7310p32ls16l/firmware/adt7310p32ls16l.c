/**
 *
 */

#include <msp430f1232.h>

#include "cfgintf.h"
#include "paramintf.h"
#include "reconfmodule.h"
#include "adt7310p32ls16l.h"
#ifdef WRAPAPP
#  include "adt7310p32ls16l-wrapapp-driver.h"
#else
#  include "adt7310p32ls16l-driver.h"
#endif

#ifndef SIMULATION
// delay between start of conversion command and query command, must be >
// 240ms, the value is given in clock cycles
// ADT7310P32LS16L only supports 16 bit SPI counter, but we simply use 20000 for
// 2ms as used by the Matlab measurement setup
#define SPI_DELAY (20000UL)
#endif

void app_setup() {
  P3OUT = 0x00;
  init_app_adt7310p32ls16l();
#ifndef SIMULATION
  // For simulation we use a shorter value to reduce simulation time. The value
  // is given as default value in chll/sripts/setup.tcl as 24000, which at a
  // clock frequency of 100kHz provides 240ms delay.
  // On chip we use e.g. 10MHz and therefore need a value of 2400000, but that
  // is not possible with the 16 bit counter. Therefore we use 20000 to realize
  // 2ms, which is defined above as SPI_DELAY.
  ParamWrite(PARAM_SPICOUNTERPRESET_I_ADDR,SPI_DELAY);
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
  ParamWrite(PARAM_PERIODCOUNTERPRESETL_I_ADDR, periode & 0x0000FFFF);
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
