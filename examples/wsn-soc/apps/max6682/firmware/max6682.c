/**
 *
 */

#include <msp430f1232.h>

#include "cfgintf.h"
#include "paramintf.h"
#include "reconfmodule.h"
#include "max6682.h"
#ifdef WRAPAPP
#  include "max6682-wrapapp-driver.h"
#else
#  include "max6682-driver.h"
#endif

#ifndef SIMULATION
// delay between two consectuvie measurements
#define PERIODE (1000000UL)
#endif

void app_setup() {
  P3OUT = 0x00;
  init_app_max6682();
#ifndef SIMULATION
  app_set_periode(PERIODE);
  // the datasheet p. 6 "Serial Interface" says, that on a falling edge of CS#,
  // a current conversion is stopped immediately. On p. 2 the conversion time
  // is given as typically 64ms ans maximum of 80ms. Additionally, the
  // conversion rate is given as typically 0.5Hz, i.e. 2000ms.
  app_set_threshold(3);   // 1/8°C per LSB --> threshold of 0.5°C (due to > comparison)
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
