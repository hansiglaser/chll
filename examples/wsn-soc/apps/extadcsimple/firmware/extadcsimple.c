/**
 *
 */

#include <msp430f1232.h>

#include "cfgintf.h"
#include "paramintf.h"
#include "reconfmodule.h"
#include "extadcsimple.h"
#ifdef WRAPAPP
#  include "extadcsimple-wrapapp-driver.h"
#else
#  include "extadcsimple-driver.h"
#endif

void app_setup() {
  P3OUT = 0x00;
  init_app_extadcsimple();
}

void app_start() {
  P3OUT |= (1 << ARRAY_MAP_RECONFMODULEIN_S_ENABLE_I);
}

void app_stop() {
  P3OUT &= ~(1 << ARRAY_MAP_RECONFMODULEIN_S_ENABLE_I);
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
