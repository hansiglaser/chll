/**
 *
 */

#include <msp430f1232.h>

#include "cfgintf.h"
#include "paramintf.h"
#include "reconfmodule.h"
#include "blinki.h"
#ifdef WRAPAPP
#  include "blinki-wrapapp-driver.h"
#else
#  include "blinki-driver.h"
#endif

void app_setup() {
  init_app_blinki();
}

void app_set_periode(uint32_t periode) {
  ParamWrite(PARAM_PERIODH_I_ADDR, periode >> 16);
  ParamWrite(PARAM_PERIODL_I_ADDR, periode & 0x0000FFFFUL);
}
