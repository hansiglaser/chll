/**
 *
 */

#include <msp430f1232.h>

#include "cfgintf.h"
#include "paramintf.h"
#include "reconfmodule.h"
#include "<app>.h"
#ifdef WRAPAPP
#  include "<app>-wrapapp-driver.h"
#else
#  include "<app>-driver.h"
#endif

void app_setup() {
  // ...
  init_app_<app>();
  // ...
}

void app_start() {
  // ...
}
                 
void app_stop() {
  // ...
}
                 
void app_set_<param>(uint16_t <param>) {
  ParamWrite(PARAM_<param>_I_ADDR, <param>);
}
                 
// ...

uint16_t app_get_<param>() {
  return ParamRead(PARAM_<param>_O_ADDR);
}

#pragma vector = VECTOR_RECONFMODULEIRQS_S_<num>
__interrupt void app_isr (void) {
  // call user's ISR function
  user_isr();
}
