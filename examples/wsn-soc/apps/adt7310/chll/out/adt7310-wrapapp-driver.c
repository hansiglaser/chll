///////////////////////////////////////////////////////////////////////////////
// Automatically generated: app_write_firmware -wrapapp -driver_source adt7310-wrapapp-driver.c
///////////////////////////////////////////////////////////////////////////////

#include "adt7310-wrapapp-driver.h"

#include "cfgintf.h"
#include "paramintf.h"

void init_app_adt7310() {
  // No Configuration, therefore the app is already running

  // Param default values
  ParamWrite(PARAM_I2C_DIVIDER800_ADDR,                           0x0000);
  ParamWrite(PARAM_I2C_ERRACKPARAM_ADDR,                          0x0000);
  ParamWrite(PARAM_PERIODCOUNTERPRESET_I_ADDR,                    0x000A);
  ParamWrite(PARAM_SPICOUNTERPRESETH_I_ADDR,                      0x0000);
  ParamWrite(PARAM_SPICOUNTERPRESETL_I_ADDR,                      0x5DC0);
  ParamWrite(PARAM_THRESHOLD_I_ADDR,                              0x001E);
}
