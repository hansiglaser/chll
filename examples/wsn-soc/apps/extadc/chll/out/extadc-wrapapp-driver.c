///////////////////////////////////////////////////////////////////////////////
// Automatically generated: app_write_firmware -wrapapp -driver_source extadc-wrapapp-driver.c
///////////////////////////////////////////////////////////////////////////////

#include "extadc-wrapapp-driver.h"

#include "cfgintf.h"
#include "paramintf.h"

void init_app_extadc() {
  // No Configuration, therefore the app is already running

  // Param default values
  ParamWrite(PARAM_I2C_DIVIDER800_ADDR,                           0x0000);
  ParamWrite(PARAM_I2C_ERRACKPARAM_ADDR,                          0x0000);
  ParamWrite(PARAM_PERIODCOUNTERPRESET_I_ADDR,                    0x000A);
  ParamWrite(PARAM_THRESHOLD_I_ADDR,                              0x000A);
}
