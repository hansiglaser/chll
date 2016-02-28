///////////////////////////////////////////////////////////////////////////////
// Automatically generated: app_write_firmware -reconfmodule -driver_source max6682mean-driver.c
///////////////////////////////////////////////////////////////////////////////

#include "max6682mean-driver.h"

#include "cfgintf.h"
#include "paramintf.h"

// include bitstreams
#include "max6682mean-config.inc.c"

void init_app_max6682mean() {
  // Configuration
  ConfigBegin();
  Configure(CFGREG_RECONFSIGNALS_ADDR,                 &CfgReconfSignals);
  Configure(CFGREG_BITDATA_ADDR,                       &Cfgbitdata);
  Configure(CFGREG_CFGREG_TRFSM0_0_ADDR,               &CfgCfgReg_TRFSM0_0);
  Configure(CFGREG_CFGREG_TRFSM1_0_ADDR,               &CfgCfgReg_TRFSM1_0);

  // Param default values
  ParamWrite(PARAM_I2C_DIVIDER800_ADDR,                           0x0000);
  ParamWrite(PARAM_I2C_ERRACKPARAM_ADDR,                          0x0000);
  ParamWrite(PARAM_PAUSECOUNTERPRESET_I_ADDR,                     0x0050);
  ParamWrite(PARAM_THRESHOLD_I_ADDR,                              0x0028);
  ParamWrite(PARAM_PERIODCOUNTERPRESETL_I_ADDR,                   0x03E8);
  ParamWrite(PARAM_PERIODCOUNTERPRESETH_I_ADDR,                   0x0000);
  ParamWrite(PARAM_PARAMIN_WORD_4_ADDR,                           0x0000);

  // Done with setup, release reconf.module from config mode
  ConfigEnd();
}
