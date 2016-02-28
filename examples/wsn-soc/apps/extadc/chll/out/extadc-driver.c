///////////////////////////////////////////////////////////////////////////////
// Automatically generated: app_write_firmware -reconfmodule -driver_source extadc-driver.c
///////////////////////////////////////////////////////////////////////////////

#include "extadc-driver.h"

#include "cfgintf.h"
#include "paramintf.h"

// include bitstreams
#include "extadc-config.inc.c"

void init_app_extadc() {
  // Configuration
  ConfigBegin();
  Configure(CFGREG_RECONFSIGNALS_ADDR,                 &CfgReconfSignals);
  Configure(CFGREG_BITDATA_ADDR,                       &Cfgbitdata);
  Configure(CFGREG_CFGREG_TRFSM0_0_ADDR,               &CfgCfgReg_TRFSM0_0);
  Configure(CFGREG_CFGREG_TRFSM1_0_ADDR,               &CfgCfgReg_TRFSM1_0);

  // Param default values
  ParamWrite(PARAM_I2C_DIVIDER800_ADDR,                           0x0000);
  ParamWrite(PARAM_I2C_ERRACKPARAM_ADDR,                          0x0000);
  ParamWrite(PARAM_PERIODCOUNTERPRESET_I_ADDR,                    0x000A);
  ParamWrite(PARAM_THRESHOLD_I_ADDR,                              0x000A);
  ParamWrite(PARAM_PARAMIN_WORD_2_ADDR,                           0x0000);
  ParamWrite(PARAM_PARAMIN_WORD_3_ADDR,                           0x0000);
  ParamWrite(PARAM_PARAMIN_WORD_4_ADDR,                           0x0000);

  // Done with setup, release reconf.module from config mode
  ConfigEnd();
}
