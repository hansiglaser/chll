///////////////////////////////////////////////////////////////////////////////
// Automatically generated: app_write_firmware -wrapapp -driver_header adt7410-wrapapp-driver.h
///////////////////////////////////////////////////////////////////////////////

#ifndef __ADT7410_WRAPAPP_DRIVER_H
#define __ADT7410_WRAPAPP_DRIVER_H

// Param Write (CPU -> ReconfModule)
#define PARAM_I2C_DIVIDER800_ADDR                           0x0000
#define PARAM_I2C_ERRACKPARAM_ADDR                          0x0001
#define PARAM_PERIODCOUNTERPRESET_I_ADDR                    0x0002
#define PARAM_THRESHOLD_I_ADDR                              0x0003
#define PARAM_WAITCOUNTERPRESET_I_ADDR                      0x0004

// Param Read (ReconfModule -> CPU)
#define PARAM_I2C_ERRORS_ADDR                               0x0000
#define PARAM_SENSORVALUE_O_ADDR                            0x0001

// Signal Mapping for Array Ports
#define ARRAY_MAP_RECONFMODULEIN_S_ENABLE_I                 0
#define ARRAY_MAP_RECONFMODULEIRQS_S_CPUINTR_O              0

void init_app_adt7410();

#endif  // __ADT7410_WRAPAPP_DRIVER_H
