///////////////////////////////////////////////////////////////////////////////
// Automatically generated: app_write_firmware -reconfmodule -driver_header adt7410-driver.h
///////////////////////////////////////////////////////////////////////////////

#ifndef __ADT7410_DRIVER_H
#define __ADT7410_DRIVER_H

// Config Chains
#define CFGREG_RECONFSIGNALS_ADDR                           0
#define CFGREG_BITDATA_ADDR                                 1
#define CFGREG_CFGREG_TRFSM0_0_ADDR                         2
#define CFGREG_CFGREG_TRFSM1_0_ADDR                         3

// Param Write (CPU -> ReconfModule)
#define PARAM_I2C_DIVIDER800_ADDR                           0x0000
#define PARAM_I2C_ERRACKPARAM_ADDR                          0x0001
#define PARAM_PARAMIN_WORD_0_ADDR                           0x0002
#define PARAM_PARAMIN_WORD_1_ADDR                           0x0003
#define PARAM_PARAMIN_WORD_2_ADDR                           0x0004
#define PARAM_PARAMIN_WORD_3_ADDR                           0x0005
#define PARAM_PARAMIN_WORD_4_ADDR                           0x0006

// Param Read (ReconfModule -> CPU)
#define PARAM_I2C_ERRORS_ADDR                               0x0000
#define PARAM_PARAMOUT_WORD_0_ADDR                          0x0001
#define PARAM_PARAMOUT_WORD_1_ADDR                          0x0002

// Param Mapping
#define PARAM_PERIODCOUNTERPRESET_I_ADDR                    PARAM_PARAMIN_WORD_0_ADDR
#define PARAM_SENSORVALUE_O_ADDR                            PARAM_PARAMOUT_WORD_0_ADDR
#define PARAM_THRESHOLD_I_ADDR                              PARAM_PARAMIN_WORD_1_ADDR
#define PARAM_WAITCOUNTERPRESET_I_ADDR                      PARAM_PARAMIN_WORD_2_ADDR

// Signal Mapping for Array Ports
#define ARRAY_MAP_RECONFMODULEIN_S_ENABLE_I                 0
#define ARRAY_MAP_RECONFMODULEIRQS_S_CPUINTR_O              0

void init_app_adt7410();

#endif  // __ADT7410_DRIVER_H
