//////////////////////////////////////////////////////////////////////////////
// Parameterization Interface Driver
//////////////////////////////////////////////////////////////////////////////

#include "paramintf.h"

void ParamWrite(uint16_t Addr, uint16_t Value) {
  PARAM_INTF_PCA = Addr;     // set address, no auto-increment
  PARAM_INTF_PDR = Value;
}

uint16_t ParamRead(uint16_t Addr) {
  PARAM_INTF_PCA = Addr;     // set address, no auto-increment
  return PARAM_INTF_PDR;
}
