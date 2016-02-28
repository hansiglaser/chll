//////////////////////////////////////////////////////////////////////////////
// Parameterization Interface Driver
//////////////////////////////////////////////////////////////////////////////

#ifndef __PARAMINTF_H
#define __PARAMINTF_H

#include <stdint.h>

#define PARAM_INTF_BASE 0x0188
#define PARAM_INTF_PCA (*((volatile uint16_t*)(PARAM_INTF_BASE+0)))
#define PARAM_INTF_PDR (*((volatile uint16_t*)(PARAM_INTF_BASE+2)))

#define PCA_AUTO_INCREMENT 0x8000

void ParamWrite(uint16_t Addr, uint16_t Value);
uint16_t ParamRead(uint16_t Addr);

#endif // __PARAMINTF_H
