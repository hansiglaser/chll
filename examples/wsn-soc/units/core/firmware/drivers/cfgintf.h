//////////////////////////////////////////////////////////////////////////////
// Configuration Interface Driver
//////////////////////////////////////////////////////////////////////////////

#ifndef __CFGINTF_H
#define __CFGINTF_H

#include <stdint.h>

/**
 * Config chain data, used by all config bitstream files
 */
typedef struct {
  uint32_t Length;
  uint8_t  Data[];
} TConfigChainData;
    

#define CFG_INTF_BASE 0x0180
#define CFG_INTF_CSR (*((volatile uint16_t*)(CFG_INTF_BASE+0)))
#define CFG_INTF_CBS (*((volatile uint16_t*)(CFG_INTF_BASE+2)))
#define CFG_INTF_CDR (*((volatile uint16_t*)(CFG_INTF_BASE+4)))

#define CSR_BUSY     0x8000
#define CSR_CFG_MODE 0x4000

void ConfigBegin();
void Configure(uint16_t ChainNum, const TConfigChainData* ConfigChainData);
void ConfigEnd();

#endif // __CFGINTF_H
