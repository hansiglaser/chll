//////////////////////////////////////////////////////////////////////////////
// Configuration Interface Driver
//////////////////////////////////////////////////////////////////////////////

#include "cfgintf.h"

void ConfigBegin() {
  CFG_INTF_CSR = CSR_CFG_MODE;     // CfgMode = '1', select chain number 0
}

void Configure(uint16_t ChainNum, const TConfigChainData* ConfigChainData) {
  uint16_t* Bitstream = (uint16_t*)ConfigChainData->Data;
  /* TConfigChainData.Data is an uint8_t[], but in this driver, 16 bit
   * words are written at onece. Since the OpenMSP430 is a little endian
   * architecture, a simple typecast to an uint16_t[] is enough.
   */

  CFG_INTF_CSR = CSR_CFG_MODE | ChainNum;     // CfgMode = '1', select chain
  CFG_INTF_CBS = ConfigChainData->Length;

  do {
    // write bitstream data
    CFG_INTF_CDR = *Bitstream;
    Bitstream++;
    while (CFG_INTF_CSR & CSR_BUSY) ;    // wait until busy bit is 0
  } while (CFG_INTF_CBS != 0);
}

void ConfigEnd() {
  CFG_INTF_CSR = 0x0000;     // CfgMode = '0', select chain number 0
}
