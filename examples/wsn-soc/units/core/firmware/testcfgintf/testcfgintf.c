/*
 * testcfgintf.c
 *
 *  Created on: 22.06.2013
 *      Author: hansi
 *
 */

#include <msp430f1132.h>

#define CFG_INTF_BASE 0x0180
#define CFG_INTF_CSR (*((volatile unsigned int*)(CFG_INTF_BASE+0)))
#define CFG_INTF_CBS (*((volatile unsigned int*)(CFG_INTF_BASE+2)))
#define CFG_INTF_CDR (*((volatile unsigned int*)(CFG_INTF_BASE+4)))

#define CSR_BUSY     0x8000
#define CSR_CFG_MODE 0x4000

int main(void) {
  int i;

  // Stop watchdog timer
  WDTCTL = WDTPW + WDTHOLD;

  // silly write cycles
  CFG_INTF_CSR = 0x5555;
  CFG_INTF_CSR = 0xAAAA;
  CFG_INTF_CBS = 0x5555;
  CFG_INTF_CBS = 0xAAAA;
  // read back
  i = CFG_INTF_CSR;  // '0' & '0' & "000000000000" & "10"
  i = CFG_INTF_CBS;  // 0xAAAA

  ///////////////////////////////////////////////////////////////////////////
  // test short bitstream (13 bits) with all '1's
  CFG_INTF_CSR = 0x0002 | CSR_CFG_MODE;     // CfgMode = '1', select chain number 2
  CFG_INTF_CBS = 13;
  CFG_INTF_CDR = 0x1FFF; // "000" & "1111111111111"
  while (CFG_INTF_CSR & CSR_BUSY) ;    // wait until busy bit is 0
  i = CFG_INTF_CBS;  // 0x0000

  ///////////////////////////////////////////////////////////////////////////
  // test short bitstream (13 bits) with pseudo-random data
  CFG_INTF_CSR = 0x0002 | CSR_CFG_MODE;     // CfgMode = '1', select chain number 2
  CFG_INTF_CBS = 13;
  CFG_INTF_CDR = 0x159E;     // "000" & "1 0101 1001 1110"
  while (CFG_INTF_CSR & CSR_BUSY) ;    // wait until busy bit is 0
  i = CFG_INTF_CBS;  // 0x0000
  
  ///////////////////////////////////////////////////////////////////////////
  // test long bitstream (3*16+13 bits) with pseudo-random data
  // B"1110010111100_0010101100101010_0001000101011000_0111000111101000"
  CFG_INTF_CSR = 0x0002 | CSR_CFG_MODE;     // CfgMode = '1', select chain number 2
  CFG_INTF_CBS = 3*16+13;
  CFG_INTF_CDR = 0x71E8;
  while (CFG_INTF_CSR & CSR_BUSY) ;    // wait until busy bit is 0
  i = CFG_INTF_CBS;  // 2*16+13

  CFG_INTF_CDR = 0x1158;
  while (CFG_INTF_CSR & CSR_BUSY) ;    // wait until busy bit is 0
  i = CFG_INTF_CBS;  // 1*16+13

  CFG_INTF_CDR = 0x2B2A;
  while (CFG_INTF_CSR & CSR_BUSY) ;    // wait until busy bit is 0
  i = CFG_INTF_CBS;  // 13

  CFG_INTF_CDR = 0x1CBC;
  while (CFG_INTF_CSR & CSR_BUSY) ;    // wait until busy bit is 0
  i = CFG_INTF_CBS;  // 0x0000

  // done
  CFG_INTF_CSR = 0x0000;     // CfgMode = '0', select chain number 0

  // infinite loop
  for (;;) {
  }
}
