/*
 * testparamintf.c
 *
 *  Created on: 2013-12-21
 *      Author: hansi
 *
 */

#include <msp430f1132.h>

#define PARAM_INTF_BASE 0x0188
#define PARAM_INTF_PCA (*((volatile unsigned int*)(PARAM_INTF_BASE+0)))
#define PARAM_INTF_PDR (*((volatile unsigned int*)(PARAM_INTF_BASE+2)))

#define PCA_AUTO_INCREMENT 0x8000

int main(void) {
  int i;

  // Stop watchdog timer
  WDTCTL = WDTPW + WDTHOLD;

  // silly write cycles
  PARAM_INTF_PCA = 0x5555;
  PARAM_INTF_PCA = 0xAAAA;
  PARAM_INTF_PDR = 0x5555;
  PARAM_INTF_PDR = 0xAAAA;
  // read back
  i = PARAM_INTF_PCA;  // 0x5555
  i = PARAM_INTF_PDR;  // 0x0000 because no parameter at address 0x5555

  ///////////////////////////////////////////////////////////////////////////
  // write parameter 0x00
  PARAM_INTF_PCA = 0x0000;     // address 0, no auto-increment
  PARAM_INTF_PDR = 0xBEEF;

  ///////////////////////////////////////////////////////////////////////////
  // write parameter 0x01
  PARAM_INTF_PCA = 0x0001;     // address 1, no auto-increment
  PARAM_INTF_PDR = 0xAFFE;

  ///////////////////////////////////////////////////////////////////////////
  // write parameter 0x02
  PARAM_INTF_PCA = 0x0002;     // address 2, no auto-increment
  PARAM_INTF_PDR = 0x1234;

  ///////////////////////////////////////////////////////////////////////////
  // write parameter 0x03
  PARAM_INTF_PCA = 0x0003;     // address 3, no auto-increment
  PARAM_INTF_PDR = 0xFEDC;

  ///////////////////////////////////////////////////////////////////////////
  // read parameter 0x00
  PARAM_INTF_PCA = 0x0000;     // address 0, no auto-increment
  i = PARAM_INTF_PDR;

  ///////////////////////////////////////////////////////////////////////////
  // read parameter 0x01
  PARAM_INTF_PCA = 0x0001;     // address 1, no auto-increment
  i = PARAM_INTF_PDR;

  ///////////////////////////////////////////////////////////////////////////
  // read parameter 0x02
  PARAM_INTF_PCA = 0x0002;     // address 2, no auto-increment
  i = PARAM_INTF_PDR;

  ///////////////////////////////////////////////////////////////////////////
  // read parameter 0x03
  PARAM_INTF_PCA = 0x0003;     // address 3, no auto-increment
  i = PARAM_INTF_PDR;

  ///////////////////////////////////////////////////////////////////////////
  // write parameters with auto-increment
  PARAM_INTF_PCA = 0x0000 | PCA_AUTO_INCREMENT;   // address 0, auto-increment
  PARAM_INTF_PDR = 0x1111;
  PARAM_INTF_PDR = 0x2222;
  PARAM_INTF_PDR = 0x3333;
  PARAM_INTF_PDR = 0x4444;

  ///////////////////////////////////////////////////////////////////////////
  // read parameters with auto-increment
  PARAM_INTF_PCA = 0x0000 | PCA_AUTO_INCREMENT;   // address 0, auto-increment
  i = PARAM_INTF_PDR;
  i = PARAM_INTF_PDR;
  i = PARAM_INTF_PDR;
  i = PARAM_INTF_PDR;

  // done

  // infinite loop
  for (;;) {
  }
}
