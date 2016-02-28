/*
 * blinki.c
 *
 *  Created on: 30.11.2012
 *      Author: hansi
 *
 * Debugging: Before each debug session you have to start the JTAG driver
 *   $ mspdebug rf2500 "prog Debug/blinki" gdb
 * from the project base directory
 * https://github.com/hansiglaser/prjblinkenlights/workspace/blinki
 *
 * http://wiki.eclipse.org/CDT/User/FAQ#How_do_I_debug_a_remote_application.3F
 *
 * To debug with the MSP430 LaunchPad:
 *  - ... -> Debug Configurations
 *     - C/C++ Remote Application, double-click to add a new one
 *     - at the bottom click on "Select other..."
 *        - [X] Use configuration specific settins
 *        - choose "GDB (DSF) Manual Remote Debugging Launcher"
 *        - [OK]
 *     - "Debugger" tab
 *        - GDB debugger: "msp430-gdb"
 *        - GDB command file: empty
 *        - Sub-Tab "Connection"
 *           - Port number: "2000"
 *     - [Debug]
 *
 */

#include <msp430g2231.h>

#define LED_R 0x01
#define LED_G 0x40
#define LEDS  (LED_R | LED_G)

unsigned int i = 0;

int main(void) {
  // Stop watchdog timer
  WDTCTL = WDTPW + WDTHOLD;

  // enable P1.0 (red LED) and P1.6 (green LED) as outputs
  P1DIR |= 0x01 | 0x40;
  // switch on red LED and switch off green LED
  P1OUT = (P1OUT & ~(0x01 | 0x40)) | 0x01;

  // main loop
  for (;;) {
    // toggle LEDs
    P1OUT ^= 0x01 | 0x40;
    // delay
    for(i=0; i< 20; i++);
  }
}
