/**
 * Test program which does nothing.
 *
 * Use the #define (via compiler argument '-D') APP_* to specify how this
 * program does nothing:
 *   APP_LOOP
 *   APP_LPM1
 *   APP_LPM3
 */

#include <msp430f1232.h>

int main(void) {
  // Stop watchdog timer (although none is included in the chip :-) )
  WDTCTL = WDTPW + WDTHOLD;

  // loop also around LPMx to be save
  for (;;) {
#if defined(APP_LOOP)
    // do nothing
#elif defined(APP_LPM1)
    LPM1;
#elif defined(APP_LPM3)
    LPM3;
#else
#  error "Invalid APP_* defined"
#endif
  }
}
