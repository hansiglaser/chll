
#include <msp430f1232.h>

#define LED_R 0x01
#define LED_G 0x40
#define LEDS  (LED_R | LED_G)

#define INTR_PIN 0x08    // P1.3


// port interrupt service routine
#pragma vector=PORT1_VECTOR
__interrupt void GPIO_ISR (void) {

  // clear interrupt flag
  P1IFG &= ~INTR_PIN;
  // toggle LEDs
  P1OUT ^= 0x01 | 0x40;
}

unsigned int i = 0;

int main(void) {
  // Stop watchdog timer
  WDTCTL = WDTPW + WDTHOLD;

  // enable P1.0 (red LED) and P1.6 (green LED) as outputs
  P1DIR |= 0x01 | 0x40;
  // switch on red LED and switch off green LED
  P1OUT = (P1OUT & ~(0x01 | 0x40)) | 0x01;

  // setup and enable GPIO interrupt
  P1IES &= ~INTR_PIN;  // interrupt on rising transition
  P1IE  |= INTR_PIN;   // enable interrupt

  // enable interrupts
  __eint();

  // main loop
  for (;;) {
    // toggle LEDs
    P1OUT ^= 0x01 | 0x40;
    // delay
    for(i=0; i< 20; i++);
  }
}
