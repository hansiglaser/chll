/**
 * Test data memory and then blink P1 bits 0 and 6.
 *
 * It is not possible to write to the program memory, because there is not
 * even a write signal.  Only the Debug unit can write to the program
 * memory.
 * 
 */

#include <msp430g2231.h>
#include <stdint.h>

//#define PMEM_TEST_START 0xE100
//#define PMEM_TEST_END   0xFF00
#define DMEM_TEST_START 0x0210
#define DMEM_TEST_END   0x02F0

#define LED_R 0x01
#define LED_G 0x40
#define LEDS  (LED_R | LED_G)

unsigned int i = 0;

uint16_t *ptr;
uint16_t value;

int main(void) {
  // Stop watchdog timer
  WDTCTL = WDTPW + WDTHOLD;

  // enable P1.0 (red LED) and P1.6 (green LED) as outputs
  P1DIR |= 0x01 | 0x40;
  // switch on red LED and switch off green LED
  P1OUT = (P1OUT & ~(0x01 | 0x40)) | 0x01;

  value = 0xBEEF;
  for (ptr = (uint16_t*)DMEM_TEST_START; ptr < (uint16_t*)DMEM_TEST_END; ptr++) {
    *ptr = value;
    value = (value << 5) ^ value ^ 0xAFFE;
  }
  value = 0xBEEF;
  for (ptr = (uint16_t*)DMEM_TEST_START; ptr < (uint16_t*)DMEM_TEST_END; ptr++) {
    if (*ptr != value)
      P1OUT ^= 0xFF;
    value = (value << 5) ^ value ^ 0xAFFE;
  }

  // main loop
  for (;;) {
    // toggle LEDs
    P1OUT ^= 0x01 | 0x40;
    // delay
    for(i=0; i< 20; i++);
  }
}
