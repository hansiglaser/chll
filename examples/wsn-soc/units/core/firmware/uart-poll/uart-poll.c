/*
 * uart-poll.c
 *
 * Test firmware for UART: poll for received character, transmit uppercase
 * character.
 */

#include <msp430g2231.h>
#include "../drivers/uart.h"

#undef BAUD
// BAUD = (mclk_freq/baudrate)-1 = (10MHz/115200)-1 = 85.80555
#define BAUD    86
// real baud rate: mclk_freq/(BAUD+1) = 114942.5
// error: (114942.5-115200)/115200 = -0.002235

char upcase(char ch) {
  if (ch >= 'a' && ch <= 'z') {
    ch = ch - ('a' - 'A');
  }
  return ch;
}

int main(void) {
  int ch;

  // Stop watchdog timer
  WDTCTL = WDTPW + WDTHOLD;

  // Initialize UART
  UART_BAUD = BAUD;
  UART_CTL  = UART_EN | UART_IEN_RX;

  puts("Hello World!");

  // main loop
  for (;;) {
    ch = getchar();
    ch = upcase(ch);
    putchar(ch);
  }
}
