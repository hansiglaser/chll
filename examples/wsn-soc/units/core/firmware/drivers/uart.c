#include "uart.h"


int putchar(int txdata) {
  // Wait until the TX buffer is not full
  while (UART_STAT & UART_TX_FULL) {}

  // Write the output character
  UART_TXD = txdata;

  // testbench UART doesn't like a stop bit directly followed by a start bit, so wait a bit (count to 100, 60 is too short!)
  while (UART_STAT & UART_TX_BUSY) {}
//  int i;
//  for (i = 0; i < 100; i++) { __nop(); }
  return 0;
}

int puts(const char* s) {
  while (*s) {
    putchar(*s++);
  }
  putchar('\n');
  return 0;
}

int getchar() {
  // Wait until we received a character
  while (!(UART_STAT & UART_RX_PND)) {}
  // Get the received character
  char ch = UART_RXD;
  // Clear the receive pending flag
  UART_STAT = UART_RX_PND;
  return ch;
}
