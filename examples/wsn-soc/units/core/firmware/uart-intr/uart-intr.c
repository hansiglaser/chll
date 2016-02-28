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

// "Hello World!\n" needs at least 13 chars
#define BUF_WIDTH 4
#define BUF_SIZE (1 << BUF_WIDTH)
#define BUF_MASK ((1 << BUF_WIDTH) - 1)

volatile char RxBuf[BUF_SIZE];
volatile char TxBuf[BUF_SIZE];
volatile uint8_t RxBufRd;
volatile uint8_t RxBufWr;
volatile uint8_t TxBufRd;
volatile uint8_t TxBufWr;

#define NEXT(Ptr) ((Ptr+1) & BUF_MASK)
#define CAN_WRITE(Buf) (NEXT(Buf ## Wr) != Buf ## Rd)
#define EMPTY(Buf) (Buf ## Wr == Buf ## Rd)

int getchar() {
  if (EMPTY(RxBuf)) {
    return -1;
  }
  char ch = RxBuf[RxBufRd];
  RxBufRd = NEXT(RxBufRd);
  return ch;
}

int putchar(int txdata) {
  if (!CAN_WRITE(TxBuf)) {
    return -1;
  }
  // append to buffer
  TxBuf[TxBufWr] = txdata;
  TxBufWr = NEXT(TxBufWr);
  // if not already sending, start sending
  if (!(UART_STAT & UART_TX_BUSY)) {
    UART_TXD = TxBuf[TxBufRd];
    TxBufRd = NEXT(TxBufRd);
  }
  return 0;
}


//--------------------------------------------------//
//        UART RX interrupt service routine         //
//         (receive a byte from the UART)           //
//--------------------------------------------------//

void __attribute((interrupt(UART_RX_VECTOR))) INT_uart_rx(void) {
  // Read the received data
  char rxdata = UART_RXD;
  // Clear the receive pending flag
  UART_STAT = UART_RX_PND;

  if (CAN_WRITE(RxBuf)) {
    RxBuf[RxBufWr] = rxdata;
    RxBufWr = NEXT(RxBufWr);
  }
  
  // Exit the low power mode
  LPM1_EXIT;
}


//--------------------------------------------------//
//        UART TX interrupt service routine         //
//         (transmit a byte to the UART)            //
//--------------------------------------------------//

void __attribute((interrupt(UART_TX_VECTOR))) INT_uart_tx(void) {
  // Clear the receive pending flag
  UART_STAT = UART_TX_PND;

  if (EMPTY(TxBuf)) {
    return;
  }
  // Write the output character
  UART_TXD = TxBuf[TxBufRd];
  TxBufRd = NEXT(TxBufRd);
}


int main(void) {
  int ch;

  // Stop watchdog timer
  WDTCTL = WDTPW + WDTHOLD;

  // Initialize UART
  UART_BAUD = BAUD;
  UART_CTL  = UART_EN | UART_IEN_RX | UART_IEN_TX;

  __eint();

  puts("Hello World!");

  // main loop
  for (;;) {
    LPM1;   // wait for a character: switch off MClk (CPU) but keep SMClk (peripherals)
    ch = getchar();
    ch = upcase(ch);
    putchar(ch);
  }
}
