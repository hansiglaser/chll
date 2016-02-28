/**
 *
 */

#include <msp430f1232.h>
#include <stdint.h>

#include <spi.h>

#include <uart.h>
#include <stdio.h>

#undef BAUD
// BAUD = (mclk_freq/baudrate)-1 = (10MHz/115200)-1 = 85.80555
#define BAUD    86
// real baud rate: mclk_freq/(BAUD+1) = 114942.5
// error: (114942.5-115200)/115200 = -0.002235


#define ADT7310_CS_DIR P1DIR
#define ADT7310_CS_OUT P1OUT
#define ADT7310_CS_PIN (1 << 0)

//// Polling /////////////////////////////////////////////////////////////////

void ADT7310_Setup(bool IntEn) {
  // setup CS_n pin as output
  ADT7310_CS_DIR |= ADT7310_CS_PIN;
  ADT7310_CS_OUT |= ADT7310_CS_PIN;   // low active --> deactivate
  // CPOL = 1, CPHA = 1
  SPI_Config(true, true, 2, 2, IntEn);
}

void ADT7310_SetOneShotMode() {
  // single-shot measurement mode: write to 8-bit configuration
  // register (0x01): send 0x08 0x20 (one shot mode)
  ADT7310_CS_OUT &= ~ADT7310_CS_PIN;   // low active --> activate
  SPI_Write(0x08);
  while (SPI_IsBusy()) {}
  SPI_Write(0x20);
  while (SPI_IsBusy()) {}
  ADT7310_CS_OUT |= ADT7310_CS_PIN;   // low active --> deactivate
}

void delay(uint16_t count) {
  uint16_t i;
  for (i=0; i < count; i++) { 
    __nop();
  }
}

uint16_t ADT7310_QueryValue() {
  uint16_t SensorValue;
  // send read command 0x50 and two dummy bytes
  ADT7310_CS_OUT &= ~ADT7310_CS_PIN;   // low active --> activate
  SPI_Write(0x50);
  while (SPI_IsBusy()) {}
  SPI_Write(0xFF);
  while (SPI_IsBusy()) {}
  SensorValue = SPI_Read() << 8;      // read MSB
  SPI_Write(0xFF);
  while (SPI_IsBusy()) {}
  ADT7310_CS_OUT |= ADT7310_CS_PIN;   // low active --> deactivate
  SensorValue |= SPI_Read();          // read LSB
  return SensorValue;
}

//// Interrupt Driven ////////////////////////////////////////////////////////

uint8_t SPI_Data[4];   // four byte SPI data buffer
uint8_t SPI_Len;       // number of bytes to transfer
uint8_t SPI_Pos;       // index of byte to send/receive

void SPI_SendAsync(uint8_t Len) {
  ADT7310_CS_OUT &= ~ADT7310_CS_PIN;   // low active --> activate
  SPI_Write(SPI_Data[0]);
  SPI_Len = Len;
  SPI_Pos = 0;
}

#pragma vector = SPI_VECTOR
void __attribute((interrupt(SPI_VECTOR)))ISR_SPI() {
  SPI_Data[SPI_Pos] = SPI_Read();
  SPI_Pos++;
  if (SPI_Pos < SPI_Len) {
    // send one more byte
    SPI_Write(SPI_Data[SPI_Pos]);
  } else {
    ADT7310_CS_OUT |= ADT7310_CS_PIN;   // low active --> deactivate
    // Exit the low power mode
    LPM1_EXIT;
  }
}

void ADT7310_SetOneShotMode_Async() {
  SPI_Data[0] = 0x08;
  SPI_Data[1] = 0x20;
  SPI_SendAsync(2);
}

void ADT7310_QueryValue_Async() {
  SPI_Data[0] = 0x50;
  SPI_Data[1] = 0xFF;
  SPI_Data[2] = 0xFF;
  SPI_SendAsync(3);
}

uint16_t ADT7310_GetValue_Async() {
  return (SPI_Data[1] << 8) | SPI_Data[2];
}

uint16_t SleepCounter;

// Timer A0 interrupt service routine, executed every millisecond
#pragma vector=TIMERA0_VECTOR
__interrupt void Timer_A (void) {
  SleepCounter--;
  if (SleepCounter == 0) {
    // Exit the low power mode
    LPM1_EXIT;
  }
}

void sleep(uint16_t ms) {
  // setup timer
  TACCR0 = 9999;           // divide clock frequency of 10MHz down to milliseconds
  // using ACLK doesn't work for FPGA style OpenMSP430, because the "aclk_en"
  // signal depends on lfxt_clk_en, which is a synchronized and edge-detected
  // version of the LFXTClk_i input, which we currently don't use
  TACTL = TASSEL_2 + TACLR + MC_1; // Set the timer A to SMCLK, count up from 0 to CCR0
  CCTL0 = CCIE;            // Capture/compare interrupt enable
  SleepCounter = ms;
  // wait until ISR wakes us up
  LPM1;   // keep SMClk running for TimerA
  TACTL = TASSEL_0 + TACLR + MC_0; // Set the timer A to TAxCLK input, stop
}

// use global value so that it can be easier found in DMem
uint16_t SensorValue;

int main() {

  // Stop watchdog timer
  WDTCTL = WDTPW + WDTHOLD;

  ADT7310_Setup(false);

  ADT7310_SetOneShotMode();

  // wait for at least 240ms
  delay(100);

  SensorValue = ADT7310_QueryValue();

  // Initialize UART
  UART_BAUD = BAUD;
  UART_CTL  = UART_EN | UART_IEN_RX;

  ADT7310_Setup(true);

  __eint();

  sleep(5);

  // infinite loop
  while (1) {
    ADT7310_SetOneShotMode_Async();
    LPM1;   // keep SMClk running for SimpleSPI
    // wait for at least 240ms
    sleep(240);
    ADT7310_QueryValue_Async();
    LPM1;   // keep SMClk running for TimerA
    SensorValue = ADT7310_GetValue_Async();       // SensorValue = temperature * 128
    SensorValue = (SensorValue * 1000UL) >> 7;    // SensorValue = temperature * 1000
    printf("New value: %d.%03d\r\n",SensorValue/1000,SensorValue%1000);
    sleep(1760);
  }

  return 0;
}
