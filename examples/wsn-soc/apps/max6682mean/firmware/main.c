/**
 *
 */

#include <msp430f1232.h>
#include <stdint.h>

#include <max6682.h>

#ifndef SIMULATION
#include <uart.h>
#include <stdio.h>

#undef BAUD
// BAUD = (mclk_freq/baudrate)-1 = (10MHz/115200)-1 = 85.80555
#define BAUD    86
// real baud rate: mclk_freq/(BAUD+1) = 114942.5
// error: (114942.5-115200)/115200 = -0.002235

void PrintSensorValue() {
  uint16_t SensorValue = app_get_value();     // Temp = SensorValue/128
  SensorValue = (SensorValue * 1000UL) >> 3;    // Temp = SensorValue * 1000
  printf("%d.%03d",SensorValue/1000,SensorValue%1000);
}
#endif   // ifndef SIMULATION

/* This function is called by the driver when an interrupt is serviced. */
void user_isr() {
#ifdef SIMULATION
  uint16_t SensorValue = app_get_value();
#else
  printf("New value: ");
  PrintSensorValue();
  printf("\r\n");
#endif
}

int main() {
  // Stop watchdog timer
  WDTCTL = WDTPW + WDTHOLD;

  app_setup();

#ifndef SIMULATION
  // Initialize UART
  UART_BAUD = BAUD;
  UART_CTL  = UART_EN | UART_IEN_RX;
#endif

  app_start();

  // Clear the timer and enable timer interrupt
  __enable_interrupt();

#ifndef SIMULATION
  puts("MAX6682 Test\r");   // adds a \n itself
  
  printf("Current value: ");
  PrintSensorValue();
  printf("\r\n");
#endif

  // infinite loop
  while (1) {
    LPM3;
  }

  return 0;
}
