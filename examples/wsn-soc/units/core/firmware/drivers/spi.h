//////////////////////////////////////////////////////////////////////////////
// SPI Master
//////////////////////////////////////////////////////////////////////////////

#ifndef __SPI_H
#define __SPI_H

#include <stdint.h>
#include <stdbool.h>

#define SPI_BASE 0x0080
#define SPI_CSR (*((volatile uint16_t*)(SPI_BASE+0)))
#define SPI_DAT (*((volatile uint16_t*)(SPI_BASE+2)))

#define SPI_CSR_BUSY        0x8000
#define SPI_CSR_INTEN       0x0400
#define SPI_CSR_BRDE_SHIFT  6
#define SPI_CSR_BRDE_WIDTH  4
#define SPI_CSR_BRDM_SHIFT  2
#define SPI_CSR_BRDM_WIDTH  4
#define SPI_CSR_CPHA        0x0002
#define SPI_CSR_CPOL        0x0001

// Vector 4 (0xFFE8)
#define SPI_VECTOR 0x0008

//   0x00 CSR Config and Status Register
//               15 (r)   Busy Bit
//            14:11       reserved
//               10 (rw)  Interrupt Enable
//             9: 6 (rw)  BRDE  (baud rate divider exponent)
//             5: 2 (rw)  BRDM  (baud rate divider mantissa)
//                1 (rw)  CPHA  (clock phase) (0: shift on falling edge of SCK (with CPOL=0))
//                0 (rw)  CPOL  (clock polarity) (0: SCK is low when idle)
//
//   0x02 DAT Data Read/Write
//            15: 8       reserved
//             7: 0 (rw)  Data
//
// SCK frequency = f / (2 * 2^BRDE * (BRDM+1))

void    SPI_Config(bool CPOL, bool CPHA, uint8_t BRDM, uint8_t BRDE, bool IntEn);
bool    SPI_IsBusy();
void    SPI_Write(uint8_t data);
uint8_t SPI_Read();

#endif // __SPI_H
