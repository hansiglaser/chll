//////////////////////////////////////////////////////////////////////////////
// SPI Master
//////////////////////////////////////////////////////////////////////////////

#include "spi.h"

void    SPI_Config(bool CPOL, bool CPHA, uint8_t BRDM, uint8_t BRDE, bool IntEn) {
  SPI_CSR = 
    (IntEn ? SPI_CSR_INTEN : 0) |
    ((BRDE & ((1 << SPI_CSR_BRDE_WIDTH)-1)) << SPI_CSR_BRDE_SHIFT) |
    ((BRDM & ((1 << SPI_CSR_BRDM_WIDTH)-1)) << SPI_CSR_BRDM_SHIFT) |
    (CPHA ? SPI_CSR_CPHA : 0) |
    (CPOL ? SPI_CSR_CPOL : 0);
}

bool    SPI_IsBusy() {
  return ((SPI_CSR & SPI_CSR_BUSY) != 0);
}
 
void    SPI_Write(uint8_t data) {
  SPI_DAT = data;
}
 
uint8_t SPI_Read() {
  return SPI_DAT;
}
