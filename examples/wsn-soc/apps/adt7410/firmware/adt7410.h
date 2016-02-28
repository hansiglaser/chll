/**
 *
 */

#ifndef __ADT7310_H
#define __ADT7310_H

#include <stdint.h>

void     app_setup();
void     app_start();
void     app_stop();
void     app_set_threshold(uint16_t threshold);
void     app_set_periode(uint16_t periode);
uint16_t app_get_value();

/* The user has to provide this function, which is called by the ISR */
void user_isr();

/* The I2C master module errors are ORed to a single std_logic for the
 * (Ex.)Apps. The whole information can be requested by the CPU as parameter at
 * PARAM_I2C_ERRORS_ADDR. The individual bits code the following errors:
 */
#define I2C_ERR_READ_COUNT_ZERO   (1 << 7)
#define I2C_ERR_DEV_NOT_PRESENT   (1 << 6)
#define I2C_ERR_CORE_STOPPED      (1 << 5)
#define I2C_ERR_FIFO_EMPTY        (1 << 4)
#define I2C_ERR_CORE_BUSY         (1 << 3)
#define I2C_ERR_GOT_NACK          (1 << 2)
#define I2C_ERR_FIFO_FULL         (1 << 1)
#define I2C_ERR_BUS_COLL          (1 << 0)

uint16_t get_i2c_errors();
void     ack_i2c_error();

#endif // __ADT7310_H
