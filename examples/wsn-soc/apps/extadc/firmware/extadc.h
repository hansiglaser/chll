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

#endif // __ADT7310_H
