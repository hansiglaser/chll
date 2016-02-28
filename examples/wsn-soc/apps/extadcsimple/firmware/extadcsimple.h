/**
 *
 */

#ifndef __EXTADCSIMPLE_H
#define __EXTADCSIMPLE_H

#include <stdint.h>

void     app_setup();
void     app_start();
void     app_stop();
void     app_set_periode(uint16_t periode);
uint16_t app_get_value();

/* The user has to provide this function, which is called by the ISR */
void user_isr();

#endif // __EXTADCSIMPLE_H
