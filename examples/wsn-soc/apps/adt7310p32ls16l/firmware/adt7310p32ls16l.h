/**
 *
 */

#ifndef __ADT7310P32LS16L_H
#define __ADT7310P32LS16L_H

#include <stdint.h>
#include <stdbool.h>

void     app_setup();
void     app_start();
void     app_stop();
void     app_set_threshold(uint16_t threshold);
void     app_set_periode(uint32_t periode);
uint16_t app_get_value();

/* The user has to provide this function, which is called by the ISR */
/* Return true to exit LPM3 */
bool user_isr();

#endif // __ADT7310P32LS16L_H
