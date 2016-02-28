/**
 *
 */

#ifndef __<APP>_H
#define __<APP>_H

#include <stdint.h>

void     app_setup();
void     app_start();
void     app_stop();
// ...
void     app_set_<param>(uint16_t <param>);
uint16_t app_get_<param>();

/* The user has to provide this function, which is called by the ISR */
void user_isr();

#endif // __<APP>_H
