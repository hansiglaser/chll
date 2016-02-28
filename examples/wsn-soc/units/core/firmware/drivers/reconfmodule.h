/**
 * Global definitions of the reconfig. module, see core.v.
 */

#ifndef __RECONFMODULE_H
#define __RECONFMODULE_H

#include <msp430f1232.h>
#include <stdint.h>

/* The input into the reconf.module ReconfModuleIn_s(7:0) is connected to the
 * output p3_dout of the OpenMSP430. Therefore it is accesses with the SFR
 * P3OUT.
 */

/* The output from the reconf.module ReconfModuleOut_s(7:0) is connected to the
 * input p3_din of the OpenMSP430. Therefore it is accessed with the SFR P3IN.
 */

/* The two SFRs P3DIR and P3SEL are not used, so these don't have to be accessed.
 */

/* The signal ReconfModuleIRQs_s(4:0) is connected as follows
 *   4: Vector 13  (0xFFFA) - unused in MSP430F1232, highest priority
 *   3: Vector 12  (0xFFF8) - unused in MSP430F1232
 *   2: Vector 11  (0xFFF6) - unused in MSP430F1232
 *   1: Vector  1  (0xFFE2) - unused in MSP430F1232
 *   0: Vector  0  (0xFFE0) - unused in MSP430F1232, lowest priority
 *
 * Implementing ISRs us done like
 *   #pragma vector = TIMER0_A1_VECTOR
 *   __interrupt void Timer0_A1 (void) {
 *     // ...
 *   }
 * where TIMER0_A1_VECTOR is the offset in the interrupt vector table at
 * 0xFFE0, see e.g. at the bottom of /usr/msp430/include/msp430f1232.h.
 *
 * Here we define constants for these vectors which can directly be used in
 * the #pragma statement.
 */
#define VECTOR_RECONFMODULEIRQS_S_0 (0x0000)
#define VECTOR_RECONFMODULEIRQS_S_1 (0x0002)
#define VECTOR_RECONFMODULEIRQS_S_2 (0x0016)
#define VECTOR_RECONFMODULEIRQS_S_3 (0x0018)
#define VECTOR_RECONFMODULEIRQS_S_4 (0x001A)

/* The auto-generated (Ex.)App. drivers have constants for the mapped interrupt
 * numbers, e.g.
 *   #define ARRAY_MAP_RECONFMODULEIRQS_S_CPUINTR_O 0
 * which can be used with the following macro e.g.
 *   #pragma vector = VECTOR_MAP(ARRAY_MAP_RECONFMODULEIRQS_S_CPUINTR_O)
 * Unfortunately, the C pre-processor is a single-pass algorithm, therefore
 * this double macro expansion doesn't work. You have to set the vector
 * manually.
 */
//#define VECTOR_MAP(n) VECTOR_RECONFMODULEIRQS_S_ ## n

#endif // __RECONFMODULE_H
