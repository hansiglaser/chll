# Makefile
#
# 2014-12-04: Johann Glaser
#
# There is a special trick to compile multiple firmwares.
#  - If "make" is executed with a given target (e.g. "clean"), the target is
#    executed right away.
#  - If no target or "all" is given, but the variable "APP" is not set (i.e.
#    default behavior), then we define an "all" target which executes "make"
#    for all possible APP values.
#  - If no target or "all" is given, and the variable "APP" is set, then we
#    define an "all" target, which simply builds that one specific firmware
#    version.
#
# Some of the sources depend on the current "APP" value, therefore a dedicated
# .o file has to be compiled. To accomplish this, the SOURCES variable uses
# the specially named .o files instead of the .c files. Additional rules to
# generate these files are used. Unfortunately I didn't find how to use the
# Make implicit rules, therefore the commands are given explicitly.
#
# If gcc complains about multiple definitions of "app_isr" and an undefined
# reference to "user_isr", then it references the .o file from the (Ex.)App.
# firmware directory. Change directory to
# ../../../../apps/$(APP)/firmware/ and "make clean".

ifndef APP
all:
	make APP=loop      # stay active, busy waiting
	make APP=lpm1      # LPM0 = LPM1: just turns off MClk (CPU, RAMs)
	make APP=lpm3      # LPM2 = LPM3 = LPM4: switch off SMClk (GPIO, TimerA, UART, SimpleSPI)
else
all: donothing-$(APP).do
endif

PLATFORM = testchip2_cpu
# lower case! (and this comment must not be in the above line!)
PLATFORM_UC = $(shell echo $(PLATFORM) | tr '[:lower:]' '[:upper:]')

# The sources have to include driver .h files, therefore the C preprocessor
# needs to know the currently compiled APP. Since it is not possible to do
# string comparisons (e.g. #if APP == "adt7310"), we have to make a dedicated
# #define for each APP.
APP_UC = $(shell echo $(APP) | tr '[:lower:]' '[:upper:]')
APPDEF = APP_$(APP_UC)

CHIP_DIR = ../../../..
IHEX2VSIM = $(CHIP_DIR)/units/core/tools/ihex2vsim/ihex2vsim

MCU = msp430f1232

ARCH = msp430-
CC = $(ARCH)gcc

# don't delete *.lst, of which Make thinks its an intermediate file
.PRECIOUS: %.lst %.hex %.elf

.PHONY: all clean

CFLAGS = -mmcu=$(MCU) -g -Wall -O2 -D$(APPDEF)

LDFLAGS = -L$(RECONF_DRV_DIR)

SOURCES = donothing-$(APP).o

OBJS = $(patsubst %.c,%.o,$(SOURCES))

donothing-$(APP).o: donothing.c
	$(CC) -c $(CPPFLAGS) $(CFLAGS) -o $@ $^

donothing-$(APP).elf: $(OBJS)
	$(CC) $(CFLAGS) -o $@ $^
	$(ARCH)objdump -xdsStr $< > $(patsubst %.elf,%.lst,$@)
	$(ARCH)size $@

%.hex: %.elf
	$(ARCH)objcopy -O ihex $< $@

%.do: %.hex
	$(IHEX2VSIM) $< '0xE000' '$$PMEM_REG' 16 $@

clean:
	rm -f *.o *.elf *.hex *.lst *.do *~
