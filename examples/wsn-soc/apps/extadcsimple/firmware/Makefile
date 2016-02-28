APP = extadcsimple
# lower case! (and this comment must not be in the above line!)
RECONF_DRV_DIR = ../../../units/core/firmware/drivers
APP_DRV_DIR = ../chll/out
IHEX2VSIM = ../../../units/core/tools/ihex2vsim/ihex2vsim

MCU = msp430f1232

ARCH = msp430-
CC = $(ARCH)gcc

CFLAGS = -mmcu=$(MCU) -g -Wall -O2 -I$(RECONF_DRV_DIR) -I$(APP_DRV_DIR) -I.

LDFLAGS = -L$(RECONF_DRV_DIR)

OBJS = firmware firmware-wrapapp

all: $(patsubst %,%.do,$(OBJS))

firmware.elf: main.o cfgintf.o paramintf.o $(APP).o $(APP)-driver.o
	$(CC) $(CFLAGS) -o $@ $^
	$(ARCH)size $@

firmware-wrapapp.elf: main.o cfgintf.o paramintf.o $(APP)-wrapapp.o $(APP)-wrapapp-driver.o
	$(CC) $(CFLAGS) -o $@ $^
	$(ARCH)size $@

cfgintf.o: $(RECONF_DRV_DIR)/cfgintf.c
	$(CC) $(CFLAGS) -c -o $@ $^

paramintf.o: $(RECONF_DRV_DIR)/paramintf.c
	$(CC) $(CFLAGS) -c -o $@ $^

$(APP)-wrapapp.o: $(APP).c
	$(CC) -DWRAPAPP $(CFLAGS) -c -o $@ $^

$(APP)-driver.o: $(APP_DRV_DIR)/$(APP)-driver.c
	$(CC) $(CFLAGS) -c -o $@ $^

$(APP)-wrapapp-driver.o: $(APP_DRV_DIR)/$(APP)-wrapapp-driver.c
	$(CC) $(CFLAGS) -c -o $@ $^

%.lst: %.elf
	$(ARCH)objdump -xdsStr $< > $@

%.hex: %.elf %.lst
	$(ARCH)objcopy -O ihex $< $@

%.do: %.hex
	$(IHEX2VSIM) $< '0xE000' '$$PMEM_REG' 16 $@

clean:
	rm -f *.o *.elf *.hex *.lst *.do *~ *.mem
