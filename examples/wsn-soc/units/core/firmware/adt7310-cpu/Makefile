
# lower case! (and this comment must not be in the above line!)
RECONF_DRV_DIR = ../drivers
IHEX2VSIM = ../../tools/ihex2vsim/ihex2vsim

MCU = msp430f1232

ARCH = msp430-
CC = $(ARCH)gcc

CFLAGS = -mmcu=$(MCU) -g -Wall -O0 -I$(RECONF_DRV_DIR) -I.

LDFLAGS = -L$(RECONF_DRV_DIR)

OBJS = firmware

all: $(patsubst %,%.do,$(OBJS))

firmware.elf: main.o uart.o spi.o
	$(CC) $(CFLAGS) -o $@ $^
	$(ARCH)size $@

uart.o: $(RECONF_DRV_DIR)/uart.c
	$(CC) $(CFLAGS) -c -o $@ $^

spi.o: $(RECONF_DRV_DIR)/spi.c
	$(CC) $(CFLAGS) -c -o $@ $^

cfgintf.o: $(RECONF_DRV_DIR)/cfgintf.c
	$(CC) $(CFLAGS) -c -o $@ $^

paramintf.o: $(RECONF_DRV_DIR)/paramintf.c
	$(CC) $(CFLAGS) -c -o $@ $^

%.lst: %.elf
	$(ARCH)objdump -xdsStr $< > $@

%.hex: %.elf %.lst
	$(ARCH)objcopy -O ihex $< $@

%.do: %.hex
	$(IHEX2VSIM) $< '0xE000' '$$PMEM_REG' 16 $@

clean:
	rm -f *.o *.elf *.hex *.lst *.do *~
