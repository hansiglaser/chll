IHEX2VSIM = ../../tools/ihex2vsim/ihex2vsim

MCU = msp430f1232

ARCH = msp430-
CC = $(ARCH)gcc

# -O0 so that the delay loop is not optimized
CFLAGS = -mmcu=$(MCU) -g -Wall -O0 -I$(RECONF_DRV_DIR) -I$(APP_DRV_DIR) -I.

LDFLAGS = -L$(RECONF_DRV_DIR)

OBJS = gpio

all: $(patsubst %,%.do,$(OBJS))

%.elf: %.c
	$(CC) $(CFLAGS) -o $@ $<

%.lst: %.elf
	$(ARCH)objdump -xdsStr $< > $@

%.hex: %.elf %.lst
	$(ARCH)objcopy -O ihex $< $@

%.do: %.hex
	$(IHEX2VSIM) $< '0xE000' '$$PMEM_REG' 16 $@

clean:
	rm -f *.o *.elf *.hex *.lst *.do *~
