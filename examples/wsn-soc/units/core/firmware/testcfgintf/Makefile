
MCU = msp430f1232

OBJS = testcfgintf

all: $(OBJS).do

%.elf: %.c
	msp430-gcc -mmcu=$(MCU) -g -O2 -o $@ $<

%.lst: %.elf
	msp430-objdump -xdsStr $< > $@

%.hex: %.elf %.lst
	msp430-objcopy -O ihex $< $@

%.do: %.hex
	../../tools/ihex2vsim/ihex2vsim $< '0xE000' 'sim:/core_tb/DUT/PMem_0/mem' 16 $@

clean:
	rm -f *.elf *.hex *.lst *.do *~
