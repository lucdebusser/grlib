LEON3 Template design for A-C4E6 ( Altera Cyclone IV EP4C10)
------------------------------------------------------------

0. Introduction
---------------

The LEON3 design can be synthesized with quartus or synplify, and can
reach 50 - 70 MHz depending on configuration and synthesis
options. Use 'make quartus' or 'make quartus-synp' to run the complete
flow. To program the FPGA in batch mode, use 'make quartus-prog-fpga'
or 'make quartus-prog-fpga-ref (reference config).  On linux, you
might need to start jtagd as root to get the proper port permissions.

* System reset is mapped to Key K2
* DSU break is mapped to Key K3
* SW 1 is mapped to DSU enable
* DSU active is not mapped
* Processor error mode indicator is not mapped

The output from grmon should look something like this:

  GRMON2 LEON debug monitor v2.0.79 32-bit eval version
  
  Copyright (C) 2016 Cobham Gaisler - All rights reserved.
  For latest updates, go to http://www.gaisler.com/
  Comments or bug-reports to support@gaisler.com
  
  This eval version will expire on 13/04/2017

 JTAG chain (1): EP3C(10|5)/EP4CE(10|6) 
  GRLIB build version: 4164
  Detected frequency:  50 MHz
  
  Component                            Vendor
  LEON3 SPARC V8 Processor             Cobham Gaisler
  JTAG Debug Link                      Cobham Gaisler
  SPI Memory Controller                Cobham Gaisler
  AHB/APB Bridge                       Cobham Gaisler
  LEON3 Debug Support Unit             Cobham Gaisler
  Single-port AHB SRAM module          Cobham Gaisler
  Generic UART                         Cobham Gaisler
  Multi-processor Interrupt Ctrl.      Cobham Gaisler
  Modular Timer Unit                   Cobham Gaisler
  Unknown device                       Unknown vendor
  Unknown device                       Unknown vendor
  Unknown device                       Unknown vendor
  AHB Status Register                  Cobham Gaisler
  
  Use command 'info sys' to print a detailed report of attached cores

grmon2> info sys
  cpu0      Cobham Gaisler  LEON3 SPARC V8 Processor    
            AHB Master 0
  ahbjtag0  Cobham Gaisler  JTAG Debug Link    
            AHB Master 1
  spim0     Cobham Gaisler  SPI Memory Controller    
            AHB: FFF00200 - FFF00300
            AHB: 00000000 - 10000000
            IRQ: 10
            SPI memory device read command: 0x0b
  apbmst0   Cobham Gaisler  AHB/APB Bridge    
            AHB: 80000000 - 80100000
  dsu0      Cobham Gaisler  LEON3 Debug Support Unit    
            AHB: 90000000 - A0000000
            CPU0:  win 8, hwbp 2, V8 mul/div, lddel 1
                   stack pointer 0x40007ff0
                   icache not implemented
                   dcache not implemented
  ahbram0   Cobham Gaisler  Single-port AHB SRAM module    
            AHB: 40000000 - 40100000
            32-bit static ram: 32 kB @ 0x40000000
  uart0     Cobham Gaisler  Generic UART    
            APB: 80000100 - 80000200
            IRQ: 2
            Baudrate 38343, FIFO debug mode
  irqmp0    Cobham Gaisler  Multi-processor Interrupt Ctrl.    
            APB: 80000200 - 80000300
  gptimer0  Cobham Gaisler  Modular Timer Unit    
            APB: 80000300 - 80000400
            IRQ: 8
            16-bit scalar, 2 * 32-bit timers, divisor 50
  adev9     Unknown vendor  Unknown device    
            APB: 80000400 - 80000500
  adev10    Unknown vendor  Unknown device    
            APB: 80000500 - 80000600
  adev11    Unknown vendor  Unknown device    
            APB: 80000600 - 80000700
  ahbstat0  Cobham Gaisler  AHB Status Register    
            APB: 80000F00 - 80001000
            IRQ: 1
  
grmon2> spim flash detect
  Got manufacturer ID 0x20 and device ID 0x2015
  Detected device: ST/Numonyx M25P16
  

  
1. RAM interface
----------------

This board has no RAM.
32 kbytes SRAM is created from the fpga internal block RAM

2. Flash memory
---------------
(todo)
Boot Flash is provided via SPIMCTRL from the board's EPCS device.

The rest of this section explains how to work with SPIMCTRL in this
design:

Typically the lower part of the EPCS device will hold the
configuration bitstream for the FPGA. The SPIMCTRL core is configured
with an offset value that will be added to the incoming AHB address
before the address is propagated to the EPCS device. The default
offset is 0x100000 (this value is set via xconfig and the constant is
called CFG_SPIMCTRL_OFFSET). When the processor starts after power-up
it will read address 0x0, this will be translated by SPIMCTRL to
0x100000. (See section 8 below for how to program the FPGA configuration
bitstream into the EPCS device).

SPIMCTRL can only add this offset to accesses made via the core's
memory area. For accesses made via the register interface the offset
must be taken into account. This means that if we want to program the
Flash with an application which is linked to address 0x0 (our typical
bootloader) then we need to add the offset 0x100000 before programming
the file with GRMON. We load the Flash with our application starting
at 0x100000 and SPIMCTRL will then translate accesses from AMBA address
0x0 + n to Flash address 0x100000 + n.

The example below shows how to program prom.srec, which should be
present at AMBA address 0x0 to a SPI Flash device where SPIMCTRL adds
an offset to the incoming address.

SPIMCTRL will add 0x100000 to the AMBA address. We now create a
S-REC file where we add this offset to our data. There are several
tools that allow us to do this (including search and replace in a text
editor) here we use srec_cat to create prom_off.srec:

user@host:~/grlib/designs/leon3-terasic-de0-nano$ srec_cat prom.srec -offset 0x50000 -o prom_off.srec
user@host:~/grlib/designs/leon3-terasic-de0-nano$ srec_info prom.srec 
Format: Motorola S-Record
Header: "prom.srec"
Execution Start Address: 00000000
Data:   0000 - 022F
user@host:~/grlib/designs/leon3-terasic-de0-nano$ srec_info prom_off.srec 
Format: Motorola S-Record
Header: "prom.srec"
Execution Start Address: 00050000
Data:   050000 - 05022F
user@host:~/grlib/designs/leon3-terasic-de0-nano$ 

We then use GRMON2 to load the S-REC. First we check that our Flash device does not
contain data at (AMBA) address 0x0:

grmon2> mem 0
  0x00000000  ffffffff  ffffffff  ffffffff  ffffffff    ................
  0x00000010  ffffffff  ffffffff  ffffffff  ffffffff    ................
  0x00000020  ffffffff  ffffffff  ffffffff  ffffffff    ................
  0x00000030  ffffffff  ffffffff  ffffffff  ffffffff    ................

If all data is not 0xff at this address then either there is
configuration data at the specified offset (and the design can be
synthesized with a larger offset), or the Flash has previously been
programmed with application data.

If the Flash is erased (all 0xFF) we can proceed with loading our S-REC:

grmon2> spim flash detect
  Got manufacturer ID 0x20 and device ID 0x2017
  Detected device: ST/Numonyx M25P64
  
grmon2> spim flash load prom_off.srec
  00050000 prom.srec                  1.4kB /   1.4kB   [===============>] 100%
  Total size: 560B (1.52kbit/s)
  Entry point 0x50000
  Image /home/user/grlib/designs/leon3-terasic-de0-nano/prom_off.srec loaded

The data has been loaded and is available at address 0x0:

grmon2> mem 0
  0x00000000  81d82000  03000004  821060e0  81884000    .. .......`...@.
  0x00000010  81900000  81980000  81800000  a1800000    ................
  0x00000020  01000000  03002040  8210600f  c2a00040    ...... @..`....@
  0x00000030  84100000  01000000  01000000  01000000    ................
  
grmon2> 

The "verify" command in GRMON performs normal memory accesses. Using
this command we can check that what the processor will see matches
what we have in the (unmodified) prom.srec:

grmon2> verify prom.srec
  00000000 prom.srec                  1.5kB /   1.5kB   [===============>] 100%
  Total size: 560B (10.64kbit/s)
  Entry point 0x0
  Image of /home/user/grlib/designs/leon3-terasic-de0-nano/prom.srec verified without errors
  
grmon2> 

The flash can be cleared using the GRMON command "spim flash erase".
Note that this will erase the entire Flash device, including the FPGA
configuration bitstream.

For simulation, the spi_flash simulation Model in testbench.vhd knows
about the offset and will subtract the offset value before accessing
its internal memory array. To illustrate:

1. Processor AMBA address ->  SPIMCTRL
2. SPIMCTRL creates Flash address = Amba address + CFG_SPIMCTRL_OFFSET 
    and sends to spi_flash 
3. spi_flash calculates: Memory array address = Flash address-CFG_SPIMCTRL_OFFSET = AMBA address
4. spi_flash returns data, which is prom.srec[AMBA address]

3. UART
---------------

The design has one UART, which is mapped to the RS232 connector


7. Other functions
---------------


8. Programming the EPCS device
-------------------------------
(todo)
For instructions on programming the serial configuration device refer
to the DE0 user manual (convert leon3mp.sof to a .jic file, targeting
EPCS16, enable compression for the SOF, and write the generated file
to the EPCS device).

