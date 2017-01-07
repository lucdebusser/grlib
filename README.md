Added support for cheap FPGA board : A-C4E6 with Altera EP4CE10E22C8N
This board has NO external RAM. Only internal fpga block ram used.
The bootprom is stored in the configuration device.

Several peripherals of this board are supported as new APB devices:
- 12 LEDs
- 8 7-segment displays
- VGA :  80x30 color text display with attributes ( 1 bit/color, blinking )

Example C program(s) , makefile that demonstrates how to build using Gaisler BCC compiler and mkprom.

