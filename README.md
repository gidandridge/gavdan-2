# GAVDAN-2
Assembly code and other supporting files for the GAVDAN-2 retro 6502 micro computer.

## Overview
The GAVDAN-2 is a 6502 micro computer inspired by the 1970s homebrew micro computer scene.

I wanted to design and build a small micro computer that would closely resemble the type of machine built by home enthusiasts around the mid 1970s. Using mostly era specific components and construction methods.

The micro's name GAVDAN-2 is a contraction of my own name and a suffixed model number. The current version, model 2, is built on two 9cm by 15cm PCB prototyping boards with the circuit soldered using AWG 28 wire.

The main functions of the two boards are as follows:

- Board 1 – Reset circuit, Clock, 6502 CPU, RAM and ROM
- Board 2 – 6522  VIA, 6551 ACIA

## Hardware
The GAVDAN-2 is built around the 6502 CPU, has 32KB of static RAM and a 16KB EPROM that holds the DANMON monitor ROM code and a ported copy of Lee Davison’s EhBASIC. There is a 6522 Versatile Interface Adapter to allow the connection of external peripherals and a 6522 Asynchronous Communications Interface Adapter for serial I/O.

The clock is a 1Mhz crystal oscillator.

The power and reset circuit consists of a 555 timer circuit in monostable mode. This generates a single pulse either at power on, or when the reset button is pressed. This pulse is fed to the reset line on the micro’s bus.

Address decoding is done using 74LS series TTL logic gate IC’s.

## Memory Map
The GAVDAN-2 uses a 6502 CPU capable of addressing a total of 65535 memory location each storing 8 bits of data.

The hardware is mapped into the address space in the following manner:

```
Base Address      Address mask		     Usage
(HEX)             (Binary)
-------------     ----------------     -----   
C000 to FFFF      11xxxxxxxxxxxxxx     16KB ROM
BFF0 to BFFF      101111111111xxxx     6522 VIA (16 registers)
BFEC to BFEF      10111111111011xx     6551 ACIA (4 registers)
8000 to BFEB      1xxxxxxxxxxxxxxx     Reserved for future expansion
0000 to 7FFF      0xxxxxxxxxxxxxxx     32KB RAM
```

The micro’s address decoding logic uses the address masks to determine when to enable the selected hardware. The TTL logic gates determine when each bit pattern is present and enables the relevant chip select lines on each IC.

## ROM
The ROM contains code for a basic monitor ROM named DANMON. The name is a contraction of my own surname and the word "monitor". The monitor consists of code for some basic I/O routines such as sending and receiving data via the 6551 ACIA and also provides a simple command line interface with the following commands:

```
B               - Launch Enhanced [B]asic
C               - [C]lear the screen
E <addr>        - [E]xecute code at hex address <addr>
H               - [H]elp
L <addr>        - [L]ist memory starting at hex address <addr>
M <addr>        - Set the [M]onitor prompt to hex address <addr>
R               - Monitor [R]eset
S <da>:<da>:... - [S]tore hex data <da> at the monitor prompt address
```

The ROM also contains a ported copy of Lee Davison's EhBasic (Enhanced Basic) for the 6502 CPU. The version used is 2.22p5 a patched and bug fixed version found in Klaus Dormann's GitHub repository https://github.com/Klaus2m5/6502_EhBASIC_V2.22.

Lee Davison's orignal website is no longer online but has been reproduced by Hans Otten. Hans' site contains Lee's orignal reference material and a full manual for EhBasic http://retro.hansotten.nl/6502-sbc/lee-davison-web-site/enhanced-6502-basic/

## What about the GAVDAN-1
The original GAVDAN-1 was a prototype built on a solderless breadboard and no longer exists.
