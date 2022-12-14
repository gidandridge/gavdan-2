# GAVDAN-2
Assembly code and other supporting files for the GAVDAN-2 retro 6502 microcomputer.

## Overview
The GAVDAN-2 is a 6502 microcomputer inspired by the 1970s homebrew microcomputer scene.

I wanted to design and build a small microcomputer that would closely resemble the type of machines built by home enthusiasts around the mid 1970s. Using mostly era specific components and construction methods.

The microcomputer's name GAVDAN-2 is a contraction of my own name and a suffixed model number. The current version, model 2, is built on two 9cm by 15cm PCB prototyping boards with the circuit soldered using AWG 28 wire.

The main functions of the two boards are as follows:

- Board 1 (Main Board) – Reset circuit, Clock, 6502 CPU, RAM and ROM
- Board 2 (IO Board) – 6522  VIA, 6551 ACIA

## Hardware
The GAVDAN-2 is built around the 6502 CPU, has 32KB of static RAM and a 16KB EPROM that holds the DANMON monitor ROM code and a ported copy of Lee Davison’s EhBASIC. There is a 6522 Versatile Interface Adapter to allow the connection of external peripherals and a 6551 Asynchronous Communications Interface Adapter for serial I/O.

The clock is a 1Mhz crystal oscillator.

The power and reset circuit consists of a 555 timer circuit in monostable mode. This generates a single pulse either at power on, or when the reset button is pressed. This pulse is fed to the reset line on the microcomputer’s bus.

Address decoding is done using 74LS series TTL logic gate IC’s.

Board 1 (Main Board) comprises the following bill of materials:
```
Reference(s)     Type                    Value/Part
------------     ----                    ----------
1MHZ1            Oscillator              1MHZ Crystal MCO-1510A
C1               Capacitor               .1uF
C2               Capacitor               .01uF
C3               Polarized Capacitor     10uF
C4               Capacitor               .1uF
C5               Capacitor               .1uF
C6               Capacitor               .1uF
D1               LED                     Red Power LED
JP1              Bridged Jumper          Clock Jumper
R1               Resistor                1M
R2               Resistor                270ohm
R3               Resistor                3K
R4               Resistor                47K
R5               Resistor                1K
R6               Resistor                3.3M
R7               Resistor                3.3M
R8               Resistor                3.3M
R9               Resistor                3K
SW1              Switch                  SW_MEC_5G
U1               IC                      NE555P
U2               IC                      74LS04
U3               IC                      6502 CPU
U4               IC                      CY62256-70PC RAM
U5               IC                      74LS00
U6               IC                      27C128 ROM
```

Board 2 (IO Board) comprises the following bill of materials:
```
Reference(s)     Type                    Value/Part
------------     ----                    ----------
C1               Capacitor               .1uF
C2               Capacitor               33pF
C3               Capacitor               .1uF
J1               Connector               DB9 Female
R1               Resistor                1M
U1               IC			 74LS04
U2               IC                      74LS133
U3               IC                      74LS08
U4               IC                      74LS08
U5               IC                      6551 ACIA
U6               IC                      MAX233CPP
U7               IC                      6522 VIA
Y1               Oscillator              1.8432MHZ Crystal
```

## Memory Map
The GAVDAN-2 uses a 6502 CPU capable of addressing a total of 65535 memory location each storing 8 bits of data.

The hardware is mapped into the address space in the following manner:

```
Base Address      Address Mask         Usage
(HEX)             (Binary)
-------------     ----------------     -----   
C000 to FFFF      11xxxxxxxxxxxxxx     16KB ROM
BFF0 to BFFF      101111111111xxxx     6522 VIA (16 registers)
BFEC to BFEF      10111111111011xx     6551 ACIA (4 registers)
8000 to BFEB      1xxxxxxxxxxxxxxx     Reserved for future expansion
0000 to 7FFF      0xxxxxxxxxxxxxxx     32KB RAM
```

The microcomputer’s address decoding logic uses the address masks to determine when to enable the selected hardware. The TTL logic gates determine when each bit pattern is present and enables the relevant chip select lines on each IC.

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

Lee Davison's orignal website is no longer online but has been reproduced by Hans Otten. Hans' site contains Lee's orignal reference material and a full manual for EhBasic http://retro.hansotten.nl/6502-sbc/lee-davison-web-site/enhanced-6502-basic/.

## Schematics
Full circuit schematics are included. These have been produced in Kicad and make good use of the 6502 Kicad library by Nicholas Parks Young located in his GitHub repository https://github.com/Alarm-Siren/6502-kicad-library. The library provides a range of symbols for the 6502 microprocessor and associated ecosystem.

## 3D Encasement
3D printable STL files are included. These have been designed for use on a typical FDM printer using PLA. They comprise a base unit with space for the GAVDAN-2 logo indented and a separate logo file. Print each STL in differing colours then press the components of the logo into the base unit.

The base unit is designed to hold four 9cm by 15cm PCB prototyping boards in a vertical arrangement. However due to the depth of some components it may only be possible to fit two boards into the current version of this encasement. I plan to create a revised version to solve this shortcoming.

## What about the GAVDAN-1
The original GAVDAN-1 was a prototype built on a solderless breadboard and no longer exists.
