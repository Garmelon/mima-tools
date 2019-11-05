# mima-tools

## File format

All tools share a common file format, which basically just contains
the MiMa's initial memory state. Its file extension is `.mima`.

A MiMa operates on words of 24 bits, so the file is split up into
blocks of 3 bytes, written directly one after the other with nothing
in-between. The bytes within one 3-byte block are ordered from most
significant to least significant.

The file contains no metadata. Opcodes are the same as specified in
the lecture. The first block is at address 0. MiMa's execution starts
at address 0 (i. e. the first block).

## Programs

These programs are just ideas at this point, but will hopefully be
working one day.

### mima-run

This program can load and run `.mima` files at a specified processor
speed (given in instructions per second).

I don't know how IO is going to happen on this machine. Maybe I'll
just print a memory dump once the `HALT` instruction was encountered,
or maybe I'll use left over instructions to add a brainfuck-like print
command. Or maybe there is already a solution designed into the MiMa?

### mima-debug

This program is like `mima-run`, but also contains a live debugging
TUI.

## Conventions

In the source code, the name MiMa is spelled `Mima`. When displayed,
it is spelled `MiMa`.

Executable names are all lowercase, and words are separated by a `-`.
