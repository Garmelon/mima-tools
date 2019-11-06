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

### `mima-run`

This program can load and run `.mima` files.

```
$ mima-run --help
Usage: mima-run INFILE [-n|--steps N] [-d|--dump OUTFILE] [-q|--quiet]
                [-s|--sparse] [-r|--norun]

Available options:
  -h,--help                Show this help text
  INFILE                   The binary memory dump to load and execute
  -n,--steps N             How many instructions to execute (if not specified,
                           runs until HALT or execution exception)
  -d,--dump OUTFILE        If specified, write the MiMa's binary memory dump to
                           this file after execution is finished
  -q,--quiet               Don't print the memory dump
  -s,--sparse              Don't print memory locations containing only 0x000000
                           in the memory dump
  -r,--norun               Don't run the MiMa. Use the initial state for all
                           further actions
```

## Conventions

In the source code, the name MiMa is spelled `Mima`. When displayed,
it is spelled `MiMa`.

Executable names are all lowercase, and words are separated by a `-`.
