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
$ ./mima-run --help
Usage: mima-run INFILE [-n|--steps N] [-d|--dump OUTFILE] [-q|--quiet]
                [-s|--sparse] [-r|--norun]

Available options:
  -h,--help                Show this help text
  INFILE                   The memory dump to load and execute
  -n,--steps N             How many instructions to execute (if not specified,
                           runs until HALT or execution exception)
  -d,--dump OUTFILE        If specified, the MiMa's memory is dumped to this
                           file after execution is finished
  -q,--quiet               Whether to print the memory after execution is
                           finished
  -s,--sparse              Whether to print memory locations that contain 0
  -r,--norun               Don't run the MiMa. Continues as if the initial state
                           was the result of running the MiMa.
```

## Conventions

In the source code, the name MiMa is spelled `Mima`. When displayed,
it is spelled `MiMa`.

Executable names are all lowercase, and words are separated by a `-`.
