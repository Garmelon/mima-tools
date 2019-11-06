# mima-tools

## MiMa specification

### General

In the following sections, `<a>` means "the value at the address
`a`". In the case of `<<a>>`, bits 19-0 of `<a>` are interpreted as
the address.

The MiMa uses words of 24 bits and addresses of 20 bits.

Each step, the MiMa fetches the value at the address stored in the
`IAR`, interprets it as an instruction and executes it. If the
instruction does not explicitly modify the `IAR`, the `IAR` it is
incremented by one automatically.

During execution, the following situations can be encountered where
execution should not be continued:

* The `HALT` instruction was executed
* The value at `<IAR>` cannot be decoded to a valid instruction
* The `IAR` is `0xFFFFF` and an instruction was executed that did not
  modify the `IAR`

In these cases, a MiMa emulator should stop execution and show a
suitable error message explaining why execution could not continue.

### Instructions

An instruction has one of the following forms:
```
Small opcode:
+----+ +-----------------------+
| SO | |         Value/Address |
+----+ +-----------------------+
23  20 19                      0

Large opcode:
+----+ +----+ +----------------+
|  F | | LO | |          Value |
+----+ +----+ +----------------+
23  20 19  16 15               0
```

Small opcodes can range from `0` to `E` and have an address or 20-bit
value as argument. Large opcodes can range from `F0` to `FF` and have,
if at all, a 16-bit value as argument.

### Registers

| Name  | Size (bits) | Function                     |
|-------|-------------|------------------------------|
| `IAR` |          20 | Instruction Address Register |
| `ACC` |          24 | Accumulator                  |
| `RA`  |          20 | Return Address               |
| `SP`  |          20 | Stack Pointer                |
| `FP`  |          20 | Frame Pointer                |

### Opcodes

| Opcode | Name                                        | Function                       | Notes                            |
|--------|---------------------------------------------|--------------------------------|----------------------------------|
| `0`    | `LDC c` (load constant)                     | `c -> ACC`                     | Bits 23-20 of `ACC` are set to 0 |
| `1`    | `LDV a` (load value)                        | `<a> -> ACC`                   |                                  |
| `2`    | `STV a` (store value)                       | `ACC -> <a>`                   |                                  |
| `3`    | `ADD a`                                     | `ACC + <a> -> ACC`             |                                  |
| `4`    | `AND a`                                     | `ACC and <a> -> ACC`           | Bitwise operation                |
| `5`    | `OR a`                                      | `ACC or <a> -> ACC`            | Bitwise operation                |
| `6`    | `XOR a`                                     | `ACC xor <a> -> ACC`           | Bitwise operation                |
| `7`    | `EQL a` (equal)                             | `(ACC == <a> ? -1 : 0) -> ACC` |                                  |
| `8`    | `JMP a` (jump)                              | `a -> IAR`                     |                                  |
| `9`    | `JMN a` (jump if negative)                  | `if (ACC < 0) {a -> IAR}`      |                                  |
| `A`    | `LDIV a` (load indirect value)              | `<<a>> -> ACC`                 |                                  |
| `B`    | `STIV a` (store indirect value)             | `ACC -> <<a>>`                 |                                  |
| `C`    | `CALL a`                                    | `IAR -> RA; JMP a`             |                                  |
| `D`    | `LDVR d` (load value with relative offset)  | `<SP + d> -> ACC`              |                                  |
| `E`    | `STVR d` (store value with relative offset) | `ACC -> <SP + d>`              |                                  |
| `F0`   | `HALT`                                      | Halt execution                 |                                  |
| `F1`   | `NOT`                                       | `not ACC -> ACC`               | Bitwise operation                |
| `F2`   | `RAR` (rotate ACC right)                    | `ACC >> 1 -> ACC`              | See below                        |
| `F3`   | `RET` (return)                              | `RA -> IAR`                    |                                  |
| `F4`   | `LDSP` (load from SP)                       | `SP -> ACC`                    |                                  |
| `F5`   | `STSP` (store to SP)                        | `ACC -> SP`                    |                                  |
| `F6`   | `LDFP` (load from FP)                       | `FP -> ACC`                    |                                  |
| `F7`   | `STFP` (store to FP)                        | `ACC -> FP`                    |                                  |
| `F8`   | `LDRA` (load from RA)                       | `RA -> ACC`                    |                                  |
| `F9`   | `STRA` (store to RA)                        | `ACC -> RA`                    |                                  |
| `FA`   | `ADC c` (add constant)                      | `ACC + c -> ACC`               | See below                        |

- `RAR` shifts all bits in the `ACC` right by one. The rightmost bit wraps around to the leftmost position.
- `ADC c` interprets bits 15-0 as a signed integer, whose value is then added to the `ACC`'s current value.

## File format

All tools share a common file format with extension `.mima`. It
contains the whole execution state of a MiMa, meaning the contents of
its memory and all its registers.

The file is split up into blocks of 3 bytes, which form MiMa
words. The bytes within a word are ordered from most to least
significant.

The values of registers which are only 20 bits long are stored in the
lower 20 bits of a MiMa word, and the remaining bits 23-20 are filled
with zeroes, like so:
```
+----+ +-----------------------+
|  0 | | 20-bit register value |
+----+ +-----------------------+
23  20 19                      0
```

The registers and memory are stored as follows:

| Word          | Content     |
|---------------+-------------|
|             0 | `IR`        |
|             1 | `ACC`       |
|             2 | `RA`        |
|             3 | `SP`        |
|             4 | `FP`        |
| starting at 6 | Memory dump |

The memory dump contains the words of the MiMa's memory, written in
increasing order directly one after the other with nothing
in-between. The dump always starts at address `0x00000`, but may end
before it reaches address `0xFFFFF`. When reading a dump, all
unspecified values are to be intialized as `0x000000`.

A `.mima` file must always be a multiple of 3 bytes long. It must
always be at least 15 bytes long (contains all register values).

## Programs

### `mima-run`

This program can load and run `.mima` files.

It currently does not follow the specification above.

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
