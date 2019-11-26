# mima-tools

A set of tools and specifications related to the MiMa
(Minimalmaschine).

* [Tools](#tools)
  * [mima-run](#mima-run)
  * [mima-asm](#mima-asm)
* [Specification](#specification)
  * [Instructions](#instructions)
  * [Registers](#registers)
  * [Opcodes](#opcodes)
  * [Extension memory flags](#extension-memory-flags)
  * [Memory dump file format: `.mima`](#memory-dump-file-format-mima)
  * [Memory flag file format: `.mima-flags`](#memory-flag-file-format-mima-flags)
  * [Symbol table file format: `.mima-symbols`](#symbol-table-file-format-mima-symbols)
* [Conventions](#conventions)

## Tools

The basic usage of these tools is as follows:

1. Create and edit an assembly file: `example.mimasm`
2. Assemble the file: `$ mima-asm example.mimasm`
3. Execute the resulting file: `$ mima-run example.mima`

For example MiMa programs, see the [examples folder](examples/).

### mima-run

This tool is a MiMa emulator. It can load and execute `.mima` files. It can also
load and use the corresponding `.mima-flags` and `.mima-symbols` files.

Basic usage: `mima-run <.mima file> [-n <steps>]`

### mima-asm

This tool is a MiMa assembler. It can parse `.mimasm` files and convert them to `.mima`
files. It can also generate the corresponding `.mima-flags` and `.mima-symbols` files.

Basic usage: `mima-asm <.mimasm file> [-o <.mima file>]`

## Specification

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

For large opcodes without an argument, the 16 value bits are
ignored. They don't have to be set to 0.

### Registers

| Name  | Size (bits) | Function                     |
|-------|-------------|------------------------------|
| `IAR` |          20 | Instruction Address Register |
| `ACC` |          24 | Accumulator                  |
| `RA`  |          20 | Return Address               |
| `SP`  |          20 | Stack Pointer                |
| `FP`  |          20 | Frame Pointer                |

### Opcodes

| Opcode | Name                            | Function                       |
|--------|---------------------------------|--------------------------------|
| `0`    | `LDC c` (load constant)         | `c -> ACC`                     |
| `1`    | `LDV a` (load value)            | `<a> -> ACC`                   |
| `2`    | `STV a` (store value)           | `ACC -> <a>`                   |
| `3`    | `ADD a`                         | `ACC + <a> -> ACC`             |
| `4`    | `AND a`                         | `ACC and <a> -> ACC`           |
| `5`    | `OR a`                          | `ACC or <a> -> ACC`            |
| `6`    | `XOR a`                         | `ACC xor <a> -> ACC`           |
| `7`    | `EQL a` (equal)                 | `(ACC == <a> ? -1 : 0) -> ACC` |
| `8`    | `JMP a` (jump)                  | `a -> IAR`                     |
| `9`    | `JMN a` (jump if negative)      | `if (ACC < 0) {a -> IAR}`      |
| `A`    | `LDIV a` (load indirect value)  | `<<a>> -> ACC`                 |
| `B`    | `STIV a` (store indirect value) | `ACC -> <<a>>`                 |
| `C`    | `CALL a`                        | `IAR -> RA; JMP a`             |
| `D`    | `ADC c` (add constant)          | `ACC + c -> ACC`               |
| `F0`   | `HALT`                          | Halt execution                 |
| `F1`   | `NOT`                           | `not ACC -> ACC`               |
| `F2`   | `RAR` (rotate ACC right)        | `ACC >> 1 -> ACC`              |
| `F3`   | `RET` (return)                  | `RA -> IAR`                    |
| `F4`   | `LDRA` (load from RA)           | `RA -> ACC`                    |
| `F5`   | `STRA` (store to RA)            | `ACC -> RA`                    |
| `F6`   | `LDSP` (load from SP)           | `SP -> ACC`                    |
| `F7`   | `STSP` (store to SP)            | `ACC -> SP`                    |
| `F8`   | `LDFP` (load from FP)           | `FP -> ACC`                    |
| `F9`   | `STFP` (store to FP)            | `ACC -> FP`                    |
| `FA`   | `LDRS o` (load relative to SP)  | `<SP + o> -> ACC`              |
| `FB`   | `STRS o` (store relative to SP) | `ACC -> <SP + o>`              |
| `FC`   | `LDRF o` (load relative to FP)  | `<FP + o> -> ACC`              |
| `FD`   | `STRF o` (store relative to FP) | `ACC -> <FP + o>`              |

* `LDC c` sets bits 23-20 of `ACC` to 0.
* `ADD a`, `AND a`, `OR a`, `XOR a` and `NOT` are bitwise operations
* `ADC c` interprets its 20-bit value as a signed integer, whose value
  is then added to the `ACC`'s current value.
* `RAR` shifts all bits in the `ACC` right by one. The rightmost bit
  wraps around to the leftmost position.
* `LDRS`, `STRS`, `LDRF` and `STRF` interpret their 16-bit value as a
  signed integer, whose value is then added to the address in the
  respective register.

## Extension memory flags

Memory flags are single characters associated with certain memory
locations and ranges. They can be used to add supplemental information
to a `.mima` file.

It is entirely up to a tool which flags it recognizes and implements,
and what each of those flags do. Unknown flags are not errors. If a
tool encounteres an unknown flag, it should ignore the flag.

The following table contains suggestions for the meanings of certain
flags, in the hope that different tool's implementations of these
flags are compatible.

| Flag | Name       | Description                                                                                                            |
|------|------------|------------------------------------------------------------------------------------------------------------------------|
| `b`  | Breakpoint | In an interactive execution environment, pause execution immediately before this instruction would have been executed. |
| `e`  | Executable | If this flag is present, only instructions at memory locations marked with this flag can be executed.                  |
| `r`  | Read-only  | Any command that would modify a memory location marked with this flag fails.                                           |

## Memory dump file format: `.mima`

All tools share a common memory dump file format with extension
`.mima`. It contains the whole execution state of a MiMa, meaning the
contents of its memory and all its registers. It also doubles as "MiMa
excutable" format. It is supplemented by the `.mima-flags` and
`.mima-symbols` file formats.

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

|          Word | Content     |
|--------------:|:------------|
|             0 | `IAR`       |
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

## Memory flag file format: `.mima-flags`

The memory flag file is a text-based file that assigns [memory
flags](#extension-memory-flags) to ranges of memory. It has the file
extension `.mima-flags`.

The format is line-based and uses LF as line endings. Other whitespace
is ignored. A line can either be empty or have the following format:

`<start address>-<end address>:<flags>`

* `<start address>` and `<end address>` are case-insensitive,
  hexadecimal, 5 digit numbers. The start and end addresses are
  inclusive. If the end address is smaller than the start address,
  their roles are swapped.
* `<flags>` are multiple characters (at least one).

The format `<address>:<flags>` is also allowed and equivalent to
`<address>-<same address>:<flags>`.

Here are some examples of valid lines:

* `12345-54321: abc`
* `00005-00004: x`
* `54d3f:y`
* `  aa5b2 - aa67c  : x    y  z  `

And here are some examples of invalid lines:

* `12g6z: abc`
* `112-115: e`
* `34321 - 22345:`
* `34321 - 22345 abc`
* `34321 22345: abc`

## Symbol table file format: `.mima-symbols`

The symbol table file contains the addresses of various labels. It can
be generated by an assembler in addition to the corresponding `.mima`
file.

The format is line-based and uses LF as line endings. Other whitespace
is ignored. A line can either be empty or have the following format:

`<address>:<label name>[ <label name>]*`

* `<address>` is a case-insensitive, hexadecimal, 5 digit number.
* `<label name>` is the name of a label. It conforms to the regex
  `[a-zA-Z][a-zA-Z0-9_-]*`.

Here are some examples of valid lines:

* `0a68c: some-label`
* `20980: label other-label third_label label_nr_4`
* ` 0a68c : label other-label `

And here are some examples of invalid lines:

* `1234: label`
* `12134:`
* `0033c label`
* `002d4: label-1, label-2, label-3`

## Conventions

In the source code, the name MiMa is spelled `Mima`. When displayed,
it is spelled `MiMa`.

Executable names are all lowercase, and words are separated by a `-`.
