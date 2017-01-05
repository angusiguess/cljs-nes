(ns cljsnes.cpu)


;; Memory map for state
;; $0000-$07FF	$0800	2KB internal RAM
;; $0800-$0FFF	$0800	Mirrors of $0000-$07FF
;; $1000-$17FF	$0800
;; $1800-$1FFF	$0800
;; $2000-$2007	$0008	NES PPU registers
;; $2008-$3FFF	$1FF8	Mirrors of $2000-2007 (repeats every 8 bytes)
;; $4000-$4017	$0018	NES APU and I/O registers
;; $4018-$401F	$0008	APU and I/O functionality that is normally disabled. See CPU Test Mode.
;; $4020-$FFFF	$BFE0	Cartridge space: PRG ROM, PRG RAM, and mapper registers (See Note)

;; Registers
;; A is one byte
;; X and Y are one byte, not accumulators, mostly used for addressing
;; PC, program counter, references 65536 memory locations
;; S is stack pointer, one byte
;; P is the status register, each bit being a status flag.
;; we can represent each of the status flags if we like
;; C is carry, if last or addition or shift resulted in a carry
;; Z is zero, if last operation resulted in a zero.
;; Interrupt, can inhibit interrupts (0 IRQ and NMI, 1 just NMI)
;; D decimal, ignored
;; s used by stack copy
;; O is overflow, if ADC or SBC resulted in overflow
;; N is negative, set to bit 7 of last operation (sign bit)

;; So our state is: memory, registers, and status flags, everything
;; can operate on them.
