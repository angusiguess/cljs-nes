(ns cljsnes.opcodes
  (:require [clojure.spec :as s]))

(s/def :opcode/code (s/int-in 0x00 0xFF))

(s/def :opcode/fn #{:adc :and :asl :bcc :bcs :beq :bit :bmi :bne :bpl :brk
                    :bvc :bvs :clc :cld :cli :clv :cmp :cpx :cpy :dec :dex
                    :dey :eor :inc :inx :iny :jmp :jsr :lda :ldx :ldy :lsr
                    :nop :ora :pha :php :pla :plp :rol :ror :rti :rts :sbc
                    :sec :sed :sei :sta :stx :sty :tax :tay :tsx :txa :txs
                    :tya})

(s/def :opcode/address-mode #{:immediate :zero :zero-x :zero-y
                              :absolute :absolute-x :absolute-y
                              :indirect :indirect-x :indirect-y
                              :relative :implied :accumulator})

(s/def :opcode/bytes-read (s/int-in 1 4))

(s/def :opcode/cycles (s/int-in 1 8))

(s/def :opcode/op (s/keys :req-un [:opcode/fn
                                   :opcode/address-mode :opcode/bytes-read
                                   :opcode/cycles]))

(s/def :opcode/ops (s/map-of :opcode/code :opcode/op))

(defn make-op
  [m code fn address-mode bytes-read cycles]
  (assert (nil? (get m code)))
  (assoc m code {:fn fn
                 :address-mode address-mode
                 :bytes-read bytes-read
                 :cycles cycles}))

(def ops (-> {}
             ;; ADC official
             (make-op 0x69 :adc :immediate 2 2)
             (make-op 0x65 :adc :zero 2 3)
             (make-op 0x75 :adc :zero-x 2 4)
             (make-op 0x6D :adc :absolute 3 4)
             (make-op 0x7D :adc :absolute-x 3 4)
             (make-op 0x79 :adc :absolute-y 3 4)
             (make-op 0x61 :adc :indirect-x 2 6)
             (make-op 0x71 :adc :indirect-y 2 5)
             ;; AND official
             (make-op 0x29 :and :immediate 2 2)
             (make-op 0x25 :and :zero 2 3)
             (make-op 0x35 :and :zero-x 2 4)
             (make-op 0x2D :and :absolute 3 4)
             (make-op 0x3D :and :absolute-x 3 4)
             (make-op 0x39 :and :absolute-y 3 4)
             (make-op 0x21 :and :indirect-x 2 6)
             (make-op 0x31 :and :indirect-y 2 5)
             ;; ASL official
             (make-op 0x0A :asl :accumulator 1 2)
             (make-op 0x06 :asl :zero 2 5)
             (make-op 0x16 :asl :zero-x 2 6)
             (make-op 0x0E :asl :absolute 3 6)
             (make-op 0x1E :asl :absolute-x 3 7)
             ;; BCC official
             (make-op 0x90 :bcc :relative 2 2)
             ;; BCS official
             (make-op 0xB0 :bcs :relative 2 2)
             ;; BEQ official
             (make-op 0xF0 :beq :relative 2 2)
             ;; BIT official
             (make-op 0x24 :bit :zero 2 3)
             (make-op 0x2C :bit :absolute 3 4)
             ;; BMI official
             (make-op 0x30 :bmi :relative 2 2)
             ;;BNE official
             (make-op 0xD0 :bne :relative 2 2)
             ;; BPL official
             (make-op 0x10 :bpl :relative 2 2)
             ;; BRK official
             (make-op 0x00 :brk :implied 1 7)
             ;; BVC official
             (make-op 0x50 :bvc :relative 2 2)
             ;; BVS official
             (make-op 0x70 :bvs :relative 2 2)
             ;; CLC official
             (make-op 0x18 :clc :implied 1 2)
             ;; CLD official
             (make-op 0xD8 :cld :implied 1 2)
             ;; CLI official
             (make-op 0x58 :cli :implied 1 2)
             ;; CLV official
             (make-op 0xB8 :clv :implied 1 2)
             ;; CMP official
             (make-op 0xC9 :cmp :immediate 2 2)
             (make-op 0xC5 :cmp :zero 2 3)
             (make-op 0xD5 :cmp :zero-x 2 4)
             (make-op 0xCD :cmp :absolute 3 4)
             (make-op 0xDD :cmp :absolute-x 3 4)
             (make-op 0xD9 :cmp :absolute-y 3 4)
             (make-op 0xC1 :cmp :indirect-x 2 6)
             (make-op 0xD1 :cmp :indirect-y 2 5)
             ;; CPX official
             (make-op 0xE0 :cpx :immediate 2 2)
             (make-op 0xE4 :cpx :zero 2 3)
             (make-op 0xEC :cpx :absolute 3 4)
             ;; CPY official
             (make-op 0xC0 :cpy :immediate 2 2)
             (make-op 0xC4 :cpy :zero 2 3)
             (make-op 0xCC :cpy :absolute 3 4)
             ;; DEC official
             (make-op 0xC6 :dec :zero 2 5)
             (make-op 0xD6 :dec :zero-x 2 6)
             (make-op 0xCE :dec :absolute 3 6)
             (make-op 0xDE :dec :absolute-x 3 7)
             ;; DEX official
             (make-op 0xCA :dex :implied 1 2)
             ;; DEY official
             (make-op 0x88 :dey :implied 1 2)
             ;; EOR official
             (make-op 0x49 :eor :immediate 2 2)
             (make-op 0x45 :eor :zero 2 3)
             (make-op 0x55 :eor :zero-x 2 4)
             (make-op 0x4D :eor :absolute 3 4)
             (make-op 0x5D :eor :absolute-x 3 4)
             (make-op 0x59 :eor :absolute-y 3 4)
             (make-op 0x41 :eor :indirect-x 2 6)
             (make-op 0x51 :eor :indirect-y 2 5)
             ;; INC official
             (make-op 0xE6 :eor :zero 2 5)
             (make-op 0xF6 :eor :zero-x 2 6)
             (make-op 0xEE :eor :absolute 3 6)
             (make-op 0xFE :eor :absolute-x 3 7)
             ;; INX official
             (make-op 0xE8 :inx :implied 1 2)
             ;; INY official
             (make-op 0xC8 :iny :implied 1 2)
             ;; JMP official
             (make-op 0x4C :jmp :absolute 3 3)
             (make-op 0x6C :jmp :indirect 3 5)
             ;; JSR official
             (make-op 0x20 :jsr :absolute 3 6)
             ;; LDA official
             (make-op 0xA9 :lda :immediate 2 2)
             (make-op 0xA5 :lda :zero 2 3)
             (make-op 0xB5 :lda :zero-x 2 4)
             (make-op 0xAD :lda :absolute 3 4)
             (make-op 0xBD :lda :absolute-x 3 4)
             (make-op 0xB9 :lda :absolute-y 3 4)
             (make-op 0xA1 :lda :indirect-x 2 6)
             (make-op 0xB1 :lda :indirect-y 2 5)
             ;; LDX official
             (make-op 0xA2 :ldx :immediate 2 2)
             (make-op 0xA6 :ldx :zero 2 3)
             (make-op 0xB6 :ldx :zero-y 2 4)
             (make-op 0xAE :ldx :absolute 3 4)
             (make-op 0xBE :ldx :absolute-y 3 4)
             ;; LDY official
             (make-op 0xA0 :ldy :immediate 2 2)
             (make-op 0xA4 :ldy :zero 2 3)
             (make-op 0xB4 :ldy :zero-x 2 4)
             (make-op 0xAC :ldy :absolute 3 4)
             (make-op 0xBC :ldy :absolute-x 3 4)
             ;; LSR official
             (make-op 0x4A :lsr :accumulator 1 2)
             (make-op 0x46 :lsr :zero 2 5)
             (make-op 0x56 :lsr :absolute 3 6)
             (make-op 0x5E :lsr :absolute-x 3 7)
             ;; NOP official
             (make-op 0xEA :nop :implied 1 2)
             ;; ORA official
             (make-op 0x09 :ora :immediate 2 2)
             (make-op 0x05 :ora :zero 2 3)
             (make-op 0x15 :ora :zero-x 2 4)
             (make-op 0x0D :ora :absolute 3 4)
             (make-op 0x1D :ora :absolute-x 3 4)
             (make-op 0x19 :ora :absolute-y 3 4)
             (make-op 0x01 :ora :indirect-x 2 6)
             (make-op 0x11 :ora :indirect-y 2 5)
             ;; PHA official
             (make-op 0x48 :pha :implied 1 3)
             ;; PHP official
             (make-op 0x08 :php :implied 1 3)
             ;; PLA official
             (make-op 0x68 :pla :implied 1 4)
             ;; PLP official
             (make-op 0x28 :plp :implied 1 4)
             ;; ROL official
             (make-op 0x2A :rol :accumulator 1 2)
             (make-op 0x26 :rol :zero 2 5)
             (make-op 0x36 :rol :zero-x 2 6)
             (make-op 0x2E :rol :absolute 3 6)
             (make-op 0x3E :rol :absolute-x 3 7)
             ;; ROR official
             (make-op 0x6A :ror :accumulator 1 2)
             (make-op 0x66 :ror :zero 2 5)
             (make-op 0x76 :ror :zero-x 2 6)
             (make-op 0x6E :ror :absolute 3 6)
             (make-op 0x7E :ror :absolute-x 3 7)
             ;; RTI official
             (make-op 0x40 :rti :implied 1 6)
             ;; RTS official
             (make-op 0x60 :rts :implied 1 6)
             ;; SBC official
             (make-op 0xE9 :sbc :immediate 2 2)
             (make-op 0xE5 :sbc :zero 2 3)
             (make-op 0xF5 :sbc :zero-x 2 4)
             (make-op 0xED :sbc :absolute 3 4)
             (make-op 0xFD :sbc :absolute-x 3 4)
             (make-op 0xF9 :sbc :absolute-y 3 4)
             (make-op 0xE1 :sbc :indirect-x 2 6)
             (make-op 0xF1 :sbc :indirect-y 2 5)
             ;; SEC official
             (make-op 0x38 :sec :implied 1 2)
             ;; SED official
             (make-op 0xF8 :sed :implied 1 2)
             ;; SEI official
             (make-op 0x78 :sei :implied 1 2)
             ;; STA official
             (make-op 0x85 :sta :zero 2 3)
             (make-op 0x95 :sta :zero-x 2 4)
             (make-op 0x8D :sta :absolute 3 4)
             (make-op 0x9D :sta :absolute-x 3 5)
             (make-op 0x99 :sta :absolute-y 3 5)
             (make-op 0x81 :sta :indirect-x 2 6)
             (make-op 0x91 :sta :indirect-y 2 6)
             ;; STX official
             (make-op 0x86 :stx :zero 2 3)
             (make-op 0x96 :stx :zero-y 2 4)
             (make-op 0x8E :stx :absolute 3 4)
             ;; STY official
             (make-op 0x84 :sty :zero 2 3)
             (make-op 0x94 :sty :zero-x 2 4)
             (make-op 0x8C :sty :absolute 3 4)
             ;; TAX official
             (make-op 0xAA :tax :implied 1 2)
             ;; TAY official
             (make-op 0xA8 :tay :implied 1 2)
             ;; TSX official
             (make-op 0xBA :tsx :implied 1 2)
             ;; TXA official
             (make-op 0x8A :txa :implied 1 2)
             ;; TXS official
             (make-op 0x9A :txs :implied 1 2)
             ;; TYA official
             (make-op 0x98 :tya :implied 1 2)))
