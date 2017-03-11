(ns cljsnes.cpu
  (:require [cljsnes.arith :as arith]
            [cljsnes.memory :as memory]
            [cljsnes.opcodes :as opcodes]
            [cljsnes.interrupts :as interrupts]
            [cljsnes.spec :as spec]
            [cljsnes.ppu :as ppu]
            [cljs.spec :as s]
            [cljs.core :as c]
            [cljs.pprint :as pprint]
            [cljs.reader :as reader])
  (:refer-clojure :exclude [and]))


;; Memory map for state
;; $0000-$07FF  $0800   2KB internal RAM
;; $0800-$0FFF  $0800   Mirrors of $0000-$07FF
;; $1000-$17FF  $0800
;; $1800-$1FFF  $0800
;; $2000-$2007  $0008   NES PPU registers
;; $2008-$3FFF  $1FF8   Mirrors of $2000-2007 (repeats every 8 bytes)
;; $4000-$4017  $0018   NES APU and I/O registers
;; $4018-$401F  $0008   APU and I/O functionality that is normally disabled. See CPU Test Mode.
;; $4020-$FFFF  $BFE0   Cartridge space: PRG ROM, PRG RAM, and mapper registers (See Note)

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


;; Manipulating state

(defn get-address [memory address]
  (let [lower (memory/cpu-read memory address)
        upper (memory/cpu-read memory (inc address))]
    (arith/make-address lower upper)))

;; Special Reads

(defn ppu-status-read? [{:keys [resolved-address] :as op}]
  (= resolved-address 0x2002))

(defn ppu-address-write? [{:keys [resolved-address] :as op}]
  (= resolved-address 0x2006))

(defn ppu-data-write? [{:keys [resolved-address] :as op}]
  (= resolved-address 0x2007))

(defn ppu-scroll-write? [{:keys [resolved-address] :as op}]
  (= resolved-address 0x2005))

;; Stack Manipulation



(s/fdef push-8 :args (s/cat :state ::spec/state :byte ::spec/byte)
        :ret ::spec/state)

(defn push-8 [{:keys [cpu memory] :as state} byte]
  (let [{:keys [s]} cpu]
    (-> state
        (assoc :memory (memory/cpu-write memory s byte))
        (update-in [:cpu :s] dec))))

(s/fdef pop-8 :args (s/cat :state ::spec/state)
        :ret ::spec/state)

(defn pop-8 [{:keys [cpu memory] :as state}]
  (let [{:keys [s]} cpu
        to-pop (memory/cpu-read memory (inc s))]
    [to-pop (update-in state [:cpu :s] inc)]))

(s/fdef push-16 :args (s/cat :state ::spec/state :address ::spec/address)
        :ret ::spec/state)

(defn push-16 [state address]
  (let [[low high] (arith/address->bytes address)]
    (-> state
        (push-8 high)
        (push-8 low))))

(s/fdef pop-16 :args (s/cat :state ::spec/state)
        :ret ::spec/state)

(defn pop-16 [state]
  (let [[low low-state] (pop-8 state)
        [high high-state] (pop-8 low-state)]
    [(arith/make-address low high) high-state]))

(s/fdef status->byte :args (s/cat :state ::spec/state)
        :ret ::spec/byte)

(defn status->byte [{:keys [cpu] :as state}]
  (let [{:keys [n v b d i z c]} cpu]
    (reader/read-string (str "2r" n v "0" b d i z c))))

(s/fdef byte->status :args (s/cat :byte ::spec/byte)
        :ret :cpu/status)

(defn bool->bit [bool]
  (if bool 1 0))

(defn byte->status [status]
  {:n (bool->bit (bit-test status 7))
   :v (bool->bit (bit-test status 6))
   :b (bool->bit (bit-test status 4))
   :d (bool->bit (bit-test status 3))
   :i (bool->bit (bit-test status 2))
   :z (bool->bit (bit-test status 1))
   :c (bool->bit (bit-test status 0))})

(defn get-memory [state]
  (get state :memory))

(defn set-ticks! [state ticks]
  (assoc-in state [:cpu :ticks] ticks))

(defn inc-ticks! [state]
  (update-in state [:cpu :ticks] inc))

(defn dec-ticks! [state]
  (update-in state [:cpu :ticks] dec))

(defn inc-cycles! [state]
  (update-in state [:cpu :cycles] inc))

;; Addressing

(defn page-crossed? [address offset]
  (let [mask 0x0F00]
    (not= (bit-and address mask)
          (bit-and (+ address offset) mask))))

(defn read-next [memory pc]
  (memory/cpu-read memory (inc pc)))

(defn get-sp [state]
  (get-in state [:cpu :s]))

(defn set-sp-to [state v]
  (assoc-in state [:cpu :s] v))

(defn get-pc [state]
  (get-in state [:cpu :pc]))

(defn set-pc-to [state v]
  (assoc-in state [:cpu :pc] v))

(defn get-a [state]
  (get-in state [:cpu :a]))

(defn set-a-to [state v]
  (assoc-in state [:cpu :a] v))

(defn dec-x [state]
  (update-in state [:cpu :x] dec))

(defn inc-x [state]
  (update-in state [:cpu :x] dec))

(defn get-x [state]
  (get-in state [:cpu :x]))

(defn set-x-to [state v]
  (assoc-in state [:cpu :x] v))

(defn get-y [state]
  (get-in state [:cpu :y]))

(defn dec-y [state]
  (update-in state [:cpu :y] dec))

(defn set-y-to [state v]
  (assoc-in state [:cpu :y] v))

(defn inc-y [state]
  (update-in state [:cpu :y] inc))

(defn get-interrupt [state]
  (get-in state [:cpu :i]))

(defn set-interrupt-to [state v]
  (assoc-in state [:cpu :i] v))

(defn set-interrupt [state]
  (set-interrupt-to state 1))

(defn clear-interrupt [state]
  (set-interrupt-to state 0))

(defn get-overflow [state]
  (get-in state [:cpu :v]))

(defn set-overflow-to [state v]
  (assoc-in state [:cpu :v] v))

(defn set-overflow [state]
  (set-overflow-to state 1))

(defn clear-overflow [state]
  (set-overflow-to state 0))

(defn get-carry [state]
  (get-in state [:cpu :c]))

(defn set-carry [state]
  (assoc-in state [:cpu :c] 1))

(defn set-carry-to [state v]
  (assoc-in state [:cpu :c] v))

(defn clear-carry [state]
  (assoc-in state [:cpu :c] 0))

(defn get-negative [state]
  (get-in state [:cpu :n]))

(defn set-negative [state value]
  (let [to-set (if (arith/neg-byte? value) 1 0)]
    (assoc-in state [:cpu :n] to-set)))

(defn set-negative-to [state v]
  (assoc-in state [:cpu :n] v))

(defn get-zero [state]
  (get-in state [:cpu :z]))

(defn set-zero [state value]
  (let [to-set (if (zero? value) 1 0)]
    (assoc-in state [:cpu :z] to-set)))

(defn advance-pc [state offset]
  (update-in state [:cpu :pc] + offset))

(defn set-decimal [state]
  (assoc-in state [:cpu :d] 1))

(defn clear-decimal [state]
  (assoc-in state [:cpu :d] 0))

(defn write-memory [state address byte]
  (update state :memory memory/cpu-write address byte))

;; Address modes


(defmulti address (fn [state op] (:address-mode op)))

(defmethod address :immediate [state op]
  (let [pc (get-pc state)
        memory (get-memory state)]
    (assoc op :resolved-arg (memory/cpu-read memory (inc pc)))))

(defmethod address :zero [state op]
  (let [pc (get-pc state)
        memory (get-memory state)
        reference (memory/cpu-read memory (inc pc))
        value (memory/cpu-read memory reference)]
    (assoc op
           :resolved-arg value
           :resolved-address reference)))

(defmethod address :zero-x [state op]
  (let [pc (get-pc state)
        x (get-x state)
        memory (get-memory state)
        address (memory/cpu-read memory (inc pc))
        [sum _] (arith/add address x)]
    (assoc op :resolved-arg (memory/cpu-read memory sum)
           :resolved-address sum)))

(defmethod address :zero-y [state op]
  (let [pc (get-pc state)
        y (get-y state)
        memory (get-memory state)
        address (memory/cpu-read memory (inc pc))
        [sum _] (arith/add address y)]
    (assoc op :resolved-arg (memory/cpu-read memory sum)
           :resolved-address sum)))

(defmethod address :absolute [state op]
  (let [pc (get-pc state)
        memory (get-memory state)
        address (get-address memory (inc pc))]
    (assoc op :resolved-arg (memory/cpu-read memory address)
           :resolved-address address)))

(defmethod address :absolute-x [state op]
  (let [memory (get-memory state)
        pc (get-pc state)
        x (get-x state)
        address (get-address memory (inc pc))]
    (cond-> op
      (page-crossed? address x) (update :cycles inc)
      true (assoc :resolved-arg (memory/cpu-read memory (+ address x))
                  :resolved-address (+ address x)))))

(defmethod address :absolute-y [state op]
  (let [memory (get-memory state)
        pc (get-pc state)
        y (get-y state)
        address (get-address memory (inc pc))]
    (cond-> op
      (page-crossed? address y) (update :cycles inc)
      true (assoc :resolved-arg (memory/cpu-read memory (+ address y))
                  :resolved-address (+ address y)))))

(defmethod address :indirect [state op]
  (let [memory (get-memory state)
        pc (get-pc state)
        address (get-address memory (inc pc))
        indirect-address (get-address memory address)]
    (assoc op :resolved-arg (memory/cpu-read memory indirect-address)
           :resolved-address indirect-address)))

(defmethod address :indirect-x [state op]
  (let [memory (get-memory state)
        pc (get-pc state)
        x (get-x state)
        address (get-address memory (inc pc))
        offset-address (+ x address)
        indirect-address (memory/cpu-read memory offset-address)]
    (assoc op :resolved-arg (memory/cpu-read memory indirect-address)
           :resolved-address indirect-address)))

(defmethod address :indirect-y [state op]
  (let [memory (get-memory state)
        pc (get-pc state)
        y (get-y state)
        address (get-address memory (inc pc))
        offset-address (+ y address)
        indirect-address (memory/cpu-read memory offset-address)]
    (cond-> op
      (page-crossed? pc offset-address) (update :cycles inc)
      true (assoc :resolved-arg (memory/cpu-read memory indirect-address)
                  :resolved-address indirect-address))))

(defmethod address :relative [state op]
  (let [memory (get-memory state)
        pc (get-pc state)
        offset (memory/cpu-read memory (inc pc))]
    (assoc op :resolved-arg offset)))

(defmethod address :implied [state op]
  (assoc op :resolved-arg nil))

(defmethod address :accumulator [state op]
  (let [a (get-a state)]
    (assoc op :resolved-arg a)))

;; Opcode implementations

(defmulti exec-op (fn [state op] (:fn op)))

(defmethod exec-op :adc [state {:keys [cycles bytes-read resolved-arg] :as op}]
  (let [a (get-a state)
        [sum carry] (arith/add a resolved-arg)]
    (cond-> state
      (ppu-status-read? op) ppu/read-status
      true (set-a-to sum)
      (not= (arith/neg-byte? sum) (arith/neg-byte? a)) set-overflow
      (pos? carry) set-carry
      true (set-negative sum)
      true (set-ticks! cycles)
      true (advance-pc bytes-read))))

(defmethod exec-op :and [state {:keys [cycles bytes-read resolved-arg] :as op}]
  (let [a (get-a state)
        and-a (bit-and a resolved-arg)]
    (cond-> state
      (ppu-status-read? op) ppu/read-status
      true (set-zero and-a)
      true (set-negative and-a)
      true (set-ticks! cycles)
      true (set-a-to and-a)
      true (advance-pc bytes-read))))

(defmethod exec-op :asl [state {:keys [cycles bytes-read resolved-arg :as op]}]
  (let [a (get-a state)
        [shifted carry] (arith/asl a resolved-arg)]
    (cond-> state
      (ppu-status-read? op) ppu/read-status
      true (set-carry-to carry)
      true (set-zero shifted)
      true (set-negative shifted)
      true (set-ticks! cycles)
      true (set-a-to shifted)
      true (advance-pc bytes-read))))

(defmethod exec-op :bcc [state
                         {:keys [cycles bytes-read resolved-arg] :as op}]
  (let [c (get-carry state)
        pc (get-pc state)
        signed-arg (arith/unsigned->signed resolved-arg)]
    (cond-> state
      true (set-ticks! cycles)
      (zero? c) inc-ticks!
      (zero? c) (advance-pc signed-arg)
      true (advance-pc bytes-read)
      (page-crossed? pc signed-arg) inc-ticks!)))

(defmethod exec-op :bcs [state
                         {:keys [cycles bytes-read resolved-arg] :as op}]
  (let [c (get-carry state)
        pc (get-pc state)
        signed-arg (arith/unsigned->signed resolved-arg)]
    (cond-> state
      true (set-ticks! cycles)
      (pos? c) inc-ticks!
      (pos? c) (advance-pc signed-arg)
      true (advance-pc bytes-read)
      (page-crossed? pc signed-arg) inc-ticks!)))

(defmethod exec-op :beq [state
                         {:keys [cycles bytes-read resolved-arg] :as op}]
  (let [z (get-zero state)
        pc (get-pc state)
        signed-arg (arith/unsigned->signed resolved-arg)]
    (cond-> state
      true (set-ticks! cycles)
      (pos? z) inc-ticks!
      (pos? z) (advance-pc signed-arg)
      true (advance-pc bytes-read)
      (page-crossed? pc signed-arg) inc-ticks!)))

(defmethod exec-op :bit [state
                         {:keys [cycles bytes-read resolved-arg] :as op}]
  (let [a (get-a state)]
    (cond-> state
      true (set-ticks! cycles)
      true (set-zero (bit-and a resolved-arg))
      true (set-overflow-to (bit-test resolved-arg 6))
      true (set-negative-to (bit-test resolved-arg 7))
      true (advance-pc bytes-read))))

(defmethod exec-op :bmi [state
                         {:keys [cycles bytes-read resolved-arg] :as op}]
  (let [pc (get-pc state)
        n (get-negative state)
        signed-arg (arith/unsigned->signed resolved-arg)]
    (cond-> state
      true (set-ticks! cycles)
      (pos? n) inc-ticks!
      (pos? n) (advance-pc signed-arg)
      (c/and (pos? n) (page-crossed? pc signed-arg)) inc-ticks!
      true (advance-pc bytes-read))))

(defmethod exec-op :bne [state
                         {:keys [cycles bytes-read resolved-arg] :as op}]
  (let [z (get-zero state)
        pc (get-pc state)
        signed-arg (arith/unsigned->signed resolved-arg)]
    (cond-> state
      true (set-ticks! cycles)
      (zero? z) inc-ticks!
      (zero? z) (advance-pc signed-arg)
      true (advance-pc bytes-read)
      (c/and (zero? z) (page-crossed? pc signed-arg)) inc-ticks!)))

(defmethod exec-op :bpl [state
                         {:keys [cycles bytes-read resolved-arg] :as op}]
  (let [pc (get-pc state)
        n (get-negative state)
        signed-arg (arith/unsigned->signed resolved-arg)]
    (cond-> state
      true (set-ticks! cycles)
      (zero? n) inc-ticks!
      (zero? n) (advance-pc signed-arg)
      (c/and (zero? n) (page-crossed? pc signed-arg)) inc-ticks!
      true (advance-pc bytes-read))))

(defmethod exec-op :brk [state
                         {:keys [cycles bytes-read] :as op}]
  (let [pc (get-pc state)]
    (-> state
        (push-16 pc)
        (push-8 (status->byte state))
        (set-pc-to (get-in state [:cpu :irq]))
        (set-ticks! 7))))

(defmethod exec-op :bvc [state
                         {:keys [cycles bytes-read resolved-arg] :as op}]
  (let [pc (get-pc state)
        v (get-overflow state)
        signed-arg (arith/unsigned->signed resolved-arg)]
    (cond-> state
      true (set-ticks! cycles)
      (zero? v) (advance-pc signed-arg)
      (zero? v) inc-ticks!
      (c/and (zero? v) (page-crossed? pc signed-arg)) inc-ticks!
      (pos? v) (advance-pc bytes-read))))

(defmethod exec-op :bvs [state
                         {:keys [cycles bytes-read resolved-arg] :as op}]
  (let [pc (get-pc state)
        v (get-overflow state)
        signed-arg (arith/unsigned->signed resolved-arg)]
    (cond-> state
      true (set-ticks! cycles)
      (pos? v) (advance-pc signed-arg)
      (pos? v) inc-ticks!
      (c/and (pos? v) (page-crossed? pc signed-arg)) inc-ticks!
      (zero? v) (advance-pc bytes-read))))

(defmethod exec-op :clc [state {:keys [cycles bytes-read] :as op}]
  (-> state
      clear-carry
      (advance-pc bytes-read)
      (set-ticks! cycles)))

(defmethod exec-op :cld [state {:keys [cycles bytes-read] :as op}]
  (-> state
      clear-decimal
      (advance-pc bytes-read)
      (set-ticks! cycles)))

(defmethod exec-op :cli [state {:keys [cycles bytes-read] :as op}]
  (-> state
      clear-interrupt
      (advance-pc bytes-read)
      (set-ticks! cycles)))

(defmethod exec-op :clv [state {:keys [cycles bytes-read] :as op}]
  (-> state
      clear-overflow
      (advance-pc bytes-read)
      (set-ticks! cycles)))

(defmethod exec-op :cmp [state {:keys [cycles bytes-read resolved-arg] :as op}]
  (let [a (get-a state)
        diff (- a resolved-arg)]
    (cond-> state
      true (set-negative diff)
      true (set-zero diff)
      (neg? diff) set-carry
      true (set-ticks! cycles)
      true (advance-pc bytes-read))))

(defmethod exec-op :cpx [state {:keys [cycles bytes-read resolved-arg] :as op}]
  (let [x (get-x state)
        diff (- x resolved-arg)]
    (cond-> state
      true (set-negative diff)
      true (set-zero diff)
      (neg? diff) set-carry
      true (set-ticks! cycles)
      true (advance-pc bytes-read))))

(defmethod exec-op :cpy [state {:keys [cycles bytes-read resolved-arg] :as op}]
  (let [y (get-y state)
        diff (- y resolved-arg)]
    (cond-> state
      true (set-negative diff)
      true (set-zero diff)
      (neg? diff) set-carry
      true (set-ticks! cycles)
      true (advance-pc bytes-read))))

(defmethod exec-op :dec [state
                         {:keys [cycles bytes-read
                                 resolved-arg resolved-address] :as op}]
  (let [memory (get-memory state)
        [_ decced] (arith/dec resolved-arg)]
    (cond-> state
      true (write-memory resolved-address decced)
      true (set-zero decced)
      true (set-negative decced)
      true (set-ticks! cycles)
      true (advance-pc bytes-read))))

(defmethod exec-op :dex [state
                         {:keys [cycles bytes-read] :as op}]
  (let [x (get-x state)
        [decced _] (arith/dec x)]
    (cond-> state
      true (set-x-to decced)
      true (set-zero decced)
      true (set-negative decced)
      true (set-ticks! cycles)
      true (advance-pc bytes-read))))

(defmethod exec-op :dey [state
                         {:keys [cycles bytes-read] :as op}]
  (let [y (get-y state)
        [decced _] (arith/dec y)]
    (cond-> state
      true (set-y-to decced)
      true (set-zero (dec y))
      true (set-negative (dec y))
      true (set-ticks! cycles)
      true (advance-pc bytes-read))))

(defmethod exec-op :eor [state
                         {:keys [cycles bytes-read resolved-arg] :as op}]
  (let [a (get-a state)
        x-or (-> a
                 (bit-xor resolved-arg)
                 (bit-and 0xFF))]
    (cond-> state
      true (set-zero x-or)
      true (set-negative x-or)
      true (set-ticks! cycles)
      true (advance-pc bytes-read))))

(defmethod exec-op :inc [state
                         {:keys [cycles bytes-read
                                 resolved-arg resolved-address] :as op}]
  (let [memory (get-memory state)
        [inced _] (arith/inc resolved-arg)]
    (cond-> state
      true (write-memory resolved-address inced)
      true (set-zero inced)
      true (set-negative inced)
      true (set-ticks! cycles)
      true (advance-pc bytes-read))))

(defmethod exec-op :inx [state
                         {:keys [cycles bytes-read] :as op}]
  (let [x (get-x state)
        [inced _] (arith/inc x)]
    (cond-> state
      true (set-x-to inced)
      true (set-zero inced)
      true (set-negative inced)
      true (set-ticks! cycles)
      true (advance-pc bytes-read))))

(defmethod exec-op :iny [state
                         {:keys [cycles bytes-read] :as op}]
  (let [y (get-y state)
        [inced _] (arith/inc y)]
    (cond-> state
      true (set-y-to inced)
      true (set-zero inced)
      true (set-negative inced)
      true (set-ticks! cycles)
      true (advance-pc bytes-read))))

(defmethod exec-op :jmp [state
                         {:keys [cycles bytes-read resolved-arg] :as op}]
  ;; fix weird paging error
  (-> state
      (set-ticks! cycles)
      (assoc :pc resolved-arg)))

(defmethod exec-op :jsr [state
                         {:keys [cycles bytes-read resolved-arg
                                 resolved-address] :as op}]
  (let [pc (get-pc state)
        return (+ pc bytes-read)
        memory (get-memory state)]
    (-> state
        (push-16 return)
        (set-pc-to resolved-address)
        (set-ticks! cycles))))

(defmethod exec-op :lda [state
                         {:keys [cycles bytes-read resolved-arg] :as op}]
  (cond-> state
    (ppu-status-read? op) ppu/read-status
    true (set-a-to resolved-arg)
    true (set-zero resolved-arg)
    true (set-negative resolved-arg)
    true (set-ticks! cycles)
    true (advance-pc bytes-read)))

(defmethod exec-op :ldx [state
                         {:keys [cycles bytes-read resolved-arg] :as op}]
  (cond-> state
    (ppu-status-read? op) ppu/read-status
    true (set-x-to resolved-arg)
    true (set-zero resolved-arg)
    true (set-negative resolved-arg)
    true (set-ticks! cycles)
    true (advance-pc bytes-read)))

(defmethod exec-op :ldy [state
                         {:keys [cycles bytes-read resolved-arg] :as op}]
  (cond-> state
    (ppu-status-read? op) ppu/read-status
    true (set-y-to resolved-arg)
    true (set-zero resolved-arg)
    true (set-negative resolved-arg)
    true (set-ticks! cycles)
    true (advance-pc bytes-read)))

(defmethod exec-op :lsr [state
                         {:keys [cycles bytes-read resolved-arg
                                 resolved-address address-mode] :as op}]
  (let [memory (get-memory state)
        to-shift (if (= :accumulator address-mode) (get-a state) resolved-arg)
        [shifted carry] (arith/lsr to-shift)]
    (cond-> state
      true (set-carry-to carry)
      true (set-zero shifted)
      true (set-negative shifted)
      true (set-ticks! cycles)
      true (advance-pc bytes-read)
      (= :accumulator address-mode) (set-a-to shifted)
      (not= :accumulator address-mode) (write-memory resolved-address shifted))))

(defmethod exec-op :nop [state {:keys [cycles bytes-read]}]
  (-> state
      (set-ticks! cycles)
      (advance-pc bytes-read)))

(defmethod exec-op :ora [state {:keys [cycles bytes-read resolved-arg]}]
  (let [a (get-a state)
        ored-a (bit-or a resolved-arg)]
    (cond-> state
      true (set-zero ored-a)
      true (set-negative ored-a)
      true (set-a-to ored-a)
      true (set-ticks! cycles)
      true (advance-pc bytes-read))))

(defmethod exec-op :pha [state {:keys [cycles bytes-read]}]
  (let [a (get-a state)]
    (-> state
        (push-8 a)
        (set-ticks! cycles)
        (advance-pc bytes-read))))

(defmethod exec-op :php [state {:keys [cycles bytes-read]}]
  (-> state
      (push-8 (status->byte state))
      (set-ticks! cycles)
      (advance-pc bytes-read)))

(defmethod exec-op :pla [state {:keys [cycles bytes-read]}]
  (let [[pop popped-state] (pop-8 state)]
    (-> popped-state
        (set-a-to pop)
        (set-ticks! cycles)
        (advance-pc bytes-read))))

(defmethod exec-op :plp [state {:keys [cycles bytes-read]}]
  (let [[pop popped-state] (pop-8 state)
        flags (byte->status pop)]
    (-> popped-state
        (merge flags)
        (set-ticks! cycles)
        (advance-pc bytes-read))))

(defmethod exec-op :rol [state
                         {:keys [cycles bytes-read address-mode
                                 resolved-arg resolved-address] :as op}]
  (let [[shifted carry] (arith/asl resolved-arg)
        rotated (+ shifted (* 128 carry))
        memory (get-memory state)]
    (cond-> state
      (= :accumulator address-mode) (set-a-to rotated)
      (not= :accumulator address-mode) (write-memory resolved-address rotated)
      true (set-carry-to carry)
      true (set-zero rotated)
      true (set-negative rotated)
      true (set-ticks! cycles)
      true (advance-pc bytes-read))))

(defmethod exec-op :ror [state
                         {:keys [cycles bytes-read address-mode
                                 resolved-arg resolved-address] :as op}]
  (let [[shifted carry] (arith/lsr resolved-arg)
        rotated (+ shifted (* 128 carry))
        memory (get-memory state)]
    (cond-> state
      (= :accumulator address-mode) (set-a-to rotated)
      (not= :accumulator address-mode) (write-memory resolved-address rotated)
      true (set-carry-to carry)
      true (set-zero rotated)
      true (set-negative rotated)
      true (set-ticks! cycles)
      true (advance-pc bytes-read))))

(defmethod exec-op :rti [state {:keys [cycles bytes-read]}]
  (let [[status state] (pop-8 state)
        status-map (byte->status status)
        [pc state] (pop-16 state)]
    (-> state
        (merge status-map)
        (set-pc-to pc)
        (set-ticks! cycles))))

(defmethod exec-op :rts [state {:keys [cycles]}]
  (let [[pc state] (pop-16 state)]
    (-> state
        (set-pc-to pc)
        (set-ticks! cycles))))

(defmethod exec-op :sbc [state {:keys [cycles resolved-arg bytes-read]}]
  (let [a (get-a state)
        [diff carry] (arith/sub a resolved-arg)]
    (cond-> state
      true (set-a-to diff)
      true (set-zero diff)
      (zero? carry) (set-overflow-to 1)
      (pos? carry) (set-overflow-to 0)
      true (set-carry-to carry)
      true (set-ticks! cycles)
      true (advance-pc bytes-read))))

(defmethod exec-op :sec [state {:keys [cycles bytes-read]}]
  (-> state
      (advance-pc bytes-read)
      (set-ticks! cycles)
      set-carry))

(defmethod exec-op :sed [state {:keys [cycles bytes-read]}]
  (-> state
      (advance-pc bytes-read)
      (set-ticks! cycles)
      set-decimal))

(defmethod exec-op :sei [state {:keys [cycles bytes-read] :as op}]
  (-> state
      set-interrupt
      (set-ticks! cycles)
      (advance-pc bytes-read)))

(defmethod exec-op :sta [state
                         {:keys [cycles resolved-address bytes-read] :as op}]
  (let [memory (get-memory state)
        a (get-a state)]
    (cond-> state
      (ppu-scroll-write? op) (ppu/write-register-scroll a)
      (ppu-data-write? op) (ppu/write-register-data a)
      (ppu-address-write? op) (ppu/write-register-address a)
      true (set-ticks! cycles)
      true (advance-pc bytes-read)
      true (write-memory resolved-address a))))

(defmethod exec-op :stx [state
                         {:keys [cycles resolved-address bytes-read] :as op}]
  (let [memory (get-memory state)
        x (get-x state)]
    (cond-> state
      (ppu-scroll-write? op) (ppu/write-register-scroll x)
      (ppu-data-write? op) (ppu/write-register-data x)
      (ppu-address-write? op) (ppu/write-register-address x)
      true (set-ticks! cycles)
      true (advance-pc bytes-read)
      true (write-memory resolved-address x))))

(defmethod exec-op :sty [state
                         {:keys [cycles resolved-address bytes-read] :as op}]
  (let [memory (get-memory state)
        y (get-y state)]
    (cond-> state
      (ppu-scroll-write? op) (ppu/write-register-scroll y)
      (ppu-data-write? op) (ppu/write-register-data y)
      (ppu-address-write? op) (ppu/write-register-address y)
      true (set-ticks! cycles)
      true (advance-pc bytes-read)
      true (write-memory resolved-address y))))

(defmethod exec-op :tax [state
                         {:keys [cycles resolved-address bytes-read] :as op}]
  (let [a (get-a state)]
    (cond-> state
      true (set-x-to a)
      true (set-zero a)
      true (set-negative a)
      true (set-ticks! cycles)
      true (advance-pc bytes-read))))

(defmethod exec-op :tay [state
                         {:keys [cycles resolved-address bytes-read] :as op}]
  (let [a (get-a state)]
    (cond-> state
      true (set-y-to a)
      true (set-zero a)
      true (set-negative a)
      true (set-ticks! cycles)
      true (advance-pc bytes-read))))

(defmethod exec-op :tsx [state
                         {:keys [cycles resolved-address bytes-read] :as op}]
  (let [sp (get-sp state)]
    (cond-> state
      true (set-x-to sp)
      true (set-zero sp)
      true (set-negative sp)
      true (set-ticks! cycles)
      true (advance-pc bytes-read))))

(defmethod exec-op :txa [state
                         {:keys [cycles resolved-address bytes-read] :as op}]
  (let [x (get-x state)]
    (cond-> state
      true (set-a-to x)
      true (set-zero x)
      true (set-negative x)
      true (set-ticks! cycles)
      true (advance-pc bytes-read))))

(defmethod exec-op :txs [state
                         {:keys [cycles resolved-address bytes-read] :as op}]
  (let [x (get-x state)]
    (cond-> state
      true (assoc-in [:cpu :s] x)
      true (set-zero x)
      true (set-negative x)
      true (set-ticks! cycles)
      true (advance-pc bytes-read))))

(defmethod exec-op :tya [state
                         {:keys [cycles resolved-address bytes-read] :as op}]
  (let [y (get-y state)]
    (cond-> state
      true (set-a-to y)
      true (set-zero y)
      true (set-negative y)
      true (set-ticks! cycles)
      true (advance-pc bytes-read))))

;; Interrupt handling

(defn handle-interrupt [state interrupt]
  (println "Handling interrupt: " interrupt)
  (let [vector (get-in state [:cpu interrupt])
        _ (println (pprint/cl-format nil "~x" vector))
        pc (get-pc state)
        status (status->byte state)
        interrupt-cycles 7]
    (-> state
        (push-16 pc)
        (push-8 status)
        (set-pc-to vector)
        (set-ticks! interrupt-cycles)
        (assoc :interrupt nil))))

;; Tick

(defn tick! [{:keys [cpu] :as state} instruction]
  (let [{:keys [cycles ticks]} cpu
        interrupt (interrupts/check-interrupt state)]
    (cond (< 0 ticks) (-> state
                          dec-ticks!
                          inc-cycles!)
          interrupt (do (handle-interrupt state interrupt))
          :else (do
                  (println instruction)
                  (exec-op state instruction)))))

(defn step [state]
  (let [pc (get-pc state)
        memory (get-memory state)
        op (memory/cpu-read memory pc)
        instruction (->> (get opcodes/ops op)
                         (address state))]
    (tick! state instruction)))
