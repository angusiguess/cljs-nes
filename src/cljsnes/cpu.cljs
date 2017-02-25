(ns cljsnes.cpu
  (:require [cljsnes.arith :as arith]
            [cljsnes.memory :as memory]
            [cljsnes.opcodes :as opcodes]
            [cljsnes.spec :as spec]
            [cljs.spec :as s]
            [cljs.core.async :as a]
            [cljs.core :as c]
            [cljs.pprint :as pprint]
            [cljs.reader :as reader])
  (:refer-clojure :exclude [and]))


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


;; Manipulating state

(defn get-address [memory address]
  (let [lower (memory/read memory address)
        upper (memory/read memory (inc address))]
    (arith/make-address lower upper)))

;; Stack Manipulation



(s/fdef push-8 :args (s/cat :state ::spec/state :byte ::spec/byte)
        :ret ::spec/state)

(defn push-8 [{:keys [memory cpu] :as state} byte]
  (let [{:keys [s]} cpu]
    (-> state
        (assoc :memory (memory/write memory s byte))
        (update-in [:cpu :s] dec))))

(s/fdef pop-8 :args (s/cat :state ::spec/state)
        :ret ::spec/state)

(defn pop-8 [{:keys [memory cpu] :as state}]
  (let [{:keys [s]} cpu
        to-pop (memory/read memory s)]
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
  (let [[low state] (pop-8 state)
        [high state] (pop-8 state)]
    [(arith/make-address low high) state]))

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

;; Eventually we'll want to broadcast cycle ticks for video.

(defonce cycle-clock (a/chan 1024))

(defn tick! [{:keys [cpu] :as state} ticks]
  (let [{:keys [cycles]} cpu]
    (dotimes [_ ticks]
     (a/put! cycle-clock (inc cycles)))
    (update-in state [:cpu :cycles] + ticks)))

;; Addressing

(defn page-crossed? [address offset]
  (let [mask 0x0F00]
    (not= (bit-and address mask)
          (bit-and (+ address offset) mask))))

(defn read-next [memory pc]
  (memory/read memory (inc pc)))

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

(defn set-negative [state]
  (assoc-in state [:cpu :n] 1))

(defn set-negative-to [state v]
  (assoc-in state [:cpu :n] v))

(defn get-zero [state]
  (get-in state [:cpu :z]))

(defn set-zero [state]
  (assoc-in state [:cpu :z] 1))

(defn advance-pc [state offset]
  (update-in state [:cpu :pc] + offset))

(defn set-decimal [state]
  (assoc-in state [:cpu :d] 1))

(defn clear-decimal [state]
  (assoc-in state [:cpu :d] 0))

(defmulti address (fn [state op] (:address-mode op)))

(defmethod address :immediate [state op]
  (let [memory (:memory state)
        pc (get-pc state)]
    (assoc op :resolved-arg (memory/read memory (inc pc)))))

(defmethod address :zero [state op]
  (let [memory (:memory state)
        pc (get-pc state)]
    (assoc op
           :resolved-arg (->> pc
                              (read-next memory)
                              (memory/read memory))
           :resolved-address (->> pc
                                  (read-next memory)))))

(defmethod address :zero-x [state op]
  (let [memory (:memory state)
        pc (get-pc state)
        x (get-x state)
        address (read-next memory pc)
        [sum _] (arith/add address x)]
    (assoc op :resolved-arg (memory/read memory sum)
           :resolved-address sum)))

(defmethod address :zero-y [state op]
  (let [memory (:memory state)
        pc (get-pc state)
        y (get-y state)
        address (read-next memory pc)
        [sum _] (arith/add address y)]
    (assoc op :resolved-arg (memory/read memory sum)
           :resolved-address sum)))

(defmethod address :absolute [state op]
  (let [memory (:memory state)
        pc (get-pc state)
        lower (read-next memory pc)
        upper (read-next memory (inc pc))]
    (assoc op :resolved-arg (arith/make-address lower upper)
           :resolved-address (arith/make-address lower upper))))

(defmethod address :absolute-x [state op]
  (let [memory (:memory state)
        pc (get-pc state)
        x (get-x state)
        lower (read-next memory pc)
        upper (read-next memory (inc pc))
        address (arith/make-address lower upper)]
    (cond-> op
      (page-crossed? address x) (update :cycles inc)
      true (assoc :resolved-arg (+ address x)
                  :resolved-address (+ address x)))))

(defmethod address :absolute-y [state op]
  (let [memory (:memory state)
        pc (get-pc state)
        y (get-y state)
        lower (read-next memory pc)
        upper (read-next memory (inc pc))
        address (arith/make-address lower upper)]
    (cond-> op
      (page-crossed? address y) (update :cycles inc)
      true (assoc :resolved-arg (+ address y)
                  :resolved-address (+ address y)))))

(defmethod address :indirect [state op]
  (let [memory (:memory state)
        pc (get-pc state)
        lower (read-next memory pc)
        upper (read-next memory (inc pc))
        address (arith/make-address lower upper)]
    (assoc op :resolved-arg (memory/read memory address))))

(defmethod address :indirect-x [state op]
  (let [memory (:memory state)
        pc (get-pc state)
        x (get-x state)
        base-address (read-next memory pc)
        offset-address (bit-and 0xFF
                                  (+ x base-address))]
    (assoc op :resolved-arg (memory/read memory offset-address)
           :resolved-address offset-address)))

(defmethod address :indirect-y [state op]
  (let [memory (:memory state)
        pc (get-pc state)
        y (get-y state)
        base-address (read-next memory pc)
        base-value (memory/read memory base-address)
        offset-value (+ base-value y)]
    (cond-> op
      (page-crossed? base-value y) (update :cycles inc)
      true (assoc :resolved-arg offset-value
                  :resolved-address offset-value))))

(defmethod address :relative [state op]
  (let [memory (:memory state)
        pc (get-pc state)
        offset (read-next memory pc)]
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
      true (set-a-to sum)
      (not= (arith/neg-byte? sum) (arith/neg-byte? a)) set-overflow
      (= 1 carry) set-carry
      (arith/neg-byte? sum) set-negative
      true (tick! cycles)
      true (advance-pc bytes-read))))

(defmethod exec-op :and [state {:keys [cycles bytes-read resolved-arg] :as op}]
  (let [a (get-a state)
        and-a (bit-and a resolved-arg)]
    (cond-> state
      (zero? and-a) set-zero
      (arith/neg-byte? and-a) set-negative
      true (tick! cycles)
      true (set-a-to and-a)
      true (advance-pc bytes-read))))

(defmethod exec-op :asl [state {:keys [cycles bytes-read resolved-arg :as op]}]
  (let [a (get-a state)
        [shifted carry] (arith/asl a resolved-arg)]
    (cond-> state
      true (set-carry-to carry)
      (zero? shifted) set-zero
      (arith/neg-byte? shifted) set-negative
      true (tick! cycles)
      true (set-a-to shifted)
      true (advance-pc bytes-read))))

(defmethod exec-op :bcc [state
                         {:keys [cycles bytes-read resolved-arg] :as op}]
  (let [c (get-carry state)
        pc (get-pc state)
        signed-arg (arith/unsigned->signed resolved-arg)]
    (cond-> state
      true (tick! cycles)
      (zero? c) (tick! 1)
      (zero? c) (advance-pc signed-arg)
      (pos? c) (advance-pc bytes-read)
      (page-crossed? pc signed-arg) (tick! 1))))

(defmethod exec-op :bcs [state
                         {:keys [cycles bytes-read resolved-arg] :as op}]
  (let [c (get-carry state)
        pc (get-pc state)
        signed-arg (arith/unsigned->signed resolved-arg)]
    (cond-> state
      true (tick! cycles)
      (pos? c) (tick! 1)
      (pos? c) (advance-pc signed-arg)
      (zero? c) (advance-pc bytes-read)
      (page-crossed? pc signed-arg) (tick! 1))))

(defmethod exec-op :beq [state
                         {:keys [cycles bytes-read resolved-arg] :as op}]
  (let [z (get-zero state)
        pc (get-pc state)
        signed-arg (arith/unsigned->signed resolved-arg)]
    (cond-> state
      true (tick! cycles)
      (pos? z) (tick! 1)
      (pos? z) (advance-pc signed-arg)
      (zero? z) (advance-pc bytes-read)
      (page-crossed? pc signed-arg) (tick! 1))))

(defmethod exec-op :bit [state
                         {:keys [cycles bytes-read resolved-arg] :as op}]
  (let [a (get-a state)]
    (cond-> state
      true (tick! cycles)
      (zero? (bit-and a resolved-arg)) set-zero
      true (set-overflow-to (bit-test resolved-arg 6))
      true (set-negative-to (bit-test resolved-arg 7))
      true (advance-pc bytes-read))))

(defmethod exec-op :bmi [state
                         {:keys [cycles bytes-read resolved-arg] :as op}]
  (let [pc (get-pc state)
        n (get-negative state)
        signed-arg (arith/unsigned->signed resolved-arg)]
    (cond-> state
      true (tick! cycles)
      (pos? n) (tick! 1)
      (pos? n) (advance-pc signed-arg)
      (c/and (pos? n) (page-crossed? pc signed-arg)) (tick! 1)
      (zero? n) (advance-pc bytes-read))))

(defmethod exec-op :bne [state
                         {:keys [cycles bytes-read resolved-arg] :as op}]
  (let [z (get-zero state)
        pc (get-pc state)
        signed-arg (arith/unsigned->signed resolved-arg)]
    (cond-> state
      true (tick! cycles)
      (zero? z) (tick! 1)
      (zero? z) (advance-pc signed-arg)
      (pos? z) (advance-pc bytes-read)
      (c/and (zero? z) (page-crossed? pc signed-arg)) (tick! 1))))

(defmethod exec-op :bpl [state
                         {:keys [cycles bytes-read resolved-arg] :as op}]
  (let [pc (get-pc state)
        n (get-negative state)
        signed-arg (arith/unsigned->signed resolved-arg)]
    (cond-> state
      true (tick! cycles)
      (zero? n) (tick! 1)
      (zero? n) (advance-pc signed-arg)
      (c/and (zero? n) (page-crossed? pc signed-arg)) (tick! 1)
      (pos? n) (advance-pc bytes-read))))

(defmethod exec-op :brk [state
                         {:keys [cycles bytes-read] :as op}]
  (let [pc (get-pc state)]
    (-> state
        (push-16 pc)
        (push-8 (status->byte state))
        (set-pc-to (:irq-brk-vector state))
        (tick! 7))))

(defmethod exec-op :bvc [state
                         {:keys [cycles bytes-read resolved-arg] :as op}]
  (let [pc (get-pc state)
        v (get-overflow state)
        signed-arg (arith/unsigned->signed resolved-arg)]
    (cond-> state
      true (tick! 2)
      (zero? v) (advance-pc signed-arg)
      (zero? v) (tick! 1)
      (c/and (zero? v) (page-crossed? pc signed-arg)) (tick! 1)
      (pos? v) (advance-pc bytes-read))))

(defmethod exec-op :bvs [state
                         {:keys [cycles bytes-read resolved-arg] :as op}]
  (let [pc (get-pc state)
        v (get-overflow state)
        signed-arg (arith/unsigned->signed resolved-arg)]
    (cond-> state
      true (tick! 2)
      (pos? v) (advance-pc signed-arg)
      (pos? v) (tick! 1)
      (c/and (pos? v) (page-crossed? pc signed-arg)) (tick! 1)
      (zero? v) (advance-pc bytes-read))))

(defmethod exec-op :clc [state {:keys [cycles bytes-read] :as op}]
  (-> state
      clear-carry
      (advance-pc bytes-read)
      (tick! cycles)))

(defmethod exec-op :cld [state {:keys [cycles bytes-read] :as op}]
  (-> state
      clear-decimal
      (advance-pc bytes-read)
      (tick! cycles)))

(defmethod exec-op :cli [state {:keys [cycles bytes-read] :as op}]
  (-> state
      clear-interrupt
      (advance-pc bytes-read)
      (tick! cycles)))

(defmethod exec-op :clv [state {:keys [cycles bytes-read] :as op}]
  (-> state
      clear-overflow
      (advance-pc bytes-read)
      (tick! cycles)))

(defmethod exec-op :cmp [state {:keys [cycles bytes-read resolved-arg] :as op}]
  (let [a (get-a state)
        diff (- a resolved-arg)]
    (cond-> state
      (arith/neg-byte? diff) set-negative
      (zero? diff) set-zero
      (neg? diff) set-carry
      true (tick! cycles)
      true (advance-pc bytes-read))))

(defmethod exec-op :cpx [state {:keys [cycles bytes-read resolved-arg] :as op}]
  (let [x (get-x state)
        diff (- x resolved-arg)]
    (cond-> state
      (arith/neg-byte? diff) set-negative
      (zero? diff) set-zero
      (neg? diff) set-carry
      true (tick! cycles)
      true (advance-pc bytes-read))))

(defmethod exec-op :cpy [state {:keys [cycles bytes-read resolved-arg] :as op}]
  (let [y (get-y state)
        diff (- y resolved-arg)]
    (cond-> state
      (arith/neg-byte? diff) set-negative
      (zero? diff) set-zero
      (neg? diff) set-carry
      true (tick! cycles)
      true (advance-pc bytes-read))))

(defmethod exec-op :dec [state
                         {:keys [cycles bytes-read
                                 resolved-arg resolved-address] :as op}]
  (let [memory (:memory state)
        decced (dec resolved-arg)]
    (cond-> state
      true (assoc :memory (memory/write memory resolved-address
                                        decced))
      (zero? decced) set-zero
      (arith/neg-byte? decced) set-negative
      true (tick! cycles)
      true (advance-pc bytes-read))))

(defmethod exec-op :dex [state
                         {:keys [cycles bytes-read] :as state}]
  (let [x (get-x state)]
    (cond-> state
      true dec-x
      (zero? (dec x)) set-zero
      (arith/neg-byte? (dec x)) set-negative
      true (tick! cycles)
      true (advance-pc bytes-read))))

(defmethod exec-op :dey [state
                          {:keys [cycles bytes-read] :as state}]
  (let [y (get-y state)]
    (cond-> state
      true dec-y
      (zero? (dec y)) set-zero
      (arith/neg-byte? (dec y)) set-negative
      true (tick! cycles)
      true (advance-pc bytes-read))))

(defmethod exec-op :eor [state
                         {:keys [cycles bytes-read resolved-arg] :as state}]
  (let [a (get-a state)
        x-or (-> a
                 (bit-xor resolved-arg)
                 (bit-and 0xFF))]
    (cond-> state
      (zero? x-or) set-zero
      (arith/neg-byte? x-or) set-negative
      true (tick! cycles)
      true (advance-pc bytes-read))))

(defmethod exec-op :inc [state
                         {:keys [cycles bytes-read
                                 resolved-arg resolved-address] :as op}]
  (let [memory (:memory state)
        inced (dec resolved-arg)]
    (cond-> state
      true (assoc :memory (memory/write memory resolved-address
                                        inced))
      (zero? inced) set-zero
      (arith/neg-byte? inced) set-negative
      true (tick! cycles)
      true (advance-pc bytes-read))))

(defmethod exec-op :inx [state
                         {:keys [cycles bytes-read] :as state}]
  (let [x (get-x state)
        inced (inc x)]
    (cond-> state
      true inc-x
      (zero? inced) set-zero
      (arith/neg-byte? inced) set-negative
      true (tick! cycles)
      true (advance-pc bytes-read))))

(defmethod exec-op :iny [state
                         {:keys [cycles bytes-read] :as state}]
  (let [y (get-y state)
        inced (inc y)]
    (cond-> state
      true inc-y
      (zero? inced) set-zero
      (arith/neg-byte? inced) set-negative
      true (tick! cycles)
      true (advance-pc bytes-read))))

(defmethod exec-op :jmp [state
                         {:keys [cycles bytes-read resolved-arg] :as op}]
  ;; fix weird paging error
  (-> state
      (tick! cycles)
      (assoc :PC resolved-arg)))

(defmethod exec-op :jsr [state
                         {:keys [cycles bytes-read resolved-arg] :as op}]
  (let [pc (get-pc state)
        return (+ pc bytes-read)]
    (-> state
        (push-8 return)
        (set-pc-to resolved-arg)
        (tick! cycles))))

(defmethod exec-op :lda [state
                         {:keys [cycles bytes-read resolved-arg] :as op}]
  (cond-> state
    true (assoc :A resolved-arg)
    (zero? resolved-arg) set-zero
    (arith/neg-byte? resolved-arg) set-negative
    true (tick! cycles)
    true (advance-pc bytes-read)))

(defmethod exec-op :ldx [state
                         {:keys [cycles bytes-read resolved-arg] :as op}]
  (cond-> state
    true (set-x-to resolved-arg)
    (zero? resolved-arg) set-zero
    (arith/neg-byte? resolved-arg) set-negative
    true (tick! cycles)
    true (advance-pc bytes-read)))

(defmethod exec-op :ldy [state
                         {:keys [cycles bytes-read resolved-arg] :as op}]
  (cond-> state
    true (assoc :Y resolved-arg)
    (zero? resolved-arg) set-zero
    (arith/neg-byte? resolved-arg) set-negative
    true (tick! cycles)
    true (advance-pc bytes-read)))

(defmethod exec-op :lsr [state
                         {:keys [cycles bytes-read resolved-arg
                                 resolved-address address-mode] :as op}]
  (let [memory (:memory state)
        to-shift (if (= :accumulator address-mode) (get-a state) resolved-arg)
        [shifted carry] (arith/lsr to-shift)]
    (cond-> state
      true (set-carry-to carry)
      (zero? shifted) set-zero
      (arith/neg-byte? shifted) set-negative
      true (tick! cycles)
      true (advance-pc cycles)
      (= :accumulator address-mode) (set-a-to shifted)
      (not= :accumulator address-mode) (assoc :memory (memory/write memory
                                                                    resolved-address
                                                                    shifted)))))

(defmethod exec-op :nop [state {:keys [cycles bytes-read]}]
  (-> state
      (tick! cycles)
      (advance-pc bytes-read)))

(defmethod exec-op :ora [state {:keys [cycles bytes-read resolved-arg]}]
  (let [a (get-a state)
        ored-a (bit-or a resolved-arg)]
    (cond-> state
      (zero? ored-a) set-zero
      (arith/neg-byte? ored-a) set-negative
      true (set-a-to ored-a)
      true (tick! cycles)
      true (advance-pc bytes-read))))

(defmethod exec-op :pha [state {:keys [cycles bytes-read]}]
  (let [a (get-a state)]
    (-> state
        (push-8 a)
        (tick! cycles)
        (advance-pc bytes-read))))

(defmethod exec-op :php [state {:keys [cycles bytes-read]}]
  (-> state
      (push-8 (status->byte state))
      (tick! cycles)
      (advance-pc bytes-read)))

(defmethod exec-op :pla [state {:keys [cycles bytes-read]}]
  (let [[pop popped-state] (pop-8 state)]
    (-> popped-state
        (assoc :A pop)
        (tick! cycles)
        (advance-pc bytes-read))))

(defmethod exec-op :plp [state {:keys [cycles bytes-read]}]
  (let [[pop popped-state] (pop-8 state)
        flags (byte->status pop)]
    (-> popped-state
        (merge flags)
        (tick! cycles)
        (advance-pc bytes-read))))

(defmethod exec-op :rol [state
                         {:keys [cycles bytes-read address-mode
                                 resolved-arg resolved-address] :as op}]
  (let [[shifted carry] (arith/asl resolved-arg)
        rotated (+ shifted (* 128 carry))
        memory (:memory state)]
    (cond-> state
      (= :accumulator address-mode) (set-a-to rotated)
      (not= :accumulator address-mode) (assoc :memory (memory/write memory resolved-address rotated))
      true (set-carry-to carry)
      (zero? rotated) set-zero
      (arith/neg-byte? rotated) set-negative
      true (tick! cycles)
      true (advance-pc bytes-read))))

(defmethod exec-op :ror [state
                         {:keys [cycles bytes-read address-mode
                                 resolved-arg resolved-address] :as op}]
  (let [[shifted carry] (arith/lsr resolved-arg)
        rotated (+ shifted (* 128 carry))
        memory (:memory state)]
    (cond-> state
      (= :accumulator address-mode) (set-a-to rotated)
      (not= :accumulator address-mode) (assoc :memory (memory/write memory resolved-address rotated))
      true (set-carry-to carry)
      (zero? rotated) set-zero
      (arith/neg-byte? rotated) set-negative
      true (tick! cycles)
      true (advance-pc bytes-read))))

(defmethod exec-op :rti [state {:keys [cycles bytes-read]}]
  (let [[status state] (pop-8 state)
        status-map (byte->status status)
        pc (pop-16 state)]
    (-> state
        (merge status-map)
        (assoc :PC pc)
        (tick! cycles))))

(defmethod exec-op :rts [state {:keys [cycles]}]
  (let [[pc state] (pop-16 state)]
    (-> state
        (set-pc-to pc)
        (tick! cycles))))

(defmethod exec-op :sbc [state {:keys [cycles resolved-arg bytes-read]}]
  (let [a (get-a state)
        [diff carry] (arith/sub a resolved-arg)]
    (cond-> state
      true (set-a-to diff)
      (zero? diff) set-zero
      (zero? carry) (assoc :V 1)
      true (set-carry-to carry)
      true (tick! cycles)
      true (advance-pc bytes-read))))

(defmethod exec-op :sec [state {:keys [cycles bytes-read]}]
  (-> state
      (advance-pc bytes-read)
      (tick! cycles)
      set-carry))

(defmethod exec-op :sed [state {:keys [cycles bytes-read]}]
  (-> state
      (advance-pc bytes-read)
      (tick! cycles)
      set-decimal))

(defmethod exec-op :sei [state {:keys [cycles bytes-read] :as op}]
  (-> state
      set-interrupt
      (tick! cycles)
      (advance-pc bytes-read)))

(defmethod exec-op :sta [state
                         {:keys [cycles resolved-address bytes-read] :as op}]
  (let [memory (:memory state)
        a (get-a state)]
    (-> state
        (tick! cycles)
        (advance-pc bytes-read)
        (assoc :memory (memory/write memory resolved-address a)))))

(defmethod exec-op :stx [state
                         {:keys [cycles resolved-address bytes-read] :as op}]
  (let [memory (:memory state)
        x (get-x state)]
    (-> state
        (tick! cycles)
        (advance-pc bytes-read)
        (assoc :memory (memory/write memory resolved-address x)))))

(defmethod exec-op :sty [state
                         {:keys [cycles resolved-address bytes-read] :as op}]
  (let [memory (:memory state)
        y (get-y state)]
    (-> state
        (tick! cycles)
        (advance-pc bytes-read)
        (assoc :memory (memory/write memory resolved-address y)))))

(defmethod exec-op :tax [state
                         {:keys [cycles resolved-address bytes-read] :as op}]
  (let [a (get-a state)]
    (cond-> state
      true (set-x-to a)
      (zero? a) set-zero
      (arith/neg-byte? a) set-negative
      true (tick! cycles)
      true (advance-pc bytes-read))))

(defmethod exec-op :tay [state
                         {:keys [cycles resolved-address bytes-read] :as op}]
  (let [a (get-a state)]
    (cond-> state
      true (set-y-to a)
      (zero? a) set-zero
      (arith/neg-byte? a) set-negative
      true (tick! cycles)
      true (advance-pc bytes-read))))

(defmethod exec-op :tsx [state
                         {:keys [cycles resolved-address bytes-read] :as op}]
  (let [sp (get-sp state)]
    (cond-> state
      true (set-x-to sp)
      (zero? sp) set-zero
      (arith/neg-byte? sp) set-negative
      true (tick! cycles)
      true (advance-pc bytes-read))))

(defmethod exec-op :txa [state
                         {:keys [cycles resolved-address bytes-read] :as op}]
  (let [x (get-x state)]
    (cond-> state
      true (set-a-to x)
      (zero? x) set-zero
      (arith/neg-byte? x) set-negative
      true (tick! cycles)
      true (advance-pc bytes-read))))

(defmethod exec-op :txs [state
                         {:keys [cycles resolved-address bytes-read] :as op}]
  (let [x (get-x state)]
    (cond-> state
      true (assoc :S x)
      (zero? x) set-zero
      (arith/neg-byte? x) set-negative
      true (tick! cycles)
      true (advance-pc bytes-read))))

(defmethod exec-op :tya [state
                         {:keys [cycles resolved-address bytes-read] :as op}]
  (let [y (get-y state)]
    (cond-> state
      true (set-a-to y)
      (zero? y) set-zero
      (arith/neg-byte? y) set-negative
      true (tick! cycles)
      true (advance-pc bytes-read))))

(defn exec [state]
  (let [pc (get-pc state)
        memory (:memory state)
        op (memory/read memory pc)
        instruction (->> (get opcodes/ops op)
                         (address state))]
    (println (pprint/cl-format nil "Hex: ~x" op))
    (println instruction)
    (exec-op state instruction)))
