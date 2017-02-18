(ns cljsnes.cpu
  (:require [cljsnes.arith :as arith]
            [cljsnes.memory :as memory]
            [cljsnes.opcodes :as opcodes]
            [cljs.spec :as s]
            [cljs.core.async :as a]
            [cljs.core :as c])
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


(s/def ::A ::arith/byte)

(s/def ::X ::arith/byte)

(s/def ::PC ::arith/address)

(s/def ::S ::arith/byte)

(s/def ::C ::arith/carry-bit)

(s/def ::Z #{0 1})

(s/def ::I #{0 1})

(s/def ::B #{0 1})

(s/def ::V #{0 1})

(s/def ::N #{0 1})

(s/def ::nmi-vector ::arith/address)

(s/def ::reset-vector ::arith/address)

(s/def ::irq-brk-vector ::arith/address)

(s/def ::cycles nat-int?)

(s/def ::state (s/keys :req-un [::A ::X ::PC ::I ::S ::C ::Z ::B ::V ::N
                                ::cycles ::nmi-vector ::reset-vector
                                ::irq-brk-vector]))

;; Manipulating state

(defonce cpu-state (atom nil))

(defn get-address [memory address]
  (let [lower (memory/read memory address)
        upper (memory/read memory (inc address))]
    (arith/make-address lower upper)))

(defn init-state [memory]
  (reset! cpu-state {:A 0
                     :X 0
                     :PC (get-address memory 0xFFFC)
                     :S 0xFD
                     :C 0
                     :Z 0
                     :I 1
                     :B 0
                     :V 0
                     :N 1
                     :memory memory
                     :cycles 0
                     :nmi-vector (get-address memory 0xFFFA)
                     :reset-vector (get-address memory 0xFFFC)
                     :irq-brk-vector (get-address memory 0xFFFE)}))

;; Eventually we'll want to broadcast cycle ticks for video.

(defonce cycle-clock (a/chan 1024))

(defn tick! [{:keys [cycles] :as state}]
  (a/put! cycle-clock (inc cycles))
  (update state :cycles inc))

(defn exec [state]
  (let [pc (:PC state)
        memory (:memory state)
        op (memory/read memory pc)]
    (get opcodes/ops op)))

;; Addressing

(defn page-crossed? [address offset]
  (let [mask 0x0F00]
    (not= (bit-and address mask)
          (bit-and (+ address offset) mask))))

(defn read-next [memory pc]
  (memory/read memory (inc pc)))

(defmulti address (fn [state op] (:address-mode op)))

(defmethod address :immediate [state op]
  (let [memory (:memory state)
        pc (:PC state)]
    (assoc op :resolved-arg (memory/read memory (inc pc)))))

(defmethod address :zero [state op]
  (let [memory (:memory state)
        pc (:PC state)]
    (assoc op :resolved-arg (->> pc
                                 (read-next memory)
                                 (memory/read memory)))))

(defmethod address :zero-x [state op]
  (let [memory (:memory state)
        pc (:PC state)
        x (:X state)
        address (read-next memory pc)
        [sum _] (arith/add address x)]
    (assoc op :resolved-arg (memory/read memory sum))))

(defmethod address :zero-y [state op]
  (let [memory (:memory state)
        pc (:PC state)
        y (:Y state)
        address (read-next memory pc)
        [sum _] (arith/add address y)]
    (assoc op :resolved-arg (memory/read memory sum))))

(defmethod address :absolute [state op]
  (let [memory (:memory state)
        pc (:PC state)
        lower (read-next memory pc)
        upper (read-next memory (inc pc))]
    (assoc op :resolved-arg
           (arith/make-address lower upper))))

(defmethod address :absolute-x [state op]
  (let [memory (:memory state)
        pc (:PC state)
        x (:X state)
        lower (read-next memory pc)
        upper (read-next memory (inc pc))
        address (arith/make-address lower upper)]
    (cond-> op
      (page-crossed? address x) (update :cycles inc)
      true (assoc :resolved-arg (+ address x)))))

(defmethod address :absolute-y [state op]
  (let [memory (:memory state)
        pc (:PC state)
        y (:Y state)
        lower (read-next memory pc)
        upper (read-next memory (inc pc))
        address (arith/make-address lower upper)]
    (cond-> op
      (page-crossed? address y) (update :cycles inc)
      true (assoc :resolved-arg (+ address y)))))

(defmethod address :indirect [state op]
  (let [memory (:memory state)
        pc (:PC state)
        lower (read-next memory pc)
        upper (read-next memory (inc pc))
        address (arith/make-address lower upper)]
    (assoc op :resolved-arg (memory/read memory address))))

(defmethod address :indirect-x [state op]
  (let [memory (:memory state)
        pc (:PC state)
        x (:X state)
        base-address (read-next memory pc)
        offset-address (bit-and 0xFF
                                  (+ x base-address))]
    (assoc op :resolved-arg (memory/read memory offset-address))))

(defmethod address :indirect-y [state op]
  (let [memory (:memory state)
        pc (:PC state)
        y (:Y state)
        base-address (read-next memory pc)
        base-value (memory/read memory base-address)
        offset-value (+ base-value y)]
    (cond-> op
      (page-crossed? base-value y) (update :cycles inc)
      true (assoc :resolved-arg offset-value))))

(defmethod address :relative [state op]
  (let [memory (:memory state)
        pc (:PC state)
        offset (read-next memory pc)]
    (assoc op :resolved-arg offset)))

(defmethod address :implied [state op]
  (assoc op :resolved-arg nil))

(defmethod address :accumulator [state op]
  (let [a (:A state)]
    (assoc op :resolved-arg a)))
