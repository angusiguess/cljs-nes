(ns cljsnes.ppu
  (:require [clojure.spec :as s]
            [cljsnes.arith :as arith]
            [cljsnes.memory :as memory]))

;;$0000-1FFF is normally mapped by the cartridge to a CHR-ROM or CHR-RAM, often with a bank switching mechanism.
;; $2000-2FFF is normally mapped to the 2kB NES internal VRAM, providing 2 nametables with a mirroring configuration controlled by the cartridge, but it can be partly or fully remapped to RAM on the cartridge, allowing up to 4 simultaneous nametables.
                                        ; $3000-3EFF is usually a mirror of the 2kB region from $2000-2EFF. The PPU does not render from this address range, so this space has negligible utility.
;; $3F00-3FFF is not configurable, always mapped to the internal palette control.

;; We need a couple of internal registers for everything to work.

;; v is a register containing current vram address info.
;; t is a register containing temporary vram address info.
;; x is the fine x scroll, which allows for partial scrolling
;; w is the write toggle, which we use since some of our registers require two writes.
;; we'll write values to register flags too. To start let's phrase them as getters.
;; cycle is the horizontal render cycle
;; line is the scan line we're rendering
;; frame is a frame counter I'll use
;; I don't think I need to remove anything, I'll just make things consistent
;; with scroll

(enable-console-print!)

;; Getters for internal state

(defn get-v [state]
  (get-in state [:ppu :v]))

(defn get-t [state]
  (get-in state [:ppu :t]))

(defn get-x [state]
  (get-in state [:ppu :x]))

(defn get-w [state]
  (get-in state [:ppu :w]))

(defn get-memory [state]
  (get state :memory))

(defn get-line [state]
  (get-in state [:ppu :line]))

(defn inc-line [state]
  (update-in state [:ppu :line] inc))

(defn zero-line [state]
  (assoc-in state [:ppu :line] 0))

(defn get-cycle [state]
  (get-in state [:ppu :cycle]))

(defn inc-cycle [state]
  (update-in state [:ppu :cycle] inc))

(defn zero-cycle [state]
  (-> state
      (assoc-in [:ppu :cycle] 0)
      (update-in [:ppu :line] inc)))

(defn get-write-started [state]
  (get-in state [:ppu :write-started]))

(defn set-vblank! [state]
  (let [memory (get-memory state)
        byte (memory/ppu-read memory 0x2002)]
    (-> state
        (assoc :memory (memory/ppu-write memory 0x2002 (bit-or 0x70 byte)))
        (assoc-in [:ppu :vblank] true))))

(defn clear-vblank! [state]
  (let [memory (get-memory state)
        byte (memory/ppu-read memory 0x2002)]
    (-> state
        (assoc :memory (memory/ppu-write memory
                                         0x2002
                                         (bit-or 0xE0 byte)))
        (assoc-in [:ppu :vblank] false))))

(defn get-frame [state]
  (get-in state [:ppu :frame]))

(defn nmi-enabled? [state]
  (get-in state [:ppu :nmi-enable]))

;; Register $2000, write only
;; PPU Control Register
;; VPHB SINN
;; V = VBLANK NMI, generate an interrupt if this is set.
;; P = PPU M/S, read backdrop from ext or write to ext
;; H = Sprite size, 0 = 8x8, 1 = 8x16
;; B = Background pattern table, 0 = $0000 1 = $1000
;; S = Sprite pattern table, 0 = $0000 1 = $1000, 8x16 ignores
;; I = VRAM increment, 0 adds 1, 1 adds 32 on vram increment
;; NN = base nametable address, 0, 1, 2, 3


;; t: ...BA.. ........ = d: ......BA
(defn write-control [state byte]
  (let [nametable-bits (bit-shift-left (bit-and 0x03 byte) 10)
        t (get-t state)]
    (update-in state [:ppu :t] bit-or nametable-bits)))

;; Register $2002, read only
;; PPU status register
;; VSO. ....

;; V = Vblank has started
;; S = Sprite zero hit. Set when a nonzero pixel of sprite 0 overlaps with nonzero background
;; O = Sprite overflow, Set during sprite evaluation

(defn read-status [state]
  (let [memory (get-memory state)
        status (memory/ppu-read memory 0x2000)
        v-cleared (bit-and 0x7F status)
        updated-memory (memory/ppu-write memory 0x2000 v-cleared)]
    (-> state
        (assoc-in [:ppu :w] false)
        (assoc :memory updated-memory))))

;; Register $2003, OAM address. TODO add this

;; Register $2004, OAM data. TODO add this.

;; Register $2005, PPUSCROLL
;; Write twice to set scroll x and then scroll y

;; If w is false

;;t: ....... ...HGFED = d: HGFED...
;;x:              CBA = d: .....CBA
;;w:                  = 1

;; If w is true
;; t: CBA..HG FED..... = d: HGFEDCBA
;; w:                  = 0

(defn- scroll-second-write [t byte]
  (let [t (bit-and 2r000110000011111 t)
        cba (bit-shift-left (bit-and 0x07 byte) 12)
        defgh (bit-shift-left (bit-and 0xF8 byte) 2)]
    (bit-or t cba defgh)))

(defn write-register-scroll [state byte]
  (let [w (get-w state)
        for-t (bit-shift-right byte 3)
        for-x (bit-and byte 0x07)]
    (cond-> state
      (not w) (update-in [:ppu :t] #(-> % (bit-shift-right 5)
                                        (bit-shift-left 5)
                                        (bit-or for-t)))
      (not w) (assoc-in [:ppu :x] for-x)
      (not w) (assoc-in [:ppu :w] true)
      w (update-in [:ppu :t] scroll-second-write byte)
      w (assoc-in [:ppu :w] false))))

;; $2005 Address register first write
;;t: .FEDCBA ........ = d: ..FEDCBA
;;t: X...... ........ = 0
;;w:                  = 1

;;second write
;;t: ....... HGFEDCBA = d: HGFEDCBA
;;v                   = t
;;w:                  = 0

(defn- first-address-write [t byte]
  (let [abcdef (-> byte (bit-and 0x3F) (bit-shift-left 8))]
    (bit-or t abcdef (bit-and 0x80FF t))))

(defn- second-address-write [t byte]
  (bit-or t byte))

(defn- copy-t-to-v [state]
  (assoc-in state [:ppu :v] (get-in state [:ppu :t])))

(defn write-register-address [state byte]
  (let [w (get-w state)]
    (cond-> state
      (not w) (update-in [:ppu :t] first-address-write byte)
      (not w) (assoc-in [:ppu :w] true)
      w (update-in [:ppu :t] second-address-write byte)
      w copy-t-to-v
      w (assoc-in [:ppu :w] false))))

(defn write-register-data [state byte]
  (let [memory (get-memory state)
        address (get-v state)
        increment-y (-> memory (memory/ppu-read 0x2000) (bit-test 3))]
    (cond-> state
      true (assoc :memory (memory/ppu-write memory address byte))
      increment-y (update-in [:ppu :v] + 32)
      (not increment-y) (update-in [:ppu :v] inc))))

(defn get-vram-address [state]
  (let [vram-low (get-in state [:ppu :write-address-low])
        vram-high (get-in state [:ppu :write-address-high])]
    (arith/make-address vram-low vram-high)))

(defn get-ppu-mask [state]
  (memory/ppu-read (get-memory state) 0x2001))

(defn background-enabled? [state]
  (let [ppu-mask (get-ppu-mask state)]
    (bit-test ppu-mask 3)))

(defn sprite-enabled? [state]
  (let [ppu-mask (get-ppu-mask state)]
    (bit-test ppu-mask 4)))

(defn rendering-enabled? [state]
  (or (background-enabled? state)
      (sprite-enabled? state)))

(defn nmi-interrupt? [state]
  (get-in state [:ppu :nmi-enable]))

(defn even-frame? [state]
  (get-in state [:ppu :f]))


(defn cycle-wrap? [state]
  (= 340 (get-cycle state)))

(defn line-wrap? [state]
  (and (cycle-wrap? state)
       (= 261 (get-line state))))

(defn v-blank? [state]
  (let [line (get-line state)
        cycle (get-cycle state)]
    (and (= line 241)
         (= cycle 1)
         (nmi-interrupt? state))))

(defn clear-vblank? [state]
  (let [line (get-line state)
        cycle (get-cycle state)]
    (and (= 261 line)
         (= 1 cycle))))

(defn visible-cycle [cycle]
  (<= 1 cycle 256))

(defn pre-fetch-cycle [cycle]
  (<= 321 cycle 336))

(defn fetch-cycle [cycle]
  (or (visible-cycle cycle)
      (pre-fetch-cycle cycle)))

(defn visible-line [line]
  (< line 240))

(defn render-line [line]
  (or (visible-line line)
      (= 261 line)))

(def render-and-fetch? (comp fetch-cycle render-line))

(defn do-fetch [state cycle])

(defn tick! [state]
  (cond-> state
    (not (cycle-wrap? state)) inc-cycle
    (cycle-wrap? state) zero-cycle
    (line-wrap? state) zero-line))

(defn trigger-vblank! [state]
  (println "Triggering Vblank")
  (-> state
      set-vblank!
      (assoc :interrupt :nmi)))

(defn step [state]
  (let [cycle (get-cycle state)]
    (cond-> state
      true tick!
      (v-blank? state) trigger-vblank!
      (clear-vblank? state) clear-vblank!)))
