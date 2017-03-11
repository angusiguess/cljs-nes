(ns cljsnes.ppu
  (:require [clojure.spec :as s]
            [cljsnes.arith :as arith]
            [cljsnes.memory :as memory]))

;;$0000-1FFF is normally mapped by the cartridge to a CHR-ROM or CHR-RAM, often with a bank switching mechanism.
;; $2000-2FFF is normally mapped to the 2kB NES internal VRAM, providing 2 nametables with a mirroring configuration controlled by the cartridge, but it can be partly or fully remapped to RAM on the cartridge, allowing up to 4 simultaneous nametables.
; $3000-3EFF is usually a mirror of the 2kB region from $2000-2EFF. The PPU does not render from this address range, so this space has negligible utility.
;; $3F00-3FFF is not configurable, always mapped to the internal palette control.

(enable-console-print!)

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

(defn read-status [state]
  (let [memory (get-memory state)
        status (memory/ppu-read memory 0x2000)
        v-cleared (bit-and 0x0F status)
        updated-memory (memory/ppu-write memory 0x2000 v-cleared)]
    (-> state
        ;; TODO do we need these?
        #_(assoc-in [:ppu :scroll-y] 0)
        #_(assoc-in [:ppu :scroll-x] 0)
        (assoc-in [:ppu :write-address-low] 0)
        (assoc-in [:ppu :write-address-high] 0)
        (assoc-in [:ppu :write-started] false)
        (assoc :memory updated-memory))))

(defn write-register-address [state byte]
  (let [write-started (get-write-started state)]
    (cond-> state
      write-started (assoc-in [:ppu :write-address-low] byte)
      (not write-started) (assoc-in [:ppu :write-address-high] byte))))

(defn write-register-scroll [state byte]
  (let [write-started (get-write-started state)]
    (cond-> state
      write-started (assoc-in [:ppu :scroll-y] byte)
      (not write-started) (assoc-in [:ppu :scroll-x] byte))))

(defn write-register-data [state byte]
  (let [memory (get-memory state)
        write-low (get-in state [:ppu :write-address-low])
        write-high (get-in state [:ppu :write-address-high])
        address (arith/make-address write-low write-high)
        ;; TODO make this dependent $2000 bit 2
        [next-high next-low] (arith/address->bytes (inc address))]
    (-> state
        (assoc :memory (memory/ppu-write memory address byte))
        (assoc-in [:ppu :write-address-low] next-low)
        (assoc-in [:ppu :write-address-high] next-high))))

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
      (render-and-fetch? cycle) (do-fetch cycle)
      (v-blank? state) trigger-vblank!
      (clear-vblank? state) clear-vblank!)))
