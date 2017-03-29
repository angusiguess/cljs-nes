(ns cljsnes.spec
  (:require [clojure.spec :as s]
            [cljsnes.memory :as memory]
            [clojure.test.check.generators :as gen]))

(s/def ::bit #{0 1})

(s/def ::byte (s/int-in 0 256))

(s/def ::address (s/int-in 0 65536))

;; CPU

(s/def :cpu/a ::byte)

(s/def :cpu/x ::byte)

(s/def :cpu/y ::byte)

(s/def :cpu/pc ::address)

(s/def :cpu/s ::byte)

(s/def :cpu/n ::bit)

(s/def :cpu/c ::bit)

(s/def :cpu/z ::bit)

(s/def :cpu/i ::bit)

(s/def :cpu/d ::bit)

(s/def :cpu/u ::bit)

(s/def :cpu/v ::bit)

(s/def :cpu/n ::bit)

(s/def :cpu/cycles (s/int-in 0 1000000000))

(s/def :cpu/ticks (s/int-in 0 8192))

(s/def :cpu/nmi ::byte)

(s/def :cpu/irq ::byte)

(s/def :cpu/reset ::byte)

(s/def :cpu/status (s/keys :req-un [:cpu/n :cpu/v :cpu/d
                                    :cpu/i :cpu/z :cpu/c]))

(s/def :state/cpu (s/keys :req-un [:cpu/a :cpu/x :cpu/pc :cpu/s :cpu/c
                                   :cpu/z :cpu/i :cpu/d :cpu/u
                                   :cpu/v :cpu/n :cpu/cycles :cpu/ticks
                                   :cpu/memory]))

;; PPU

(s/def :ppu/frame-parity boolean?)

(s/def :ppu/cycle (s/int-in 0 341))

(s/def :ppu/scan-line (s/int-in 0 262))

(s/def :ppu/frame nat-int?)

;; PPU CTRL $2000

(s/def :ppu/nmi-enable boolean?)

(s/def :ppu/ppu-master boolean?)

(s/def :ppu/sprite-height boolean?)

(s/def :ppu/background-tile-select boolean?)

(s/def :ppu/increment-mode boolean?)

(s/def :ppu/nametable-select #{0 1 2 3})

;; PPU MASK $2001

(s/def :ppu/colour-emphasis-red boolean?)

(s/def :ppu/colour-emphasis-green boolean?)

(s/def :ppu/colour-emphasis-blue boolean?)

(s/def :ppu/sprite-enable boolean?)

(s/def :ppu/background-enable boolean?)

(s/def :ppu/sprite-column-left boolean?)

(s/def :ppu/background-left-column-enable boolean?)

(s/def :ppu/greyscale boolean?)

;; PPU STATUS $2002

(s/def :ppu/vblank boolean?)

(s/def :ppu/sprite-zero-hit boolean?)

(s/def :ppu/sprite-overflow boolean?)

(s/def :ppu/oam-addr ::byte)

(s/def :ppu/oam-data ::byte)

(s/def :ppu/ppu-scroll ::byte)

(s/def :ppu/ppu-addr ::byte)

(s/def :ppu/ppu-data ::byte)

(s/def :ppu/oam-data ::byte)

(s/def :ppu/rgb (s/int-in 0x000000 0x1000000))

(s/def :ppu/display (s/coll-of :ppu/rgb
                               :count 61440
                               :into []))

(s/def ::ppu (s/keys :req-un [:ppu/cycle :ppu/scan-line :ppu/frame
                              :ppu/nmi-enable :ppu/master :ppu/sprite-height
                              :ppu/background-tile-select :ppu/increment-mode
                              :ppu/colour-emphasis-red :ppu/colour-emphasis-green
                              :ppu/colour-emphasis-blue :ppu/sprite-column-left
                              :ppu/background-left-column-enable :ppu/vblank
                              :ppu/sprite-zero-hit :ppu/sprite-overflow
                              :ppu/oam-addr :ppu/oam-data :ppu/ppu-scroll
                              :ppu/ppu-addr :ppu/ppu-data :ppu/oam-data]))

;; Interrupts

(s/def :interrupt/interrupt #{:nmi :reset :irq})

;; State

(s/def ::state (s/keys :req-un [:state/cpu :state/ppu]))
