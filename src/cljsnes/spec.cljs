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

;; State

(s/def ::state (s/keys :req-un [:state/cpu :state/memory]))
