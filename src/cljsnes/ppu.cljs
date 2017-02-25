(ns cljsnes.ppu
  (:require [clojure.spec :as s]))

;;$0000-1FFF is normally mapped by the cartridge to a CHR-ROM or CHR-RAM, often with a bank switching mechanism.
;; $2000-2FFF is normally mapped to the 2kB NES internal VRAM, providing 2 nametables with a mirroring configuration controlled by the cartridge, but it can be partly or fully remapped to RAM on the cartridge, allowing up to 4 simultaneous nametables.
; $3000-3EFF is usually a mirror of the 2kB region from $2000-2EFF. The PPU does not render from this address range, so this space has negligible utility.
;; $3F00-3FFF is not configurable, always mapped to the internal palette control.

(s/def ::cycle (s/int-in 0 341))

(s/def ::scan-line (s/int-in 0 262))

(s/def ::frame nat-int?)
