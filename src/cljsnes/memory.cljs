(ns cljsnes.memory)

(defprotocol CPUMemory
  (cpu-read [_ addr])
  (cpu-write [_ addr byte]))

(defprotocol PPUMemory
  (ppu-read [_ addr])
  (ppu-write [_ addr byte]))

;; CPU ROM mappers

(defrecord Nrom [ram ppu-registers apu-io test-mode
                 lower-bank upper-bank prg-ram chr vram mirroring palette-ram]
  CPUMemory
  (cpu-read [_ addr]
    (cond (<= 0x0 addr 0x1FFF) (get ram (mod addr 0x800))
          (<= 0x2000 addr 0x3FFF) (get ppu-registers (-> addr
                                                         (- 0x2000)
                                                         (mod 0x08)))
          (<= 0x4000 addr 0x4017) (get apu-io (- addr 0x4000))
          (<= 0x4018 addr 0x401F) (get test-mode (- addr 0x4018))
          (<= 0x6000 addr 0x7FFF) (get prg-ram (-> addr
                                                (- 0x6000)
                                                (mod 0x800)))
          (<= 0x8000 addr 0xBFFF) (get lower-bank (- addr 0x8000))
          (<= 0xC000 addr 0xFFFF) (get upper-bank (- addr 0xC000))
          :else (throw (js/Error (str "Address" addr "out of range")))))
  (cpu-write [_ addr byte]
    (cond (<= 0x0 addr 0x1FFF) (Nrom.
                                (assoc ram (mod addr 0x800) byte)
                                ppu-registers
                                apu-io
                                test-mode
                                lower-bank
                                upper-bank
                                prg-ram
                                chr
                                vram
                                mirroring
                                palette-ram)
          (<= 0x2000 addr 0x3FFF) (Nrom. ram
                                         (assoc ppu-registers
                                                (-> addr
                                                    (- 0x2000)
                                                    (mod 0x08))
                                                byte)
                                         apu-io
                                         test-mode
                                         lower-bank
                                         upper-bank
                                         prg-ram
                                         chr
                                         vram
                                         mirroring
                                         palette-ram)
          (<= 0x4000 addr 0x4017) (Nrom. ram
                                         ppu-registers
                                         (assoc apu-io (- addr 0x4000) byte)
                                         test-mode
                                         lower-bank
                                         upper-bank
                                         prg-ram
                                         chr
                                         vram
                                         mirroring
                                         palette-ram)
          (<= 0x4018 addr 0x401F) (Nrom. ram
                                         ppu-registers
                                         apu-io
                                         (assoc test-mode (- addr 0x4018) byte)
                                         lower-bank
                                         upper-bank
                                         prg-ram
                                         chr
                                         vram
                                         mirroring
                                         palette-ram)
          (<= 0x6000 addr 0x7FFF) (Nrom. ram
                                         ppu-registers
                                         apu-io
                                         test-mode
                                         lower-bank
                                         upper-bank
                                         (assoc prg-ram
                                                (-> addr
                                                    (- 0x6000)
                                                    (mod 0x800))
                                                byte)
                                         chr
                                         vram
                                         mirroring
                                         palette-ram)
          :else (throw (js/Error (str "Address" addr "out of range")))))
  PPUMemory
  (ppu-read [_ addr]
    (let [vram-offset 0x2000
          palette-offset 0x3000]
      (cond (<= 0x0000 addr 0x1FFF) (get chr addr)
            (<= 0x2000 addr 0x2007) (get ppu-registers (- addr vram-offset))
            (<= 0x2008 addr 0x2FFF) (get vram (- addr vram-offset))
            (<= 0x3F00 addr 0x3FFF) (get palette-ram (- addr palette-offset))
            :else (throw (js/Error (str "Address" addr "out of range"))))))
  (ppu-write [_ addr byte]
    (let [vram-offset 0x2000
          palette-offset 0x3000]
      (cond
        (<= 0x2000 addr 0x2007) (Nrom. ram
                                       (assoc ppu-registers (- addr vram-offset) byte)
                                       apu-io
                                       test-mode
                                       lower-bank
                                       upper-bank
                                       prg-ram
                                       chr
                                       vram
                                       mirroring
                                       palette-ram)
        (<= 0x2008 addr 0x2FFF) (Nrom. ram
                                       ppu-registers
                                       apu-io
                                       test-mode
                                       lower-bank
                                       upper-bank
                                       prg-ram
                                       chr
                                       (assoc vram
                                              (- addr vram-offset)
                                              byte)
                                       mirroring
                                       palette-ram)
        (<= 0x3000 addr 0x3FFF) (Nrom. ram
                                       ppu-registers
                                       apu-io
                                       test-mode
                                       lower-bank
                                       upper-bank
                                       prg-ram
                                       chr
                                       vram
                                       mirroring
                                       (assoc palette-ram
                                              (- addr palette-offset)
                                              byte))
        :else (throw (js/Error (str "Address" addr "out of range")))))))

(defn make-nrom [lower-bank upper-bank prg-ram chr mirroring]
  (map->Nrom {:ram (into [] (repeat 0x2000 0))
              :ppu-registers (into [] (repeat 0x08 0))
              :apu-io (into [] (repeat 0x18 0))
              :test-mode (into [] (repeat 0x08 0))
              :lower-bank lower-bank
              :upper-bank upper-bank
              :prg-ram prg-ram
              :chr chr
              :vram (into [] (repeat 0xFFF 0))
              :mirroring mirroring
              :palette-ram (into [] (repeat 0x1F 0))}))

(defn init-mem [rom]
  (case (:mapper rom)
    0 (make-nrom (get-in rom [:rom-bank-bytes 0])
                 (or (get-in rom [:rom-bank-bytes 1])
                     (get-in rom [:rom-bank-bytes 0]))
                 (get-in rom [:vrom-bank-bytes 0])
                 (get-in rom [:chr-bank-bytes 0])
                 (get rom :vertical-mirroring))))
