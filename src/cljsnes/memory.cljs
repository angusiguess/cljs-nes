(ns cljsnes.memory)

(defprotocol Memory
  (read [_ addr])
  (write [_ addr byte]))

(defrecord Nrom [ram ppu-registers apu-io test-mode
                 lower-bank upper-bank prg-ram]
  Memory
  (read [_ addr]
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
  (write [_ addr byte]
    (cond (<= 0x0 addr 0x1FFF) (Nrom.
                                (assoc ram (mod addr 0x800) byte)
                                ppu-registers
                                apu-io
                                test-mode
                                lower-bank
                                upper-bank
                                prg-ram)
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
                                         prg-ram)
          (<= 0x4000 addr 0x4017) (Nrom. ram
                                         ppu-registers
                                         (assoc apu-io (- addr 0x4000) byte)
                                         test-mode
                                         lower-bank
                                         upper-bank
                                         prg-ram)
          (<= 0x4018 addr 0x401F) (Nrom. ram
                                         ppu-registers
                                         apu-io
                                         (assoc test-mode (- addr 0x4018) byte)
                                         lower-bank
                                         upper-bank
                                         prg-ram)
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
                                          prg-ram))
          :else (throw (js/Error (str "Address" addr "out of range"))))))

(defn make-nrom [lower-bank upper-bank prg-ram]
  (->Nrom (into [] (repeat 0x2000 0))
          (into [] (repeat 0x08 0))
          (into [] (repeat 0x18 0))
          (into [] (repeat 0x08 0))
          lower-bank
          upper-bank
          prg-ram))

(defn init-mem [rom]
  (case (:mapper rom)
    0 (make-nrom (get-in rom [:rom-bank-bytes 0])
                 (or (get-in rom [:rom-bank-bytes 1])
                     (get-in rom [:rom-bank-bytes 0]))
                 (get-in rom [:vrom-bank-bytes 0]))))
