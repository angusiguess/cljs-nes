(ns cljsnes.memory)

(defprotocol Memory
  (read [_ addr])
  (write [_ addr byte]))

(defrecord Nrom [ram ppu-registers apu-io test-mode
                 lower-bank upper-bank vram]
  Memory
  (read [_ addr]
    (cond (<= 0x0 addr 0x1FFF) (get ram (mod addr 0x800))
          (<= 0x2000 addr 0x3FFF) (get ppu-registers (-> addr
                                                         (- 0x2000)
                                                         (mod 0x08)))
          (<= 0x4000 addr 0x4017) (get apu-io (- addr 0x4000))
          (<= 0x4018 addr 0x401F) (get test-mode (- addr 0x4018))
          (<= 0x6000 addr 0x7FFF) (get vram (-> addr
                                                (- 0x6000)
                                                (mod 0x800)))
          (<= 0x8000 addr 0xBFFF) (get lower-bank (- addr 0x8000))
          (<= 0xC000 addr 0xFFFF) (get upper-bank (- addr 0xC000))
          :else (throw (js/Error (str "Address" addr "out of range")))))
  (write [_ addr byte]
    (cond (<= 0x0 addr 0x1FFF) (assoc ram (mod addr 0x800) byte)
          (<= 0x2000 addr 0x3FFF) (assoc ppu-registers
                                         (-> addr
                                             (- 0x2000)
                                             (mod 0x08))
                                         byte)
          (<= 0x4000 addr 0x4017) (assoc apu-io (- addr 0x4000) byte)
          (<= 0x4018 addr 0x401F) (assoc test-mode (- addr 0x4018) byte)
          (<= 0x6000 addr 0x7FFF) (assoc vram
                                         (-> addr
                                             (- 0x6000)
                                             (mod 0x800))
                                         byte)
          :else (throw (js/Error (str "Address" addr "out of range"))))))

(defn make-nrom [lower-bank upper-bank vram]
  (->Nrom (into [] (repeat 0x2000 0))
          (into [] (repeat 0x08 0))
          (into [] (repeat 0x18 0))
          (into [] (repeat 0x08 0))
          lower-bank
          upper-bank
          vram))

(defn init-mem [rom]
  (case (:mapper rom)
    0 (make-nrom (get-in rom [:rom-bank-bytes 0])
                 (get-in rom [:rom-bank-bytes 1])
                 (get-in rom [:vrom-bank-bytes 0]))))
