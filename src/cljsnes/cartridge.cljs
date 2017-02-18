(ns cljsnes.cartridge
  (:require [cljs.nodejs :as node]
            [clojure.string :as str]
            [cljs.reader :as reader]
            [clojure.spec :as s]))

(def fs (node/require "fs"))

(def buffer (node/require "buffer"))

(def util (node/require "util"))

(def Buffer (.-Buffer buffer))

(s/def :cart/rom-banks (s/int-in 2 9))

(s/def :cart/vrom-banks (s/int-in 1 11))

(s/def :cart/vrom-bank-bytes (s/coll-of (s/coll-of
                                         (s/int-in 0 256) :into [])
                                        :into []))

(s/def :cart/rom-bank-bytes (s/coll-of (s/coll-of (s/int-in 0 256)
                                                  :into [])
                                       :into []))

(s/def :cart/vertical-mirroring boolean?)




(defn stat-file [path]
  (.statSync fs path))

(defn get-size [stat]
  (aget stat "size"))

(defn open-file [path]
  (.openSync fs path "r"))

(defn read-file [path]
  (let [fd (open-file path)
        size (-> path
                 stat-file
                 get-size)
        buf (.alloc Buffer size)]
    (.readSync fs fd buf 0 size)
    (into [] (es6-iterator-seq (.values buf)))))

(defn get-opts-one [byte]
  (let [bits (map #(if (bit-test byte %) 1 0) (range 8))
        [vertical-mirroring battery-ram trainer four-screen
         & mapper-lower] bits]
    {:vertical-mirroring (pos? vertical-mirroring)
     :battery-ram (pos? battery-ram)
     :trainer (pos? trainer)
     :four-screen (pos? four-screen)
     :mapper-lower (into [] mapper-lower)}))

(defn get-opts-two [byte]
  (let [bits (map #(if (bit-test byte %) 1 0) (range 8))
        [vs-system _ _ _ & mapper-upper] bits]
    {:vs-system (pos? vs-system)
     :mapper-upper (into [] mapper-upper)}))

(defn pal? [byte]
  (bit-test byte 0))

(defn get-mapper [cart]
  (let [lower (get cart :mapper-lower)
        upper (get cart :mapper-upper)]
    (assoc cart :mapper (reader/read-string (str
                                            "2r"
                                            (str/join upper)
                                            (str/join lower))))))

(defn parse-headers
  "This table was cribbed from http://fms.komkon.org/EMUL8/NES.html#LABM
  Byte     Contents
  ---------------------------------------------------------------------------
  0-3      String \"NES^Z\" used to recognize .NES files.
  4        Number of 16kB ROM banks.
  5        Number of 8kB VROM banks.
  6        bit 0     1 for vertical mirroring, 0 for horizontal mirroring.
         bit 1     1 for battery-backed RAM at $6000-$7FFF.
         bit 2     1 for a 512-byte trainer at $7000-$71FF.
         bit 3     1 for a four-screen VRAM layout.
         bit 4-7   Four lower bits of ROM Mapper Type.
  7        bit 0     1 for VS-System cartridges.
         bit 1-3   Reserved, must be zeroes!
         bit 4-7   Four higher bits of ROM Mapper Type.
  8        Number of 8kB RAM banks. For compatibility with the previous
         versions of the .NES format, assume 1x8kB RAM page when this
         byte is zero.
  9        bit 0     1 for PAL cartridges, otherwise assume NTSC.
         bit 1-7   Reserved, must be zeroes!
  10-15    Reserved, must be zeroes!
  16-...   ROM banks, in ascending order. If a trainer is present, its
         512 bytes precede the ROM bank contents.
  ...-EOF  VROM banks, in ascending order."
  [rom]
  (let [header (subvec rom 0 4)
        rom-banks (get rom 4)
        vrom-banks (get rom 5)
        opts-one (get rom 6)
        opts-two (get rom 7)
        ram-banks (get rom 8)
        pal (pal? (get rom 9))
        rom-offset (+ 16 (* 16384 rom-banks))
        vrom-offset (+ rom-offset (* 8192 vrom-banks))]
    (assert (= header [78 69 83 26]))
    (-> {:header header
         :rom-banks rom-banks
         :vrom-banks vrom-banks
         :ram-banks ram-banks
         :pal pal
         :rom-bank-bytes (mapv (partial into [])
                               (partition 16384
                                          (subvec rom 16 rom-offset)))
         :vrom-bank-bytes (mapv (partial into [])
                                (partition 8192
                                           (subvec rom
                                                   rom-offset
                                                   vrom-offset)))
         :count (count rom)}
        (merge (get-opts-one opts-one)
               (get-opts-two opts-two))
        get-mapper)))

(defn load-rom [path]
  (-> (read-file path)
      parse-headers))
