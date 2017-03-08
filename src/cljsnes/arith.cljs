(ns cljsnes.arith
  (:require [cljs.spec :as s])
  (:refer-clojure :exclude [inc dec]))

(s/def ::byte (s/int-in 0 256))

(s/def ::address (s/int-in 0 65536))

(s/def ::carry-bit #{0 1})

;; Some addition fns
(defn add [x y]
  (let [sum (+ x y)
        masked-sum (bit-and 255 sum)
        carry (if (bit-test sum 8) 1 0)]
    [masked-sum carry]))

(defn inc [x]
  (add x 1))

(defn sub [x y]
  (let [diff (- x y)
        masked-sum (bit-and 255 diff)
        carry (if (bit-test diff 8) 0 1)]
    [masked-sum carry]))

(defn dec [x]
  (sub x 1))

(defn make-address [lower upper]
  (+ lower (bit-shift-left upper 8)))

(defn address->bytes [address]
  (let [mask-high 0xFF00
        mask-low 0xFF]
    [(bit-and mask-low address)
     (bit-shift-right (bit-and mask-high address) 8)]))

(s/fdef make-address
        :args (s/cat :lower ::byte :upper ::byte)
        :ret ::address)

(s/fdef add
        :args (s/cat :x ::byte :y ::byte)
        :ret (s/cat :sum ::byte :carry ::carry-bit))

(s/fdef inc
        :args (s/cat :x ::byte)
        :ret (s/cat :sum ::byte :carry ::carry-bit))

;; Negative?

(defn neg-byte? [x]
  (bit-test x 8))

(s/fdef neg-byte?
        :args (s/cat :x nat-int?)
        :ret boolean?)

;; Logical Shifts

(defn asl ([x offset]
           (let [shifted (bit-shift-left x offset)
                 masked (bit-and shifted 255)
                 carry (if (neg-byte? shifted) 1 0)]
             [masked carry]))
  ([x] (asl x 1)))

(s/fdef asl
        :args (s/cat :x ::byte)
        :fn #(or (>= (-> % :ret :shifted) (-> % :args :x))
                 (= (-> % :ret :carry) 1))
        :ret (s/cat :shifted ::byte :carry ::carry-bit))

(defn lsr [x]
  (let [shifted (bit-shift-right x 1)
        carry (if (bit-test x 0) 1 0)]
    [shifted carry]))

(s/fdef lsr
        :args (s/cat :x ::byte)
        :fn #(or (<= (-> % :ret :shifted) (-> % :args :x))
                 (= (-> % :ret :carry) 1))
        :ret (s/cat :shifted ::byte :carry ::carry-bit))

(defn unsigned->signed [x]
  (let [mask 128]
    (+ (- (bit-and x mask))
       (bit-and x (bit-not mask)))))

;; Logical ops

(defn l-and [x y]
  (bit-and x y))

(s/fdef l-and
        :args (s/cat :x ::byte :y ::byte)
        :ret ::byte)

(defn l-or [x y]
  (bit-or x y))

(s/fdef l-or
        :args (s/cat :x ::byte :y ::byte)
        :ret ::byte)

(defn l-xor [x y]
  (bit-xor x y))

(s/fdef l-xor
        :args (s/cat :x ::byte :y ::byte)
        :ret ::byte)
