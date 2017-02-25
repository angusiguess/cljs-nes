(ns cljsnes.spec
  (:require [clojure.spec :as s]
            [cljsnes.memory :as memory]
            [clojure.test.check.generators :as gen]))

(s/def ::bit #{0 1})

(s/def ::byte (s/int-in 0 256))

(s/def ::address (s/int-in 0 65536))

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

(s/def :cpu/b ::bit)

(s/def :cpu/u ::bit)

(s/def :cpu/v ::bit)

(s/def :cpu/n ::bit)

(s/def :cpu/cycles (s/int-in 0 1000000000))

(s/def :state/memory (s/spec any?
                             :gen #(gen/return
                                    (memory/init-mem {:mapper 0}))))

(s/def :state/cpu (s/keys :req-un [:cpu/a :cpu/x :cpu/pc :cpu/s :cpu/c
                                   :cpu/z :cpu/i :cpu/d :cpu/b :cpu/u
                                   :cpu/v :cpu/n :cpu/cycles]))

(s/def :cpu/status (s/keys :req-un [:cpu/n :cpu/v :cpu/b :cpu/d
                                    :cpu/i :cpu/z :cpu/c]))

(s/def ::state (s/keys :req-un [:state/cpu :state/memory]))
