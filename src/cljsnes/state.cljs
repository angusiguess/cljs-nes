(ns cljsnes.state
  (:require [cljsnes.spec :as spec]
            [clojure.spec :as s]))

(defonce state (atom nil))

(defn init-state [memory]
  {:cpu {:a 0
         :x 0
         :pc 0
         :s 0xFD
         :c 0
         :z 0
         :i 0
         :d 0
         :b 0
         :u 0
         :v 0
         :n 0
         :cycles 0}
   :memory memory})

(s/valid? ::spec/state (init-state []))
