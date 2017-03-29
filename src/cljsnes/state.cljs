(ns cljsnes.state
  (:require [cljsnes.spec :as spec]
            [reagent.ratom :as r]
            [cljsnes.cpu :as cpu]
            [cljsnes.ppu :as ppu]
            [clojure.spec :as s]))

(defonce state (r/atom nil))

(defonce save-state (r/atom nil))

(defn make-buffer
  ([]
   (make-buffer 0x00))
  ([colour]
   (let [row (into [] (repeat 257 colour))]
     (into [] (repeat 225 row)))))

(defn init-state [memory]
  {:order [:ppu :ppu :ppu :cpu]
   :cpu {:a 0
         :x 0
         :y 0
         :pc 0
         :s 0xFD
         :c 0
         :z 0
         :i 1
         :d 0
         :b 0
         :u 0
         :v 0
         :n 0
         :cycles 0
         :ticks 0
         :nmi 0
         :irq 0
         :reset 0}
   :memory memory
   :ppu {:cycle 0
         :line 0
         :write-address-low 0
         :write-address-high 0
         :write-started false
         :nmi-enable true}
   :display {:front (make-buffer 0x26)
             :back (make-buffer 0x27)}})

(defn get-order [state]
  (get state :order))

(defn step-order [state]
  (update state :order #(drop 1 (cycle %))))

(defn init-vectors [state]
  (let [cpu-mem (get state :memory)]
    (-> state
        (assoc-in [:cpu :nmi] (cpu/get-address cpu-mem 0xFFFA))
        (assoc-in [:cpu :reset] (cpu/get-address cpu-mem 0xFFFC))
        (assoc-in [:cpu :irq] (cpu/get-address cpu-mem 0xFFFE))
        (assoc-in [:cpu :pc] (cpu/get-address cpu-mem 0xFFFC)))))

(defn init! [memory]
  (reset! state (init-vectors
                 (init-state memory))))

(defn step! []
  (let [next (first (get-order @state))]
    (if (= :ppu next) (swap! state (comp step-order ppu/step))
        (swap! state (comp step-order cpu/step)))))

(defn save-state! []
  (reset! save-state @state))

(defn load-state! []
  (reset! state @save-state))
