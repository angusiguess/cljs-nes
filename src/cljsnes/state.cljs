(ns cljsnes.state
  (:require [cljsnes.spec :as spec]
            [reagent.ratom :as r]
            [cljsnes.cpu :as cpu]
            [cljsnes.ppu :as ppu]
            [clojure.spec :as s]))

(defonce state (r/atom nil))

(defonce order (r/atom (cycle [:ppu :ppu :ppu :cpu])))

(defonce save-state (r/atom nil))

(defn init-state [memory ppu-memory]
  {:cpu {:a 0
         :x 0
         :y 0
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
         :cycles 0
         :ticks 0
         :nmi 0
         :irq 0
         :reset 0
         :memory memory}
   :ppu {:cycle 0
         :line 0
         :nmi-enable true
         :memory ppu-memory}})

(defn init-vectors [state]
  (let [cpu-mem (get-in state [:cpu :memory])]
    (-> state
        (assoc-in [:cpu :nmi] (cpu/get-address cpu-mem 0xFFFA))
        (assoc-in [:cpu :reset] (cpu/get-address cpu-mem 0xFFFC))
        (assoc-in [:cpu :irq] (cpu/get-address cpu-mem 0xFFFE))
        (assoc-in [:cpu :pc] (cpu/get-address cpu-mem 0xFFFC)))))

(defn init! [memory ppu-memory]
  (reset! state (init-vectors
                 (init-state memory ppu-memory)))
  (reset! order (cycle [:ppu :ppu :ppu :cpu])))

(defn step! []
  (let [next (first @order)]
    (swap! order rest)
    (if (= :ppu next) (swap! state ppu/step)
        (swap! state cpu/step))))

(defn save-state! []
  (reset! save-state @state))

(defn load-state! []
  (reset! state @save-state ))
