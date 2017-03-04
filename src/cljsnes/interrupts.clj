(ns cljsnes.interrupts
  (:require [cljsnes.spec :as spec]))

(defn check-interrupt [state]
  (get state :interrupt nil))
