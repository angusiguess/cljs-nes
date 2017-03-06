(ns cljsnes.emulator
  (:require [cljsnes.cartridge :as cart]
            [cljsnes.memory :as memory]
            [cljsnes.state :as state]
            [cljsnes.cpu :as cpu]))

(defn init [path]
  (let [rom (cart/load-rom path)
        memory (memory/init-mem rom)]
    (state/init! memory)))
