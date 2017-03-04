(ns app.renderer
  (:require [reagent.core :as r]
            [cljs.spec :as s]
            [cljs.core.async :as a]
            [cljsnes.state :as state]
            [cljsnes.spec :as spec]
            [clojure.pprint :as pprint]
            [cljsnes.cpu :as cpu]
            [cljsnes.emulator :as emulator]
            [cljsnes.assembler :as assembler]
            [cljsnes.cartridge :as cartridge]
            [cljsnes.memory :as memory]
            [cljsnes.opcodes :as opcodes])
  (:require-macros [cljs.core.async.macros :refer [go]]))

(enable-console-print!)

(def ppu-cycles (r/cursor state/state [:ppu :cycle]))

(def cpu-cycles (r/cursor state/state [:cpu :cycles]))

(defn init-button []
  [:div [:input {:type "button"
                 :value "Init State!"
                 :on-click #(emulator/init "/Users/angusiguess/Downloads/Super Mario Bros. (Japan, USA).nes")}]])

(defn step-button []
  [:div [:input {:type "button"
                 :value "Step State!"
                 :on-click #(state/step!)}]])

(defn cpu-cycle []
  [:div [:p "CPU Cycles: " @cpu-cycles]])

(defn ppu-cycle []
  [:div [:p "PPU Cycles: " @ppu-cycles]])

(defn cpu-pc []
  (let [cursor (r/cursor state/state [:cpu :pc])]
    [:div [:p "Program Counter: " (pprint/cl-format nil "~x" @cursor)]]))

(defn cpu-registers []
  (let [a (r/cursor state/state [:cpu :a])
        x (r/cursor state/state [:cpu :x])
        y (r/cursor state/state [:cpu :y])
        i (r/cursor state/state [:cpu :i])
        s (r/cursor state/state [:cpu :s])]
    [:div [:p "A: " @a " X: " @x " Y: " @y " I: " @i]
          [:p "Stack Pointer: " @s]]))

(defn container []
  [:div
   (init-button)
   (step-button)
   (ppu-cycle)
   (cpu-cycle)
   (cpu-pc)
   (cpu-registers)])

(defn init []
  (r/render-component [container]
                      (.getElementById js/document "container"))
  (js/console.log "Starting Application"))
