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
  (:require-macros [cljs.core.async.macros :refer [go go-loop]]))

(enable-console-print!)

(def ppu-cycles (r/cursor state/state [:ppu :cycle]))

(def ppu-line (r/cursor state/state [:ppu :line]))

(def cpu-cycles (r/cursor state/state [:cpu :cycles]))

(defonce stop-chan (a/chan))

(defn init-button []
  [:div [:input {:type "button"
                 :value "Init State!"
                 :on-click #(emulator/init "/Users/angusiguess/Downloads/Super Mario Bros. (Japan, USA).nes")}]])

(defn step-button []
  [:div [:input {:type "button"
                 :value "Step State!"
                 :on-click #(state/step!)}]])

(defn stop-button []
  [:div [:input {:type "button"
                 :value "Stop state!"
                 :on-click #(a/put! stop-chan :stop)}]])

(defn run-button []
  [:div [:input {:type "button"
                 :value "Run state!"
                 :on-click #(go-loop []
                              (let [[v ch] (a/alts! [stop-chan (go 1)])]
                                (when (not= ch stop-chan)
                                  (state/step!)
                                  (recur))))}]])

(defn save-state []
  [:div [:input {:type "button"
                 :value "Save state"
                 :on-click #(state/save-state!)}]])

(defn load-state []
  [:div [:input {:type "button"
                 :value "Load state"
                 :on-click #(state/load-state!)}]])

(defn cpu-cycle []
  [:div [:p "CPU Cycles: " @cpu-cycles]])

(defn ppu-cycle []
  [:div [:p "PPU Cycles: " @ppu-cycles]
   [:p "PPU Line: " @ppu-line]])

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
   (save-state)
   (load-state)
   (run-button)
   (stop-button)
   (ppu-cycle)
   (cpu-cycle)
   (cpu-pc)
   (cpu-registers)])

(defn init []
  (r/render-component [container]
                      (.getElementById js/document "container"))
  (js/console.log "Starting Application"))
