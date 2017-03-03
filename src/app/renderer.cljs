(ns app.renderer
  (:require [reagent.core :as r]
            [cljs.spec :as s]
            [cljs.core.async :as a]
            [cljsnes.state :as state]
            [cljsnes.spec :as spec]
            [cljsnes.cpu :as cpu]
            [cljsnes.emulator :as emulator]
            [cljsnes.assembler :as assembler]
            [cljsnes.cartridge :as cartridge]
            [cljsnes.memory :as memory]
            [cljsnes.opcodes :as opcodes])
  (:require-macros [cljs.core.async.macros :refer [go]]))

(enable-console-print!)

(defonce lda-example (r/atom
                      "LDA #$01 ; Immediate
LDA $a0000 ; Absolute
LDA $b0 ; Zero Page
LDA $c0,X ; Zero Page, X
LDA $d0,Y ; Zero Page, Y
LDA $e000,X ; Absolute X
LDA $f000,Y ; Absolute Y
LDA $(7000) ; Indirect
LDA $(7000,X) ; Indexed Indirect, X
LDA $(7000,Y) ; Indexed Indirect, Y"))

(defn assembly-container []
  [:textarea {:rows 10 :cols 60 :defaultValue @lda-example}])

(defn assembled-code []
  [:div [:p (str (assembler/assemble @lda-example))]])

(defn get-instruction [rom]
  (let [opcode (-> rom
                   :rom-bank-bytes
                   first
                   first)]
    (get opcodes/ops opcode)))


(defn opcodes-valid? []
  [:div
   [:p (str (s/valid? :opcode/ops opcodes/ops))]])

(defn fd []
  #_[:div [:p (str (cartridge/parse-headers (cartridge/read-file "/Users/angusiguess/Downloads/Super Mario Bros. (Japan, USA).nes")))]
   [:p (count (:vrom-bank-bytes (cartridge/parse-headers (cartridge/read-file "/Users/angusiguess/Downloads/Super Mario Bros. (Japan, USA).nes"))))]])

(defn init []
  (emulator/init "/Users/angusiguess/Downloads/Super Mario Bros. (Japan, USA).nes")
  (r/render-component [fd]
                      (.getElementById js/document "container"))
  (js/console.log "Starting Application"))
