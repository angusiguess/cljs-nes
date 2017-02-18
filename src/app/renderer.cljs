(ns app.renderer
  (:require [reagent.core :as r]
            [cljs.spec :as s]
            [cljs.core.async :as a]
            [cljsnes.cpu :as cpu]
            [cljsnes.emulator :as emulator]
            [cljsnes.assembler :as assembler]
            [cljsnes.cartridge :as cartridge]
            [cljsnes.memory :as memory]
            [cljsnes.opcodes :as opcodes])
  (:require-macros [cljs.core.async.macros :refer [go]]))

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

(defn cpu-state-component []
  [:div
   [:p
    (str (dissoc cpu/init-state :mem))]
   (assembly-container)
   (assembled-code)])

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
  [:div [:p (str (cartridge/parse-headers (cartridge/read-file "/Users/angusiguess/Downloads/Super Mario Bros. (Japan, USA).nes")))]
   [:p (- 0x401F 0x4018)]
   [:p 2r00000000]
   [:p (str (assoc [0 1 2] 1 2))]])

(defn emulator-test []
  [:p (str (cpu/exec @cpu/cpu-state))])

(defn init []
  (emulator/init "/Users/angusiguess/Downloads/Super Mario Bros. (Japan, USA).nes")
  (r/render-component [emulator-test]
                      (.getElementById js/document "container"))
  (js/console.log "Starting Application"))
