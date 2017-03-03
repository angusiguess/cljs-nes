(ns cljsnes.all-test
  (:require [cljs.test :refer [deftest is testing run-tests]]
            [cljs.spec :as s]
            [cljs.pprint :as pprint]
            [cljs.spec.test :as stest]
            [cljsnes.arith :as arith]
            [cljsnes.opcodes :as opcodes]
            [cljsnes.ppu :as ppu]
            [cljsnes.cpu :as cpu]))

(enable-console-print!)

(defn summarize-results' [spec-check]
  (map (comp #(pprint/write % :stream nil) stest/abbrev-result) spec-check))

(defn check' [spec-check]
  (is (empty? (->> spec-check
                   (filter :failure))) (summarize-results' spec-check)))


(deftest byte-arithmetic
  (check' (stest/check [`arith/add
                        `arith/inc
                        `arith/neg-byte?
                        `arith/asl
                        `arith/lsr
                        `arith/l-and
                        `arith/l-or
                        `arith/l-xor
                        `arith/make-address])))

(deftest cpu-ops
  (check' (stest/check [#_`cpu/push-8
                        #_`cpu/pop-8
                        #_`cpu/push-16
                        #_`cpu/status->byte
                        `cpu/byte->status])))

(deftest opcodes-conform
  (is (nil? (s/explain :opcode/ops opcodes/ops))))

(deftest ppu-ticks-cycle
  (testing "cycle increment"
   (let [state (ppu/step {:ppu {:cycle 0 :line 0}})]
     (is (= 1 (get-in state [:ppu :cycle]))))
   (testing "cycle wrap"
     (let [state (ppu/step {:ppu {:cycle 340 :line 10}})]
       (is (= 0 (get-in state [:ppu :cycle])))
       (is (= 11 (get-in state [:ppu :line])))))
   (testing "cycle-and-line-wrap"
     (let [state (ppu/step {:ppu {:cycle 340 :line 261}})]
       (is (= 0 (get-in state [:ppu :cycle]))
           (= 0 (get-in state [:ppu :line])))))))


(run-tests)
