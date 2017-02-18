(ns cljsnes.all-test
  (:require [cljs.test :refer [deftest is testing run-tests]]
            [cljs.spec :as s]
            [cljs.pprint :as pprint]
            [cljs.spec.test :as stest]
            [cljsnes.opcodes :as opcodes]
            [cljsnes.arith :as arith]))

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

(deftest opcodes-conform
    (is (nil? (s/explain :opcode/ops opcodes/ops))))


(run-tests)
