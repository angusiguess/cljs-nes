(ns cljsnes.all-test
  (:require [cljs.test :refer [deftest is testing run-tests]]
            [cljs.spec :as s]
            [cljs.pprint :as pprint]
            [cljs.spec.test :as stest]
            [cljsnes.cpu :as cpu]))

(enable-console-print!)

(defn summarize-results' [spec-check]
  (map (comp #(pprint/write % :stream nil) stest/abbrev-result) spec-check))

(defn check' [spec-check]
  (is (empty? (->> spec-check
                   (filter :failure))) (summarize-results' spec-check)))

(deftest a-binary-test
  (is (bit-test 256 8)))

(deftest addition
  (check' (stest/check [`cljsnes.cpu/add
                        `cljsnes.cpu/inc
                        `cljsnes.cpu/neg-byte?
                        `cljsnes.cpu/asl
                        `cljsnes.cpu/lsr
                        `cljsnes.cpu/l-and
                        `cljsnes.cpu/l-or
                        `cljsnes.cpu/l-xor])))

(run-tests)
