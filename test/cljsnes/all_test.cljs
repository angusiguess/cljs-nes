(ns cljsnes.all-test
  (:require [cljs.test :refer [deftest is testing run-tests]]
            [cljs.spec :as s]
            [cljs.pprint :as pprint]
            [cljs.spec.test :as stest]
            [cljsnes.byte :as byte]))

(enable-console-print!)

(defn summarize-results' [spec-check]
  (map (comp #(pprint/write % :stream nil) stest/abbrev-result) spec-check))

(defn check' [spec-check]
  (is (empty? (->> spec-check
                   (filter :failure))) (summarize-results' spec-check)))


(deftest byte-arithmetic
  (check' (stest/check [`byte/add
                        `byte/inc
                        `byte/neg-byte?
                        `byte/asl
                        `byte/lsr
                        `byte/l-and
                        `byte/l-or
                        `byte/l-xor])))

(run-tests)
