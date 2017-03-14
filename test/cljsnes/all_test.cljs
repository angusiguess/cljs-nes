(ns cljsnes.all-test
  (:require [cljs.test :refer [deftest is testing run-tests]]
            [cljs.spec :as s]
            [cljs.pprint :as pprint]
            [cljs.spec.test :as stest]
            [cljsnes.memory :as memory]
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

(deftest cpu-stack-test
  (let [memory (memory/make-nrom [[]] [[]] [[]] [[]] false)
        state {:cpu {:s 0xFF}
               :memory memory}]
    (testing "push-8 pop-8"
      (let [pushed-state (cpu/push-8 state 0xBB)]
        (is (= 0xFE (get-in pushed-state [:cpu :s])))
        (is (= 0xBB (first (cpu/pop-8 pushed-state))))
        (is (= 0xFF (-> (cpu/pop-8 pushed-state)
                        last
                        (get-in [:cpu :s]))))))
    (testing "push-16 pop-16"
      (let [pushed-state (cpu/push-16 state 0xBBCC)]
        (is (= 0xFD (get-in pushed-state [:cpu :s])))
        (is (= 0xBBCC (first (cpu/pop-16 pushed-state))))
        (is (= 0xFF (-> (cpu/pop-16 pushed-state)
                        last
                        (get-in [:cpu :s]))))))))

(deftest ppu-registers-shared
  (let [memory (memory/make-nrom [[]] [[]] [[]] [[]] false)]
    (testing "Shared registers"
      (doseq [address (range 0x2000 0x2008)]
        (is (= 0xBB (-> memory
                        (memory/cpu-write address 0xBB)
                        (memory/ppu-read address))))
        (is (= 0xBB (-> memory
                        (memory/ppu-write address 0xBB)
                        (memory/cpu-read address))))))))

(deftest status-read
  (is (cpu/ppu-status-read? {:resolved-address 0x2002}))
  (is (false? (cpu/ppu-status-read? {}))))

(deftest control-write
  (let [state {:memory (memory/make-nrom [[]] [[]] [[]] [[]] false)
               :ppu {:t 0}}]
    (is (cpu/ppu-control-write? {:resolved-address 0x2000}))
    (is (= 3072 (get-in (ppu/write-control state 0xFF) [:ppu :t])))))

(deftest ppu-read-status
  (let [state {:memory (memory/make-nrom [[]] [[]] [[]] [[]] false)
               :ppu {:write-address-low 0xBB
                     :write-address-high 0xCC
                     :w true}}
        after-read (ppu/read-status state)]
    (is (= false (get-in after-read [:ppu :w])))))

(deftest ppu-write-scroll
  (let [state {:memory (memory/make-nrom [[]] [[]] [[]] [[]] false)
               :ppu {:t 0
                     :x 0
                     :w false}}
        first-write (ppu/write-register-scroll state 0xFF)
        second-write (ppu/write-register-scroll first-write 0xFF)]
    (is (get-in first-write [:ppu :w]))
    (is (= 0x07 (get-in first-write [:ppu :x])))
    (is (= 31 (get-in first-write [:ppu :t])))
    (is (not (get-in second-write [:ppu :w])))
    (is (= 2r111001111111111 (get-in second-write [:ppu :t])))))

(deftest ppu-write-address
  (let [state {:memory (memory/make-nrom [[]] [[]] [[]] [[]] false)
               :ppu {:t 0
                     :x 0
                     :w false}}
        first-write (ppu/write-register-address state 0xFF)
        second-write (ppu/write-register-address first-write 0xFF)]
    (is (get-in first-write [:ppu :w]))
    (is (= 2r011111100000000 (get-in first-write [:ppu :t])))
    (is (not (get-in second-write [:ppu :w])))
    (is (= 2r011111111111111 (get-in second-write [:ppu :t])))
    (is (= 2r011111111111111 (get-in second-write [:ppu :v])))))

(run-tests)
