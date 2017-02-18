(ns cljsnes.assembler
  (:require [clojure.string :as str]
            [clojure.spec :as s]))

(s/def ::opcode #{"LDA"})

(defn drop-comments [s]
  (take-while (fn [x] (not= ";" x)) s))

(defn assemble [s]
  (let [lines (->> s
                  str/split-lines
                  (map (fn [s] (str/split s #"\s+"))))]
    (map drop-comments lines)))
