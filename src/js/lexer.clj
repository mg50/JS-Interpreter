(ns js.lexer
  (:require [statemachine.core :as st]))

(defn get-delim-string [chars]
  (let [delim (first chars)]
    (loop [string [delim]
           remaining (rest chars)
           escaped? false]
      (let [c (first remaining)
            rest (rest remaining)]
        (cond
         (nil? c) nil
         (and (not escaped?)
              (= delim c)) (conj string c)
         (= c \\) (recur (conj string c) rest true)
         :else (recur (conj string c) rest false))))))

(defn strip-left [chars]
  (drop-while #(re-find #"\s" (str %)) chars))

(defn kind [chars]
  (let [c (-> chars first str)]
    (cond
     (re-matches #"[0-9]" c) :number
     (re-matches #"[A-Za-z$_]" c) :identifier
     (re-find #"['\"]" c) :quote
     :else :punctuation)))

(defmulti next-token kind)
(defmethod next-token :punctuation [chars]
            (first chars))
(defmethod next-token :quote [chars]
            (get-delim-string chars))
(defmethod next-token :number [chars]
  (take-while #(re-matches #"\d" (str %)) chars))
(defmethod next-token :identifier [chars]
  (take-while #(re-matches #"[A-Za-z0-9$_]" (str %)) chars))

(defn lex [chars]
  (loop [tokens []
         remaining chars]
    (let [remaining (strip-left remaining)]
      (if (empty? remaining)
        tokens
        (let [token (apply str (next-token remaining))]
          (recur (conj tokens token)
                 (drop (count token) remaining)))))))
