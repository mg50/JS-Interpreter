(ns statemachine.core)

(defn kind [matcher]
  (cond
   (fn? matcher) :function
   (= java.util.regex.Pattern (class matcher)) :regex
   :else :default))

(defmulti transition-matches? (fn [val matcher] (kind matcher)))
(defmethod transition-matches? :function [val matcher]
  (matcher val))
(defmethod transition-matches? :regex [val matcher]
  (re-find matcher (str val)))
(defmethod transition-matches? :default [val matcher]
  (= val matcher))

(defmacro deftransition [name metadata & body]
  (let [s (gensym)
        body-with-matchers (map-indexed (fn [idx form]
                                          (if (even? idx)
                                            `(transition-matches? ~s ~form)
                                            form))
                                        body)
        cond-body (if (even? (count body))
                    body-with-matchers
                    (concat (butlast body-with-matchers) [:else (last body-with-matchers)]))]
    `(def ~name (with-meta (fn [~s]
                             (cond
                                 ~@cond-body))
                  ~metadata))))

(defmacro declare-all [& syms]
  `(do
     ~@(map #(list 'declare %) syms)))

(defmacro defstatemachine [name & forms]
  (let [state-names (map first forms)]
    `(do
       (declare-all ~@state-names)
       ~@(map (fn [[name metadata & rest]]
                `(deftransition ~name ~metadata ~@rest))
              forms)
              (def ~name ~(ffirst forms)))))

(defn process
  ([initial-state coll]
     (process initial-state coll identity))
  ([initial-state coll transform]
     (let [s (seq coll)]
       (loop [state initial-state
              processed []
              remainder s]
         (if remainder
           (let [new-state (state (first remainder))]
             (recur new-state
                    (conj processed (transform new-state))
                    (next remainder)))
           processed)))))
