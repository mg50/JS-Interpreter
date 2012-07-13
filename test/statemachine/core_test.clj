(ns statemachine.core-test
  (:use statemachine.core clojure.test))

(testing "kind"
  (deftest identify-functions
    (is (= (kind identity) (kind map) :function)))
  (deftest identify-regexes
    (is (= (kind #"asdfa") (kind #"(.+)$") :regex)))
  (deftest identify-default
    (is (= (kind 1) (kind "string") (kind :symbol) :default))))

(testing "transition-matches?"
  (testing "functions"
    (deftest function-1
      (is (transition-matches? 3 #(= 3 %))))
    (deftest function-2
      (is (not (transition-matches? 3 #(= 5 %)))))
    (deftest function-3
      (is (= (transition-matches? [1 2 3] #(> 2 (count %)))))))

  (testing "regexes"
    (deftest regex-1
      (is (transition-matches? "dabc" #"abc")))
    (deftest regex-2
      (is (transition-matches? "fgggd" #"(a|g){3}d$")))
    (deftest regex-3
      (is (not (transition-matches? "fgggda" #"(a|g){3}d$")))))

  (testing "default"
    (deftest default-1
      (is (transition-matches? "xyz" "xyz")))
    (deftest default-2
      (is (transition-matches? 123 123)))
    (deftest default-3
      (is (transition-matches? [1 2] [1 2])))
    (deftest default-4
      (is (not (transition-matches? "fds" "abc"))))
    (deftest default-5
      (is (not (transition-matches? 333 "hello"))))
    (deftest default-6
      (is (not (transition-matches? [3 3 2] 332))))
    (deftest default-7
      (is (transition-matches? [1 2 3] '(1 2 3))))))

(comment)

(deftransition t1 {:a 1 :b 2}
  77 :x
  "hello" :y
  #(and (coll? %) ( > (count %) 2)) :z
  #"a*" :w)
(deftransition t2 {:a 1 :b 2}
  77 inc
  "hello" identity
  inc)

(testing "deftransition"
  (deftest deftransition-returns-function
    (is (fn? t1)))
  (deftest transition-has-metadata
    (is (= {:a 1 :b 2} (meta t1))))
  (deftest transition-returns-correct-value
    (is (= :x (t1 77)))
    (is (= :y (t1 "hello")))
    (is (= :z (t1 [1 2 3])))
    (is (= :w (t1 "aaaa"))))
  (deftest odd-numbered-transitions-have-fallthrough-function
    (is (= inc (t2 77)))
    (is (= identity (t2 "hello")))
    (is (= 49 (t2 48)))))

(testing "declare-all"
  (deftest declare-all-1
    (declare-all a b c)
    (doseq [v [(var a) (var b) (var c)]]
      (is (not (bound? v))))))


(defstatemachine st
  (one {}
       "hello" two
       "goodbye" three
       #"asdf" one)
  (two {}
       coll? one
       three)
  (three {}
         30 one
         one))

(testing "defstatemachine"
  (testing "state one"
    (deftest one-1
      (is (= (one "hello") two)))
    (deftest one-2
      (is (= (one "goodbye") three)))
    (deftest one-3
      (is (= (one "basdf") one))))
  (testing "state two"
    (deftest two-1
      (is (= (two []) one)))
    (deftest two-2
      (is (= (two '(4 3 2)) one)))
    (deftest two-3
      (is (= (two 30) one))))
  (testing "state three"
    (deftest three-1
      (is (= (three 30) one)))
    (deftest three-2
      (is (= (three "hello") two)))
    (deftest three-3
      (is (= (three "goodbye") three)))))
