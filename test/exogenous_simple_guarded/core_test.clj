(ns exogenous-simple-guarded.core-test
  (:require [clojure.test :refer :all]
            [clojure.java.io :as io]
            [exogenous-simple-guarded.analysis :as analysis]
            [exogenous-simple-guarded.core :as lang]
            [exogenous-simple-guarded.parser :as parser]))

(def program1 (slurp (io/resource "program1.smpl")))
(def program2 (slurp (io/resource "program2.smpl")))

(deftest parsing
  (is '([:thread
         [:assign :x [:exp [:exp :x] #'clojure.core/+ [:exp [:num "1"]]]]]
        [:thread
         [:assign :x [:exp [:exp :x] #'clojure.core/+ [:exp [:num "1"]]]]]
        [:thread
         [:assign :x [:exp [:exp :x] #'clojure.core/+ [:exp [:num "3"]]]]]
        [:thread
         [:await [:exp [:exp :x] #'clojure.core/= [:exp [:num "3"]]]]
         [:assign :y [:exp [:num "2"]]]])
      (parser/parse program1))

  (is '([:thread
         [:while [:exp [:exp :x] #'clojure.core/< [:exp [:num "5"]]]
          [[:assign :x [:exp [:exp :x] #'clojure.core/+ [:exp [:num "1"]]]]]]])
      (parser/parse program2)))

(deftest execution
  ;; program1
  (let [[state trace status] (lang/simulate program1)]
    (is (= state
           (if (= status :success)
             {:x 5, :y 2}
             {:x 5})))
    (is (= (count trace)
           (if (= status :success)
             4
             3))))

  ;; program2
  (let [[state trace status] (lang/simulate program2)]
    (is (= status :success))
    (is (= (count trace) 11))
    (is (= state {:x 5}))))

(deftest record&replay
  ;; program1
  (let [[state1 trace1 status1] (lang/simulate program1)
        [state2 trace2 status2] (lang/simulate program1 {:trace trace1})]
    (is (= state1 state2))
    (is (= trace1 trace2))
    (is (= status1 status2)))
  ;; program2
  (let [[state1 trace1 status1] (lang/simulate program2)
        [state2 trace2 status2] (lang/simulate program2 {:trace trace1})]
    (is (= state1 state2))
    (is (= trace1 trace2))
    (is (= status1 status2))))
