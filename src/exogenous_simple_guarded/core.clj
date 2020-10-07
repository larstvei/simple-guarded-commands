(ns exogenous-simple-guarded.core
  (:require [instaparse.core :as insta]
            [clojure.core.match :refer [match]]
            [clojure.java.io :as io]
            [clojure.set :as s]))

(def simple-guarded-parser
  (-> (io/resource "simple-guarded.grammar")
      (insta/parser :auto-whitespace :standard)))

(def ->built-in (comp resolve symbol))

(def transformations
  {:op ->built-in
   :rel ->built-in
   :var keyword
   :block vector})

(defn parse [input]
  (->> (insta/parse simple-guarded-parser input)
       (insta/transform transformations)))

(defn eval-exp [state exp]
  (match exp
    [:exp e1 op e2] (op (eval-exp state e1)
                        (eval-exp state e2))
    [:exp [:paren e]] (eval-exp state e)
    [:exp [:num n]] (Integer/parseInt n)
    [:exp (x :guard keyword?)] (or (state x) 0)))

(defn read-write-sets [stmt]
  (match stmt
    [:assign v e] (-> (read-write-sets e)
                      (update :writes conj v))
    [:if e _] (read-write-sets e)
    [:if e _ _] (read-write-sets e)
    [:while e _] (read-write-sets e)
    [:exp e1 op e2] (merge-with s/union
                                (read-write-sets e1)
                                (read-write-sets e2))
    [:exp [:paren e]] (read-write-sets e)
    [:exp (x :guard keyword?)] {:reads #{x} :writes #{}}
    :else {:reads #{} :writes #{}}))

(defn unblocked? [state [t stmt]]
  (match (first stmt)
    [:await e] (when (eval-exp state e) t)
    :else t))

(defn schedule [state thread-pool [event & replay]]
  (let [unblocked (keep (partial unblocked? state) thread-pool)]
    (if-let [t (:thread event)]
      (do (assert ((set unblocked) t))
          t)
      (rand-nth unblocked))))

(defn step [state stmt]
  (match stmt
    [:assign v e] [(assoc state v (eval-exp state e)) []]
    [:if e b] [state (if (eval-exp state e) b [])]
    [:if e b1 b2] [state (if (eval-exp state e) b1 b2)]
    _ [state []]))

(defn skip-await [stmt]
  (match (first stmt)
    [:await e] (rest stmt)
    :else stmt))

(defn update-thread-pool [state thread-pool t stmts]
  (let [thread (rest (skip-await (thread-pool t)))
        new-thread (into [] (concat stmts thread))]
    (if (empty? new-thread)
      (dissoc thread-pool t)
      (assoc thread-pool t new-thread))))

(defn terminated-with-status [state thread-pool]
  (cond (empty? thread-pool) :success
        (every? (complement (partial unblocked? state)) thread-pool) :deadlock))

(defn exec [state thread-pool replay recorded]
  (if-let [status (terminated-with-status state thread-pool)]
    [state recorded status]
    (let [t (schedule state thread-pool replay)
          stmt (first (skip-await (thread-pool t)))
          read-writes (read-write-sets stmt)
          [new-state new-stmts] (step state stmt)
          new-thread-pool (update-thread-pool state thread-pool t new-stmts)
          new-replay (rest replay)
          new-recorded (conj recorded (assoc read-writes :thread t))]
      (recur new-state new-thread-pool new-replay new-recorded))))

(defn simulate
  ([program] (simulate program {}))
  ([program options]
   (let [init-state (or (:init-state options) {})
         thread-pool (zipmap (range) (map rest program))
         replay (:trace options)
         recorded []]
     (exec init-state thread-pool replay recorded))))
