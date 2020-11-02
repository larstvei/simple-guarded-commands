(ns exogenous-simple-guarded.runtime
  (:require [clojure.core.match :refer [match]]
            [exogenous-simple-guarded.analysis :refer [read-write-sets]]))

(defn eval-exp [state exp]
  (match exp
    [:exp e1 op e2] (op (eval-exp state e1)
                        (eval-exp state e2))
    [:exp [:paren e]] (eval-exp state e)
    [:exp [:num n]] (Integer/parseInt n)
    [:exp (x :guard keyword?)] (or (state x) 0)))

(defn step [state stmt]
  (match stmt
    [:assign v e] [(assoc state v (eval-exp state e)) []]
    [:if e s] [state (if (eval-exp state e) s [])]
    [:if e s1 s2] [state (if (eval-exp state e) s1 s2)]
    [:while e s] [state (if (eval-exp state e) (conj (vec s) stmt) [])]
    _ [state []]))

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

(defn next-seq [t recorded]
  (count (filter (comp (partial = t) :thread) recorded)))

(defn exec [state thread-pool replay recorded]
  (if-let [status (terminated-with-status state thread-pool)]
    [state recorded status]
    (let [t (schedule state thread-pool replay)
          stmt (first (skip-await (thread-pool t)))
          read-writes (read-write-sets stmt)
          [new-state new-stmts] (step state stmt)
          new-thread-pool (update-thread-pool state thread-pool t new-stmts)
          new-replay (rest replay)
          new-recorded (conj recorded (-> read-writes (assoc :thread t)
                                          (assoc :seq (next-seq t recorded))))]
      (recur new-state new-thread-pool new-replay new-recorded))))
