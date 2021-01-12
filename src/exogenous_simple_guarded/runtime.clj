(ns exogenous-simple-guarded.runtime
  (:require [clojure.core.match :refer [match]]
            [exogenous-simple-guarded.analysis :as analysis]))

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

(defn threads->thread-pool [state threads]
  (let [pred (partial unblocked? state)]
    {:ready (into {} (filter pred threads))
     :blocked (into {} (filter (complement pred) threads))}))

(defn update-thread-pool [state {:keys [ready blocked]} t stmts]
  (let [thread (rest (skip-await (ready t)))
        new-thread (into [] (concat stmts thread))
        threads (merge (dissoc ready t) blocked)]
    (if (empty? new-thread)
      (threads->thread-pool state threads)
      (threads->thread-pool state (conj threads new-thread)))))

(defn terminated-with-status [state {:keys [ready blocked]}]
  (cond (and (empty? ready) (empty? blocked)) :success
        (and (empty? ready) (not (empty? blocked))) :deadlock))

(defn exec [state thread-pool replay recorded]
  (if-let [status (terminated-with-status state thread-pool)]
    [state recorded status]
    (let [t (schedule state (:ready thread-pool) replay)
          stmt (first (skip-await ((:ready thread-pool) t)))
          read-writes (analysis/read-write-sets stmt)
          [new-state new-stmts] (step state stmt)
          new-thread-pool (update-thread-pool new-state thread-pool t new-stmts)
          new-replay (rest replay)
          new-event (-> read-writes
                        (assoc :type :schedule)
                        (assoc :thread t)
                        (assoc :seq (analysis/next-seq t recorded)))
          new-recorded (apply conj recorded new-event
                              (analysis/enable-disable-events
                               thread-pool new-thread-pool recorded))]
      (recur new-state new-thread-pool new-replay new-recorded))))
