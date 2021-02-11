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
  (let [enabled (keys (:enabled thread-pool))]
    (if-let [t (:thread event)]
      (do (assert ((set enabled) t))
          t)
      (rand-nth enabled))))

(defn skip-await [stmt]
  (match (first stmt)
    [:await e] (rest stmt)
    :else stmt))

(defn threads->thread-pool [state threads]
  (let [pred (partial unblocked? state)]
    {:enabled (into {} (filter pred threads))
     :disabled (into {} (filter (complement pred) threads))}))

(defn update-thread-pool [state {:keys [enabled disabled]} t stmts]
  (let [thread (rest (skip-await (enabled t)))
        new-thread (into [] (concat stmts thread))
        threads (merge (dissoc enabled t) disabled)]
    (if (empty? new-thread)
      (threads->thread-pool state threads)
      (threads->thread-pool state (assoc threads t new-thread)))))

(defn terminated-with-status [state {:keys [enabled disabled]}]
  (cond (and (empty? enabled) (empty? disabled)) :success
        (and (empty? enabled) (not (empty? disabled))) :deadlock))

(defn exec [state thread-pool replay recorded enabled-disabled]
  (if-let [status (terminated-with-status state thread-pool)]
    [state recorded enabled-disabled status]
    (let [t (schedule state thread-pool replay)
          stmt (first (skip-await ((:enabled thread-pool) t)))
          read-writes (analysis/read-write-sets stmt)
          [new-state new-stmts] (step state stmt)
          new-thread-pool (update-thread-pool new-state thread-pool t new-stmts)
          new-replay (rest replay)
          new-event (analysis/make-event :schedule read-writes recorded t)
          new-recorded (conj recorded new-event)
          new-enabled-disabled (->> (analysis/thread-pool->enabled-disabled new-thread-pool new-recorded)
                                    (conj enabled-disabled))]
      (recur new-state new-thread-pool new-replay new-recorded new-enabled-disabled))))
