(ns exogenous-simple-guarded.analysis
  (:require [clojure.core.match :refer [match]]
            [clojure.set :as s]))

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

(defn next-seq [t recorded]
  (count (filter (fn [{:keys [thread type]}]
                   (and (= t thread) (= type :schedule)))
                 recorded)))

(defn make-event [type read-writes recorded t]
  (-> read-writes
      (assoc :type type)
      (assoc :thread t)
      (assoc :seq (next-seq t recorded))))

(defn thread->schedule-event [thread-pool recorded t]
  (let [rw (match (first ((:disabled thread-pool) t))
             [:await e] (read-write-sets e)
             :else {})]
    (make-event :schedule rw recorded t)))

(defn thread-pool->enabled-disabled [thread-pool recorded]
  (let [t->e (partial thread->schedule-event thread-pool recorded)]
    (-> thread-pool
        (update :enabled (comp set keys))
        (update :enabled #(set (map t->e %)))
        (update :disabled (comp set keys))
        (update :disabled #(set (map t->e %))))))

(defn abstract-event [e]
  (select-keys e [:type :thread :seq]))

(defn abstract-enabled-disabled [{:keys [enabled disabled]}]
  {:enabled (set (map abstract-event enabled))
   :disabled (set (map abstract-event disabled))})

(defn mhb [trace]
  (set (for [{t1 :thread s1 :seq :as e1} trace
             {t2 :thread s2 :seq :as e2} trace
             :when (and (= t1 t2) (< s1 s2))]
         [(abstract-event e1) (abstract-event e2)])))

(defn interference [events]
  (set (for [{t1 :thread r1 :reads w1 :writes :as e1} events
             {t2 :thread r2 :reads w2 :writes :as e2} events
             :when (and (not= t1 t2)
                        (or (not (empty? (s/intersection w1 (s/union r2 w2))))
                            (not (empty? (s/intersection w2 (s/union r1 w1))))))]
         [(abstract-event e1) (abstract-event e2)])))

(comment
  ;; These functions allow us to flatten an abstract syntax tree, which
  ;; simplifies the processes of statically determining interference. However,
  ;; it seems easier to just decide must-happen-before and interference
  ;; relationships on the trace directly.
  ;;
  ;; This has to do with event identities. We cannot determine what event
  ;; identities will be generated by a program statically; that is not to say
  ;; that we can't statically provide a scheme that dictates what the names of
  ;; the events. Consider a while loop; each iteration of a while loop may lead
  ;; to reads and writes, all of which must be identified. If we name the while
  ;; loop, can say that each iteration of the while loop is named by the name
  ;; of the while loop /and/ a number, which would keep things unique.
  ;;
  ;; But all things considered, it seems easier to simply determine the
  ;; relationships on the generated trace, and not worse in any (currently)
  ;; apparent way.

  (defn flatten-stmts [stmts]
    (match stmts
      [[:if e s] & ss] (concat [e] (mapcat flatten-stmts [s ss]))
      [[:if e s1 s2] & ss] (concat [e] (mapcat flatten-stmts [s1 s2 ss]))
      [[:while e s] & ss] (concat [e] (mapcat flatten-stmts [s ss]))
      [stmt & ss] (concat [stmt] (flatten-stmts ss))
      [] []))

  (defn flatten-ast [ast]
    (for [thread ast]
      (vec (flatten-stmts thread)))))
