(ns exogenous-simple-guarded.core
  (:require [clojure.java.io :as io]
            [exogenous-simple-guarded.analysis :as analysis]
            [exogenous-simple-guarded.parser :as parser]
            [exogenous-simple-guarded.runtime :as runtime]
            [exogenous.core :as exo]))

(def program1 (slurp (io/resource "program1.smpl")))
(def program2 (slurp (io/resource "program1.smpl")))
(def program-rudi (slurp (io/resource "program1-rudi-style.smpl")))

(defn syncprn [& args]
  (locking syncprn
    (apply prn args)))

(defn simulate
  ([program-str] (simulate program-str {}))
  ([program-str options]
   (let [init-state (or (:init-state options) {})
         ast (or (:ast options) (parser/parse program-str))
         threads (zipmap (range) (map rest ast))
         thread-pool (runtime/threads->thread-pool init-state threads)
         replay (filterv #(= :schedule (:type %)) (:trace options))
         recorded []
         enabled-history [(analysis/thread-pool->enabled-disabled thread-pool recorded)]]
     (runtime/exec init-state thread-pool replay recorded enabled-history))))

(defn exo-sim [program-str]
  (let [ast (parser/parse program-str)]
    (fn [seed-trace]
      (let [[state trace enabled-disabled]
            (simulate program-str {:trace seed-trace :ast ast})
            events (into (set trace) (mapcat :disabled enabled-disabled))
            mhb (analysis/mhb events)
            interference (analysis/interference events)
            out-enabled-disabled (mapv analysis/abstract-enabled-disabled enabled-disabled)
            out-trace (mapv analysis/abstract-event trace)]
        (syncprn out-trace)
        {:trace out-trace :mhb mhb :interference interference
         :enabled-disabled out-enabled-disabled}))))

(defn exo-explore
  ([program-str]
   (exo-explore program-str {}))
  ([program-str options]
   (exo/informed-explore (exo-sim program-str) options)))
