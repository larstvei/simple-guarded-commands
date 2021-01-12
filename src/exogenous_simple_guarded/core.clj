(ns exogenous-simple-guarded.core
  (:require [clojure.java.io :as io]
            [exogenous-simple-guarded.analysis :as analysis]
            [exogenous-simple-guarded.parser :as parser]
            [exogenous-simple-guarded.runtime :as runtime]
            [exogenous.core :as exo]))

(def program1 (slurp (io/resource "program1.smpl")))

(defn simulate
  ([program-str] (simulate program-str {}))
  ([program-str options]
   (let [init-state (or (:init-state options) {})
         ast (or (:ast options) (parser/parse program-str))
         threads (zipmap (range) (map rest ast))
         thread-pool (runtime/threads->thread-pool init-state threads)
         replay (filterv #(= :schedule (:type %)) (:trace options))
         recorded (analysis/enable-disable-events {} thread-pool [])]
     (runtime/exec init-state thread-pool replay recorded))))

(defn exo-sim [program-str]
  (let [ast (parser/parse program-str)]
    (fn [seed-trace]
      (let [[state trace status] (simulate program-str {:trace seed-trace :ast ast})
            mhb (analysis/mhb trace)
            interference (analysis/interference trace)
            out-trace (mapv analysis/abstract-event trace)]
        {:trace out-trace :mhb mhb :interference interference}))))

(defn exo-explore
  ([program-str]
   (exo-explore program-str {}))
  ([program-str options]
   (exo/informed-explore (exo-sim program-str) options)))
