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
         thread-pool (zipmap (range) (map rest ast))
         replay (filterv #(= :schedule (:type %))(:trace options))
         recorded (->> (keys thread-pool)
                       (map (fn [id] {:type :spawn :thread id}))
                       (into []))]
     (runtime/exec init-state thread-pool replay recorded))))

(defn exo-sim [program-str]
  (let [ast (parser/parse program-str)]
    (fn [seed-trace]
      (prn seed-trace)
      (prn)
      (let [[state trace status] (simulate program-str {:trace seed-trace :ast ast})
            mhb (analysis/mhb trace)
            interference (analysis/interference trace)]
        {:trace trace :mhb mhb :interference interference}))))

(defn exo-explore
  ([program-str]
   (exo-explore program-str {}))
  ([program-str options]
   (exo/informed-explore (exo-sim program-str) options)))
