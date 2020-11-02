(ns exogenous-simple-guarded.core
  (:require [exogenous-simple-guarded.parser :refer [parse]]
            [exogenous-simple-guarded.runtime :refer [exec]]))

(defn simulate
  ([program-str] (simulate program-str {}))
  ([program-str options]
   (let [init-state (or (:init-state options) {})
         ast (parse program-str)
         thread-pool (zipmap (range) (map rest ast))
         replay (:trace options)
         recorded []]
     (exec init-state thread-pool replay recorded))))
