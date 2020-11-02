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
