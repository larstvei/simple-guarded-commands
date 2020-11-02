(ns exogenous-simple-guarded.parser
  (:require [clojure.java.io :as io]
            [instaparse.core :as insta]))

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
