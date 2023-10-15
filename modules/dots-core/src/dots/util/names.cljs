(ns dots.util.names
  (:require [camel-snake-kebab.core :as csk]
            [clojure.string :as str]))

(def ^:private core-names
  (into #{} (map (comp name key)) (ns-publics 'cljs.core)))

(defn core-name? [n]
  (contains? core-names n))

(defn internal? [n]
  (str/starts-with? n "_"))

(defn cljs-name [n]
  {:pre [(string? n)]}
  (-> n
      (str/replace "\"" "")
      (str/replace #"\W+" "_")
      csk/->kebab-case-string
      (cond->
       (str/starts-with? n "_") (->> (str "-"))
       (str/ends-with? n "_") (str "*"))))

(defn split-fqn [fqn]
  (vec (re-seq #"\"[^\"]+\"|\w+" fqn)))
