(ns dots.util.names
  (:require [camel-snake-kebab.core :as csk]
            [clojure.string :as str])
  #?(:cljs (:require-macros [dots.util.names :refer [cljs-core-macro-names]])))

(defn- collect-cljs-core-public-names [coll]
  (into coll (map (comp name key)) (ns-publics 'cljs.core)))

(defmacro ^:private cljs-core-macro-names []
  (collect-cljs-core-public-names #{}))

(def ^:private cljs-core-public-names
  (collect-cljs-core-public-names (cljs-core-macro-names)))

(defn cljs-core-name? [n]
  (contains? cljs-core-public-names n))

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

(def reserved-names #{"nil"})

(defn munge-name [n]
  (if (reserved-names n)
    (str (n "*"))
    n))

(defn strip-getter-prefix [n]
  (if-let [[_ n*] (re-matches #"(?:get|is)-(.+)" n)]
    n*
    n))
