(ns dots.node.path
  (:require ["path" :as path]))

(defn join [& paths]
  (apply path/join paths))

(defn dirname [path]
  (path/dirname path))
