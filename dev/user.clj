(ns user)

(try
  (require 'dev)
  (catch Throwable _
    (println "Warning: Failed to load 'dev' namespace")))
