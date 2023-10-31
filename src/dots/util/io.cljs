(ns dots.util.io
  (:require [dots.node.fs :as fs])
  (:require-macros dots.util.io))

(defprotocol IClosable
  (-close [this]))

(defn file-writer [path]
  (let [fd (fs/open-sync path "w")]
    (reify
      IWriter
      (-write [_ s]
        (fs/write-sync fd s))
      (-flush [_]
        (fs/fdatasync-sync fd))
      IClosable
      (-close [_]
        (fs/close-sync fd)))))
