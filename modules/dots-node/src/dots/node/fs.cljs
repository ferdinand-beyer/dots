(ns dots.node.fs
  (:require ["fs" :as fs]))

(defn open-sync
  ([path]
   (fs/openSync path))
  ([path flags]
   (fs/openSync path flags))
  ([path flags mode]
   (fs/openSync path flags mode)))

(defn close-sync [fd]
  (fs/closeSync fd))

(defn write-sync
  {:arglists '([fd buffer offset]
               [fd buffer offset length]
               [fd buffer offset length position]
               [fd buffer options]
               [fd string]
               [fd string position]
               [fd string position encoding])}
  [fd buffer & [x y z]]
  (fs/writeSync fd buffer x y z))

(defn fdatasync-sync [fd]
  (fs/fdatasyncSync fd))

(defn mkdir-sync
  {:arglists '([path] [path options])}
  [path & [^js options]]
  (fs/mkdirSync path options))
