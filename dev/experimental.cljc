(ns experimental
  (:require #?(:clj  [instaparse.core :as insta :refer [defparser]]
               :cljs [instaparse.core :as insta :refer-macros [defparser]])
            #?(:clj [clojure.java.io :as io]
               :cljs [shadow.resource :as rc]))
  #?(:cljs (:require-macros dots.parser)))

#?(:clj
   (defn- read-resource [path]
     (slurp (io/resource path)))

   :cljs
   (defmacro read-resource [path]
     (rc/inline path)))

(defparser dots
  (read-resource "dots/dots.ebnf")
  :optimize :memory)

(defn parse [s]
  (insta/parse dots s))

(comment
  #?(:clj
     (tap> (parse (slurp "examples/vscode.d.ts")))))
