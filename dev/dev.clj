(ns dev
  (:require [dots.parser :as parser]
            [portal.api :as portal]))

(defonce portal (portal/open))
(add-tap #'portal/submit)

(defn parse [f]
  (parser/parse (slurp f)))

(comment
  (require :reload 'dots.parser)
  (tap> (parse "examples/vscode.d.ts"))
  )
