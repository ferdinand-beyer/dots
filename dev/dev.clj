(ns dev
  (:require [portal.api :as portal]))

(defonce portal (portal/open))
(add-tap #'portal/submit)
