(ns magnus-demo.prod
  (:require
    [magnus-demo.core :as core]))

;;ignore println statements in prod
(set! *print-fn* (fn [& _]))

(core/init!)
