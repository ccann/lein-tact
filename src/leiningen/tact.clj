(ns leiningen.tact
  "Summary:Keep me under 68 chars"
  (:require [clojure.java.io :as io]
            [leiningen.core
             [eval :as lein.core.eval]
             [main :as lein.main]]))

(defn tact
  [project]
  (let [proj (update project :dependencies conj ['lein-tact "0.1.0-SNAPSHOT"])]
    (lein.core.eval/eval-in-project
     proj
     `(if (tact.core/have-tact '~proj)
        (System/exit 0)
        (System/exit -1))
     '(require 'tact.core))))


#_(defn tact
  [project command & args]
  {:subtasks [#'check]}
  (case command
    "check" (apply check project args)
    nil     :not-implemented-yet
    (lein.main/abort "Unknown tact command:" command)))
