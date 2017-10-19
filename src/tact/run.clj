(ns tact.run)

(defn run
  [project paths]
  (require 'tact.core)
  ((resolve 'tact.core/have-tact) project paths))
