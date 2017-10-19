(ns tact.util
  (:require [clojure.java.io :as io]))

(defn get-project-dirs
  [project]
  (let [paths (concat (:source-paths project)
                      (:test-paths project))]
    (if (seq paths)
      (do (println paths)
          (or (->> paths
                   (map io/file)
                   (filter #(and (.exists %) (.isDirectory %)))
                   (seq))
              (println "No paths resolved to existing files")))
      (println "Both :source-paths and :test-paths are empty in project map"))))

(defn relative-path
  [dir file]
  (-> (.toURI dir)
      (.relativize (.toURI file))
      (.getPath)))


(defn grep
  [re dir]
  (filter #(re-find re (relative-path dir %))
          (file-seq (io/file dir))))


(defn file-pattern
  [project]
  (get-in project [:tact :file-pattern] #"\.clj[sx]?$"))


(defn find-files
  [project path]
  (let [f (io/file path)]
    (when-not (.exists f) (println "No such file:" (str f)))
    (if (.isDirectory f)
      (grep (file-pattern project) f)
      [f])))
