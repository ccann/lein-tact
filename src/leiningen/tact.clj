(ns leiningen.tact
  (:require [clojure.java.io :as io]
            [clojure.tools.analyzer.jvm :as analyzer]
            [clojure.tools.analyzer.passes.jvm.emit-form :as e]
            [clojure.tools.namespace.parse :as ns.parse]
            [leiningen.core.main :as lein])
  (:import java.io.File))

(defn get-project-paths
  [project]
  (let [paths (concat (:source-paths project)
                      (:test-paths project))]
    (if (seq paths)
      (or (->> paths
               (map io/file)
               (filter #(and (.exists %) (.isDirectory %)))
               (seq))
          (lein/abort "No paths resolved to existing files"))
      (lein/abort "Both :source-paths and :test-paths are empty in project map"))))



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
    (when-not (.exists f) (lein/abort "No such file:" (str f)))
    (if (.isDirectory f)
      (grep (file-pattern project) f)
      [f])))


(defn shame-one
  [project ^File file]
  (println file)
  (let [reader (java.io.PushbackReader. (io/reader file))
        ;; og-ns  (ns.parse/read-ns-decl reader)
        ;; ns-sym (second og-ns)
        ]

    ;; read namespace into edn? or clojure?
    ;; walk the namespace
    ;; analyze each form
    ;; process all locals and var names
    (for [line lines]
      (analyzer/analyze ))


    ()

    #_(env/with-env (analyzer/global-env)
        )


    ))

(defn check
  ([project]
   (if project
     (apply check project (get-project-paths project))
     (lein/abort "No project found")))

  ([project path & paths]
   (let [files   (mapcat #(find-files project %) (cons path paths))
         results (map #(shame-one project %) files)]
     results)))

(defn tact
  [project command & args]
  (case command
    "check" (apply check project args)
    (lein/abort "Unknown tact command:" command)
    )

  )

#_(tact {:source-paths ["/Users/cody/dev/alembic/src/alembic/"]}
      "check")
