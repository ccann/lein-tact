(ns leiningen.tact
  (:require [camel-snake-kebab.core :as case]
            [clojure.java.io :as io]
            [clojure.tools.analyzer.jvm :as analyzer]
            [clojure.tools.reader :as clojure.tools.reader.edn]
            [clojure.walk :as walk]
            [leiningen.core.main :as lein]
            [taoensso.timbre :as timbre]
            [clojure.string :as str]
            [clojure.tools.reader :as tools.reader]
            clojure.tools.reader)
  (:import java.io.File))

(timbre/refer-timbre)

(set! *warn-on-reflection* false)
(set! *print-length* 20)
(set! *print-level* 20)


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





    #_(env/with-env (analyzer/global-env)
        )


    ))
;; :children
;; :bindings vector -> each elem is a map ->

(defn ->kebab-case-sym
  [s]
  (let [kebab (if s (case/->kebab-case s)
                  (case/->kebab-case s))]
    (symbol (if (not= kebab s) kebab s))))

(defn read-all
  [file]
  (let [rdr (-> file io/file io/reader java.io.PushbackReader.)]
    (loop [forms []]
      (let [form (try
                   (clojure.tools.reader/read rdr)
                   (catch Exception e nil))]
        (if form
          (recur (conj forms form))
          forms)))))



(defn test1
  []
  (let [m (atom {})
        _ (walk/postwalk #(when (and (symbol? %)
                                     (not (namespace %))
                                     (not (contains? #{'let* 'do '. '&} %))
                                     (not (contains? (set (map str (all-ns))) (str %)))
                                     (try (not (resolve %)) (catch Exception e true)))
                            (swap! m assoc % (->kebab-case-sym %)))
                         (remove
                          #(try
                             (do (println (nth (nth (:form %) 2) 2))
                                 (= (nth (nth (:form %) 2) 2) '(setMacro)))
                             (catch Exception e false))
                          (analyzer/analyze-ns 'clojure.pprint)))]
    @m))


(defn find-binding-syms
  [ns]
  (let [syms (atom #{})]
    (walk/postwalk
     #(if (and (map? %)
               (= (:op %) :binding)
               (contains? #{:let :arg :letfn} (:local %)))
        (do
          (swap! syms conj (:form %)))
        %)
     (analyzer/analyze-ns 'leiningen.tact))
    @syms))


(def opts
  {:generated-sym? #(re-find #"__[0-9]+(#)?(__auto__)?$" (str %))
   :ignored-sym?   #{'_}})


(defn lazy-ast
  "Return the AST for this form as a lazy seq of nodes."
  [form]
  (->> form
       (analyzer/analyze+eval)
       (tree-seq :children #(flatten (for [kw (:children %)]
                             (kw %))))))


(defn get-bound-syms!
  "Return the set of symbols bound in this form's scope."
  [form]
  (->> form
       (lazy-ast)
       (filter #(= (:op %) :binding))
       (map :form)
       (remove (:generated-sym? opts))
       (remove (:ignored-sym? opts))
       (set)))


(defn assert-case!
  "assert case on bound syms
   TODO: var names"
  [path caser]
  (for [form (read-all path)]
    (let [syms (get-bound-syms! form)
          m    (->> (zipmap syms (map caser syms))
                    (remove #(= (first %) (second %)))
                    (into {}))]
      (when (seq m)
        (println "replacing:" m))
      (walk/postwalk-replace m form))))

#_(assert-case! "/Users/cody/dev/lein-tact/src/leiningen/tact.clj"
              ->kebab-case-sym)


(defn get-top-level-var-names!
  [nsSym]
  (->> (analyzer/analyze-ns nsSym)
       (filter #(= (:op %) :def))
       (map :name)))

#_(get-top-level-var-names! 'leiningen.tact)


;; function names!
#_(->> (analyzer/analyze-ns 'leiningen.tact)
     (filter #(= (:op %) :def))
     (map :name))

;; function args!
;; (->> (analyzer/analyze-ns 'leiningen.tact)
;;      (filter #(= (:op %) :def))
;;      (filter #(= (:name %) '))
;;      (first)
;;      (:init)
;;      :expr
;;      :methods
;;      (map :params)
;;      (flatten)
;;      (filter #(and (= (:op %) :binding)
;;                    (contains? #{:let :arg :letfn} (:local %))))
;;      (map :form))


;; function args 2!
;; (->> (analyzer/analyze-ns 'leiningen.tact)
;;      (filter #(= (:op %) :def))
;;      (filter #(= (:name %) 'grep))
;;      (first)
;;      (:var)
;;      (meta)
;;      (:arglists)
;;      (flatten))

;; top level let-bindings
;; (->> (analyzer/analyze-ns 'leiningen.tact)
;;      (filter #(contains? #{:let :letfn} (:op %)))
;;      (map :bindings)
;;      ;; (flatten)
;;      ;; (map :form)
;;      )

#_(analyzer/analyze '(let [x 1]
                       (let [z 2] (inc x))))




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
    (lein/abort "Unknown tact command:" command)))
