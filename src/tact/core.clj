(ns tact.core
  (:require [camel-snake-kebab.core :as case]
            [clojure pprint
             [walk :as walk]]
            [clojure.java.io :as io]
            [clojure.tools.analyzer.jvm :as analyzer]
            [clojure.tools.namespace.find :as ns.find]
            clojure.tools.reader
            [tact.util :as util]))

(set! *warn-on-reflection* false)
(set! *print-length* 20)
(set! *print-level* 20)


;; :children

;; :bindings vector -> each elem is a map ->

(defn ->kebab-case-sym
  [s]
  (let [kebab (if s (case/->kebab-case s)
                  (case/->kebab-case s))]
    (symbol (if (not= kebab s) kebab s))))





#_(defn test1
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




#_(assert-case! "/Users/cody/dev/lein-tact/src/leiningen/tact.clj"
              ->kebab-case-sym)


(defn get-top-level-var-names!
  [ns-sym]
  (->> (analyzer/analyze-ns ns-sym)
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


(defn assert-case!
  "assert case on bound syms
   TODO: var names"
  [forms config]
  (let [caser  (case (:case config)
                 :kebab case/->kebab-case
                 :camel case/->camelCase
                 case/->camelCase)
        ->case (fn [s]
                 (let [new-case (try (caser s) (catch Exception e nil))]
                   (symbol (if (not= new-case s) new-case s))))]
    (mapv
     (fn [form] (let [syms (get-bound-syms! form)
                      m    (->> (zipmap syms (map ->case syms))
                                (remove #(= (first %) (second %)))
                                (into {}))]
                  (when (seq m)
                    (println "replacing:" m))
                  (walk/postwalk-replace m form)))
     forms)))


(defn read-forms
  [file]
  (let [rdr (-> file io/file io/reader java.io.PushbackReader.)]
    (loop [forms []]
      (let [form (try
                   (clojure.tools.reader/read rdr)
                   (catch Exception e nil))]
        (if form
          (recur (conj forms form))
          forms)))))

(defn have-tact
  [project]
  (println "have tact")
  ;; (clojure.pprint/pprint project)
  (let [dirs       (util/get-project-dirs project)
        namespaces (mapcat ns.find/find-namespaces-in-dir dirs)
        files      (mapcat #(util/find-files project %) dirs)]
    (println files)
    (apply require namespaces)
    (doseq [file files]
      (assert-case! (read-forms file) (:tact project)))))
