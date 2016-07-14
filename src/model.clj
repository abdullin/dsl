(ns model
  (:require [clojure.string :as str]))

(defn- switch-case [s fn]
  (let [[first & rest] s]
    (str/join (cons (fn first) rest))))

(defn- camel [s] (switch-case s str/lower-case))
(defn- pascal [s] (switch-case s str/upper-case))

(defn- interpolate
  "Takes a string and a collection of fields to create a .NET format string"
  [s fields]
  (take 2 
        (reduce
         (fn [acc field]
           (let [
                 [s matched idx] acc
                 prop (:prop field)
                 id (or (:id field) prop)
                 regex (re-pattern (str "(?i)\\{(" (str/join "|" [prop (name id)]) ")\\}"))
                 ]
             (if (re-find regex s)
               [(str/replace s regex (str "{" idx "}")) (conj matched prop) (inc idx)]
               [s matched idx]))
           )
         [s [] 0] fields)))


(defn- field
  ([type] (field type type))
  ([type name] (field type name nil))
  ([type name schema] (if (vector? type)
     (let [type (first type)]
       (assoc (field (str type "[]") name) :array-of (str type)))
     {:name (camel (str name)) :type (str type) :prop (pascal (str name)) :schema schema})))

(defn unwrap-field
  "field can be :const [type] [type name] [type name schema]"
  [index fld const]
  (let [
        tuple (get const fld fld)
        id (if-not (seq? fld) fld)
        ]
    (assoc (apply field tuple) :id id :order (inc index))))

(defn unwrap-message- [msg agg const extern]
  (let [
        [kind id fs txt] msg
        fs (if (= kind 'rec) fs  (concat (:common agg) fs))
        clean (filter some? (map-indexed #(if %2 (unwrap-field %1 %2 const)) fs))
        hs (if txt (interpolate txt clean))
        ]
    {
     :name id
     :fields clean 
     :string hs
     :kind kind
     :base ((keyword kind) agg)
     :extern extern
     }
    ))

(defn unwrap-agg- [file-const file-extern agg]
  (let [
        {:keys [event command const messages]} agg
        const (merge file-const const)
        ]
    (println (str "Processing aggregate" (:name agg)))
    (assoc agg :messages (map #(unwrap-message- % agg const file-extern) messages))))

(defn dsl->model [cfg]
  (let [{:keys [const aggs extern]} cfg]
    (assoc cfg :aggs (map #(unwrap-agg- const extern %) aggs))))
