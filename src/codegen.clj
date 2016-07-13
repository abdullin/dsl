(ns codegen
  (:require [clojure.string :as str]))

(defn indent [seq]
  (reduce
   (fn [agg next]
     (if (or (empty? agg) (= :nl (last agg)))
       (conj agg :indent next)
       (conj agg next)))
   []
   (filter some? (flatten seq))))

(defn gen-property
  [m]
  (let [{:keys [order prop type]} m]
    ["[DataMember(Order = " order ")] public " type " " prop " { get; private set; }" :nl]))

(defn gen-assignment [m] [(:prop m) " = " (:name m) ";" :nl])
(defn gen-assert
  [f]
  (let [{:keys [schema name]} f]
    (case schema
      'NotNull ["if ( " name " == null ) throw new ArgumentNullException( \"" name "\" );" :nl]
      nil)))

(defn gen-arg [m] [(:type m) " " (:name m)])
(defn gen-array-init [m] [(:prop m) " = new " (:array-of m) "i[0];" :nl])

(defn gen-private-ctor
  [name fields]
  (let [arrays (filter #(:array-of %) fields)]
    (if (seq arrays)
      [name " ()" :nl "{" :nl (indent (map gen-array-init arrays)) "}" :nl]
      [name " () {}" :nl])))

(defn gen-public-ctor
  [name fields]
  ["public " name "(" (interpose ", " (map gen-arg fields)  ) ")" :nl
   "{" :nl
   (indent (map gen-assert fields))
   (indent (map gen-assignment fields))
   "}" :nl
   ])

(defn gen-to-string
  [txt names]
  ["public override string ToString()" :nl
   "{" :nl
   (indent ["return string.Format(@\"" txt "\", " (str/join ", " names) ");" :nl])
   "}" :nl])

(defn gen-contract
  [model]
  (let [{:keys [name base fields string extern]} model]
    [
     "[DataContract(Namespace = \"" extern "\")]" :nl
     "public partial class " name ": " base :nl
     "{" :nl
     (indent
      [
       (if (seq fields)
         [(map gen-property fields)
          (gen-private-ctor name fields) 
          (gen-public-ctor name fields)
          (if string (apply gen-to-string string))
          ])])
     "}" :nl
     ]
    ))

(defn gen-agg [agg] (map gen-contract (:messages agg)))

(defn model->code
  "Generates C# code tree given the model"
  [model]
  (let [{:keys [using namespace aggs]} model]
    (concat
     (map #(list "using " % ";" :nl) using)
     ["namespace " namespace :nl "{" :nl
      (indent (map gen-agg aggs))
      "}" :nl])))
