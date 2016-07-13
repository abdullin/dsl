(ns core
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str])
  (:gen-class))

(defn switch-case [s fn]
  (let [[first & rest] s]
    (str/join (cons (fn first) rest))))

(defn camel [s] (switch-case s str/lower-case))
(defn pascal [s] (switch-case s str/upper-case))

(defn load-config [filename]
  (with-open [r (io/reader filename)]
    (read (java.io.PushbackReader. r))))

(defn to-string-
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

(defn field-
  ([type] (field- type type))
  ([type name] (field- type name nil))
  ([type name schema] (if (vector? type)
     (let [type (first type)]
       (assoc (field- (str type "[]") name) :array-of (str type)))
     {:name (camel (str name)) :type (str type) :prop (pascal (str name)) :schema schema})))

(defn unwrap-field-
  "field can be :const [type] [type name] [type [] name]"
  [index fld const]
  (let [
        tuple (get const fld fld)
        id (if-not (seq? fld) fld)
        ]
    (assoc (apply field- tuple) :id id :order (inc index))))

(defn unwrap-message- [msg agg const extern]
  (let [
        [kind id fs txt] msg
        fs (if (= kind 'rec) fs  (concat (:common agg) fs))
        clean (filter some? (map-indexed #(if %2 (unwrap-field- %1 %2 const)) fs))
        hs (if txt (to-string- txt clean))
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
    (assoc agg :messages (map #(unwrap-message- % agg const file-extern) messages) )))

(defn unwrap- [cfg]
  (let [{:keys [const aggs extern]} cfg]
    (assoc cfg :aggs (map #(unwrap-agg- const extern %) aggs))))

(defn tab [seq]
  (reduce
   (fn [agg next]
     (if (or (empty? agg) (= :nl (last agg)))
       (conj agg :indent next)
       (conj agg next)))
   []
   (filter some? (flatten seq))))

(defn ctor-assert- [field ln]
  (let [{:keys [schema name]} field]
    (case schema
      'NotNull (ln "\t\t\tif ( %s == null ) throw new ArgumentNullException( \"%s\" );\n" name name)
      nil)))

(defn gen-property-
  [m]
  (let [{:keys [order prop type]} m]
    ["[DataMember(Order = " order ")] public " type " " prop " { get; private set; }" :nl]))

(defn gen-property-assoc- [m] [(:prop m) " = " (:name m) ";" :nl])
(defn gen-arg-assert-
  [f]
  (let [{:keys [schema name]} f]
    (case schema
      'NotNull ["if ( " name " == null ) throw new ArgumentNullException( \"" name "\" );" :nl]
      nil)))

(defn gen-arg- [m] [(:type m) " " (:name m)])
(defn gen-array-init [m] [(:prop m) " = new " (:array-of m) "i[0];" :nl])

(defn gen-private-ctor-
  [name fields]
  (let [arrays (filter #(:array-of %) fields)]
    (if (seq arrays)
      [name " ()" :nl "{" :nl (tab (map gen-array-init arrays)) "}" :nl]
      [name " () {}" :nl])))

(defn gen-public-ctor-
  [name fields]
  ["public " name "(" (interpose ", " (map gen-arg- fields)  ) ")" :nl
   "{" :nl
   (tab (map gen-arg-assert- fields))
   (tab (map gen-property-assoc- fields))
   "}" :nl
   ])

(defn gen-string-
  [txt names]
  ["public override string ToString()" :nl
   "{" :nl
   (tab ["return string.Format(@\"" txt "\", " (str/join ", " names) ");" :nl])
   "}" :nl])

(defn gen-message-
  [model]
  (let [{:keys [name base fields string extern]} model]
    [
     "[DataContract(Namespace = \"" extern "\")]" :nl
     "public partial class " name ": " base :nl
     "{" :nl
     (tab
      [
       (if (seq fields)
         [(map gen-property- fields)
          (gen-private-ctor- name fields) 
          (gen-public-ctor- name fields)
          (if string (apply gen-string- string))
          ])])
     "}" :nl
     ]
    ))

(defn gen-agg- [agg] (map gen-message- (:messages agg)))

(defn render-
  [seq]
  (for [x (flatten seq)]
    (case x
      :indent "    "
      :nl "\r\n"
      (str x))))

(defn print-code [seq] (doseq [x seq] (print x)))

(defn gen-code-
  "Generates C# code tree given the model"
  [model]
  (let [{:keys [using namespace aggs]} model]
    (concat
     (map #(list "using " % ";" :nl) using)
     ["namespace " namespace :nl "{" :nl
      (tab
       (map gen-agg- aggs))
      "}" :nl])))

(defn walk [dirpath pattern]
  (doall (filter #(re-matches pattern (.getName %))
                 (file-seq (io/file dirpath)))))

(defn -main [& args]
  (let [
        [dir] args
        dir (or dir ".")
        ]
    (doseq [file (walk dir #".*\.edn")]
      (let [
            filename (.getPath file)
            cfg (load-config filename)
            data (unwrap- cfg)
            out  (io/file (.getParent file)  (str (:file cfg)))
            ]
        (print (str "Generating " filename  " to " out "..."))
        (with-open [w (io/writer (str out))]
          (doseq [l (render- (gen-code- data))] (.write w l)))
        (println "Done")))))
