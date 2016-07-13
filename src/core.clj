(ns core
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]
   [model :as model]
   [codegen :as gen])
  (:gen-class))

(defn read-dsl [filename]
  (with-open [r (io/reader filename)]
    (read (java.io.PushbackReader. r))))

(defn code->str
  [seq]
  (for [x (flatten seq)]
    (case x
      :indent "    "
      :nl "\r\n"
      (str x))))

(defn print-code [seq] (doseq [x seq] (print x)))

(defn walk [dirpath pattern]
  (doall (filter #(re-matches pattern (.getName %))
                 (file-seq (io/file dirpath)))))

(defn -main [& args]
  (let [
        [dir] args
        dir (or dir ".")
        ]
    (doseq [file (walk dir #".*\.edn")]
      (print (str "Reading " file  "... "))
      (let [
            model (-> file
                      .getPath
                      read-dsl
                      model/dsl->model)
            lines (-> model
                      gen/model->code
                      code->str)
            out (io/file (.getParent file) (str (:file model)))
            ]
        (print (str "saving to " out))
        (with-open [w (io/writer (str out))]
          (doseq [l lines] (.write w l)))
        (println "Done")))))
