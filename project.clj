(defproject dsl "0.1.0"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]]
  :main core
  :uberjar-name ~(str "dsl-%s-"
                      (-> (clojure.java.shell/sh "git" "rev-parse" "--short" "HEAD") :out .trim)
                      ".uber.jar")
  )
