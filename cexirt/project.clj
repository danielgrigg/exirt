(defproject cexirt "0.0.1"
  :description "cexirt project"
  :main cexirt.core
  :dependencies [[org.clojure/clojure "1.3.0"]
                 [org.clojure/math.numeric-tower "0.0.1"] 
                 [incanter "1.3.0"]
                 [local/jna "1.0.0"]]
  :repositories {"local" ~(str (.toURI (java.io.File. "maven-repo")))})
