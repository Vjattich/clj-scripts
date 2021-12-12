(ns fast-namespace
  (:require [clojure.tools.cli :as cli]
            [clojure.string :as s])
  (:import (java.io File)))

(def ns-template
  "(ns %s)")

(def test-template
  "(ns %s\n  (:require [clojure.test :refer :all]\n            [%s :refer :all]))\n\n(deftest a-test\n  (testing\n    (is (= [] []))))")


(let [data (:options (cli/parse-opts *command-line-args* [["-n" "--name NAME" "Name of file"
                                                           :default nil
                                                           :parse-fn str]
                                                          ["-d" "--directory DIRECTORY" "Path of proj"
                                                           :default nil
                                                           :parse-fn str]
                                                          ["-s" "--src SOURCES" "Name of source folder"
                                                           :default nil
                                                           :parse-fn str]]))
      name (:name data)
      src (:src data)
      dir (:directory data)
      paths {:ns-path       (s/join File/separator [dir "src" src (str name ".clj")])
             :test-path     (s/join File/separator [dir "test" src (str name "_test.clj")])
             :ns-template   (format ns-template
                                    (s/replace (str (s/replace src File/separator ".") "." name) "_" "-"))
             :test-template (format test-template
                                    (s/replace (str (s/replace src File/separator ".") "." name "-test") "_" "-")
                                    (s/replace (str (s/replace src File/separator ".") "." name) "_" "-"))}]
  (do
    (spit (:ns-path paths) (:ns-template paths))
    (spit (:test-path paths) (:test-template paths))))