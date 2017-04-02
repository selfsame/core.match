(ns clojure.core.match.test.java
  (:refer-clojure :exclude [reify == inc compile])
  (:use [clojure.core.match.protocols]
        [clojure.core.match]
        [clojure.core.match.java]
        [clojure.test]))

(bean-match DateTime)

(deftest bean-match-date
  (is (= 10 (matchm [(DateTime. 2009 10 1 12 30)]
              [{:year 2009 :month a}] a
              [{:year (:or 2010 2011) :month b}] b
              :else :wrong))))

#_(bean-match System.IO.File)

#_(deftest bean-match-file
  (is (= (.getAbsolutePath (System.IO.File. ".")) 
        (matchm [(java.io.File. ".")]
          [{:directory? true :absolute-path p}] p
          :else :wrong))))

