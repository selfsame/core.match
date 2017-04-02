(ns clojure.core.match.test.date
  (:use clojure.test)
  (:use [clojure.core.match :only [matchm]])
  (:use clojure.core.match.date))

(deftest date-test1
  (is (= (matchm [(DateTime. 2010 10 1 12 30 0)]
           [{:year 2009 :month a}] a
           [{:year (:or 2010 2011) :month b}] b
           :else :wrong)
        10)))
