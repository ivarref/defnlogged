(ns com.github.ivarref.defnlogged-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is use-fixtures]]
            [clojure.tools.logging :as log]
            [com.github.ivarref.defnlogged :as d :refer [defnl]])
  (:import (java.time Duration)))

(defonce events (atom #{}))

(defn setup! [f]
  (locking d/history
    (reset! d/history (sorted-map))
    (reset! events #{})
    (f)))

(use-fixtures :each setup!)

(defnl timeout-with-default-val
  {:timeout     (Duration/ofMillis 1)
   :default-val :timeout}
  []
  (Thread/sleep 1000)
  :normal-return)

(defnl timeout-throws
  {:timeout (Duration/ofMillis 1)}
  []
  (Thread/sleep 1000)
  :normal-return)

(defnl exception-with-default-val
  {:default-val :exception-default-value}
  []
  (/ 1 0))

(defnl exception-throws
  {}
  []
  (throw (ex-info "My exception" {})))

(defmacro ex-message-starts-with? [stmt sub]
  `(str/starts-with?
     (try
       ~stmt
       ""
       (catch Throwable t#
         (ex-message t#)))
     ~sub))

(deftest basics
  (is (= :timeout (timeout-with-default-val)))
  (is (= :exception-default-value (exception-with-default-val)))
  (is (ex-message-starts-with? (timeout-throws) "Function timed out."))
  (is (ex-message-starts-with? (exception-throws) "My exception")))

(defn tap-event! [msg]
  (swap! events conj msg))

(defmacro has-event [e]
  `(let [start-time# (System/currentTimeMillis)]
     (while (and
              (false? (contains? (deref events) ~e))
              (let [diff-time# (- (System/currentTimeMillis) start-time#)]
                (< diff-time# 5000)))
       (Thread/sleep 10))
     (is (contains? (deref events) ~e))))

(defnl timeout-with-default-val-log
  {:timeout     (Duration/ofMillis 1)
   :default-val :timeout
   :tap-event!  tap-event!}
  []
  (Thread/sleep 1000)
  :normal-return)

(deftest timeout-events
  (is (= :timeout (timeout-with-default-val-log)))
  (has-event :timeout)
  (has-event :completion-after-timeout))
