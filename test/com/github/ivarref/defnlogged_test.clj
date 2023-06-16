(ns com.github.ivarref.defnlogged-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is use-fixtures]]
            [com.github.ivarref.defnlogged :as d :refer [defnl]])
  (:import (java.time Duration)))

(defonce events (atom #{}))

(defn tap-event! [msg]
  (swap! events conj msg))

(defmacro has-event [e]
  `(let [start-time# (System/currentTimeMillis)]
     (while (and
              (false? (contains? (deref events) ~e))
              (let [diff-time# (- (System/currentTimeMillis) start-time#)]
                (< diff-time# 3000)))
       (Thread/sleep 10))
     (is (contains? (deref events) ~e))))

(defmacro not-has-event [e]
  `(is (false? (contains? (deref events) ~e))))

(defn setup! [f]
  (locking d/history
    (reset! d/history (sorted-map))
    (reset! events #{})
    (f)))

(use-fixtures :each setup!)

(defnl timeout-with-default-val-fn
  {:timeout-ms     (Duration/ofMillis 1)
   :default-val :timeout
   :tap-event!  tap-event!}
  []
  (Thread/sleep 1000)
  :normal-return)

(defnl timeout-throws-fn
  {:timeout-ms    (Duration/ofMillis 1)
   :tap-event! tap-event!}
  []
  (Thread/sleep 1000)
  :normal-return)

(defnl exception-with-default-val-fn
  {:default-val :exception-default-value
   :tap-event!  tap-event!}
  []
  (/ 1 0))

(defnl exception-throws-fn
  {:tap-event! tap-event!}
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

(defnl retval
  {:tap-event! tap-event!}
  [x]
  x)

(deftest regular-return-value
  (is (= nil (retval nil)))
  (is (= 123 (retval 123)))
  (has-event :return-val)
  (not-has-event :internal-error))

(deftest timeout-default-val
  (is (= :timeout (timeout-with-default-val-fn)))
  (has-event :timeout)
  (has-event :completion-after-timeout)
  (not-has-event :internal-error))

(deftest exception-default-val
  (is (= :exception-default-value (exception-with-default-val-fn)))
  (has-event :exception)
  (not-has-event :internal-error))

(deftest timeout-throws
  (is (ex-message-starts-with? (timeout-throws-fn) "Function timed out."))
  (has-event :timeout-throw)
  (not-has-event :internal-error))

(deftest exception-throws
  (is (ex-message-starts-with? (exception-throws-fn) "My exception"))
  (has-event :throw)
  (not-has-event :internal-error))

(defnl default-val-can-be-nil
  {:default-val nil}
  []
  (throw (ex-info "oh no" {})))

(deftest default-val-can-be-nil-test
  (is (= nil (default-val-can-be-nil))))

(defnl no-tap-event
  {}
  [x]
  x)

(deftest no-tap-event-test
  (is (= 123 (no-tap-event 123))))

(defmacro swallow [& body]
  `(try
     ~@body
     (is (= "Expected an exception" "Got nothing"))
     (catch Throwable t#
       (is (= 1 1)))))

(deftest stat-test
  (with-redefs [d/today-yyyy-MM-dd-utc (fn [& _args] "2000-01-01")]
    (no-tap-event :demo)
    (swallow (exception-throws-fn))
    (swallow (exception-throws-fn))
    (swallow (timeout-throws-fn))
    (exception-with-default-val-fn)
    (is (= {"error" {"2000-01-01" {"com.github.ivarref.defnlogged-test/exception-throws-fn"           2
                                   "com.github.ivarref.defnlogged-test/exception-with-default-val-fn" 1
                                   "com.github.ivarref.defnlogged-test/timeout-throws-fn"             1}}
            "ok"    {"2000-01-01" {"com.github.ivarref.defnlogged-test/no-tap-event" 1}}}
           (d/fn-stats)))))
