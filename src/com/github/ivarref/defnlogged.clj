(ns com.github.ivarref.defnlogged
  (:require [clojure.stacktrace :as st]
            [clojure.tools.logging :as log])
  (:import (java.time Duration ZoneId ZonedDateTime)
           (java.time.format DateTimeFormatter)
           (java.util.concurrent TimeoutException)))

(defonce history (atom (sorted-map)))

(defn today-yyyy-MM-dd-utc []
  (.format (ZonedDateTime/now (ZoneId/of "UTC")) (DateTimeFormatter/ofPattern "yyyy-MM-dd")))

(defn add-fn-count [count-type yyyy-MM-dd-utc fn-name-str old-state]
  (let [fn-state (get old-state "fn")
        keep-keys (->> (into (vec (keys fn-state)) [yyyy-MM-dd-utc])
                       (sort)
                       (take-last 7)
                       (vec))
        day-map (get-in old-state ["fn" yyyy-MM-dd-utc] (sorted-map))
        new-day-map (update-in day-map [count-type fn-name-str] (fnil inc 0))]
    (assoc old-state "fn"
                     (-> fn-state
                         (assoc yyyy-MM-dd-utc new-day-map)
                         (select-keys keep-keys)))))

(defn coerce-to-millis [x]
  (cond (nil? x)
        0

        (number? x)
        (long x)

        (instance? Duration x)
        (.toMillis ^Duration x)

        :else
        (throw (ex-info "Could not coerce to millis" {:x x}))))

(defn now-millis []
  (System/currentTimeMillis))

(defn- logged-defn
  [{:keys [timeout
           default-val
           fn-history-atom
           tap-event!]
    :or   {timeout         300000                           ; 5 minutes timeout
           default-val     ::none
           fn-history-atom `history
           tap-event!      `comment}
    :as   _attr-map} s1 body]
  `(let [f# (subs (str ~s1) 1)
         default-val# ~default-val
         timeout-ms# (coerce-to-millis ~timeout)
         exception?# (promise)
         timeout?# (promise)
         thread# (promise)
         bound-fn# (bound-fn []
                     (try
                       (~tap-event! :start)
                       (deliver thread# (Thread/currentThread))
                       (swap! ~fn-history-atom assoc-in ["running-threads" (.getName (Thread/currentThread))] (now-millis))
                       (let [start-time# (now-millis)
                             res# (do ~@body)]
                         (if (realized? timeout?#)
                           (do
                             (~tap-event! :completion-after-timeout)
                             (log/error "Function" (str f#) "was timed out, but finished after"
                                        (- (now-millis) start-time#)
                                        "milliseconds"))
                           (swap! ~fn-history-atom (partial add-fn-count :ok (today-yyyy-MM-dd-utc) (str f#))))
                         res#)
                       (catch Throwable t#
                         (do
                           (~tap-event! :exception)
                           (log/error t# "Unhandled exception occurred while executing function" f#)
                           (swap! ~fn-history-atom (partial add-fn-count :error (today-yyyy-MM-dd-utc) (str f#)))
                           (deliver exception?# t#)))
                       (finally
                         (swap! ~fn-history-atom update "running-threads" (fn [m#] (dissoc m# (.getName (Thread/currentThread))))))))
         fut# (future (bound-fn#))
         res# (try
                (deref fut# timeout-ms# ::timeout)
                (catch Throwable t#
                  (~tap-event! :internal-error)
                  (log/error t# "defnlogged internal error. Should not happen. Error message:"
                             (ex-message t#))
                  (throw t#)))]
     (when (= res# ::timeout)
       (~tap-event! :timeout)
       (deliver timeout?# true)
       (log/error
         (str "Function timed out. Fn: " f#
              (when (realized? thread#)
                (let [^Thread thread# @thread#]
                  (str " thread name: "
                       (.getName thread#)
                       " stacktrace:\n"
                       (with-out-str
                         (doseq [ste# (into [] (.getStackTrace thread#))]
                           (st/print-trace-element ste#)
                           (println ""))))))))
       (swap! ~fn-history-atom (partial add-fn-count :timeout (today-yyyy-MM-dd-utc) (str f#))))
     (cond (and (not= ::none default-val#)
                (or (realized? exception?#)
                    (= res# ::timeout)))
           (do
             (~tap-event! :default-val)
             default-val#)

           (= res# ::timeout)
           (do
             (~tap-event! :timeout-throw)
             (throw (TimeoutException. (str "Function timed out. Fn: " f#))))

           (realized? exception?#)
           (do
             (~tap-event! :throw)
             (throw @exception?#))

           :else
           (do
             (~tap-event! :return-val)
             res#))))

(defn- fn-sigs [def? fn-sym sigs]
  (let [single-arity? (vector? (first sigs))
        sigs (if single-arity? (list sigs) sigs)
        base-id (str *ns* "/" (if def? "" "fn_") (name fn-sym))

        get-id
        (if single-arity?
          (fn [fn-sym _params] (keyword base-id))
          (fn [fn-sym params] (keyword (str base-id "_" (count params)))))

        new-sigs
        (map
          (fn [[params & others]]
            (let [has-prepost-map? (and (map? (first others)) (next others))
                  [?prepost-map & body] (if has-prepost-map? others (cons nil others))]
              (if ?prepost-map
                `(~params ~?prepost-map ~(logged-defn (meta fn-sym) (get-id fn-sym params) body))
                `(~params ~(logged-defn (meta fn-sym) (get-id fn-sym params) body)))))
          sigs)]
    new-sigs))

(defn- name-with-attrs
  "Given a symbol and args, returns [<name-with-attrs-meta> <args>] with
  support for `defn` style `?docstring` and `?attrs-map`."
  ([sym args] (name-with-attrs sym args nil))
  ([sym args attrs-merge]
   (let [[?docstring args] (if (and (string? (first args)) (next args)) [(first args) (next args)] [nil args])
         [attrs args] (if (and (map? (first args)) (next args)) [(first args) (next args)] [{} args])
         attrs (if ?docstring (assoc attrs :doc ?docstring) attrs)
         attrs (if (meta sym) (conj (meta sym) attrs) attrs)
         attrs (conj attrs attrs-merge)]
     [(with-meta sym attrs) args])))

(defmacro defnl
  "defnl is a supercharged `defn` that:
  * Logs if an exception or timeout occurs (at error level).
  * Saves statistics about number of ok invocations,
  exceptions and timeouts for each function.

  attr-map? can be a map with the following keys:

  :timeout (java.time.Duration/ofMinutes 5)
  ; If a number is used, milliseconds is assumed
  ; The default timeout is 5 minutes.

  :default-val :some-value
  ; If specified, this value will be returned if a timeout
  ; or an exception occurs. `nil` is considered a specified
  ; value.

  ; If :default-val is unspecified, exceptions will be thrown.
  ; If a timeout occurs an exception of type
  ; java.util.concurrent.TimeoutException
  ; will be thrown.

  Example usage:
  (defnl my-fn-wrapping-some-brittle-library
    {:timeout (java.time.Duration/ofMinutes 1)
     :default-val [] ; We don't want exceptions/timeouts to bubble up,
                     ; and are happy with returning an empty vector
                     ; in the case of exceptions/timeouts.
     }
    [my-args]
    (some-lib/fn-doing-some-network-io-without-proper-timeouts my-args))
  "
  {:arglists
   '([name doc-string? attr-map? [params*] prepost-map? body]
     [name doc-string? attr-map? ([params*] prepost-map? body) + attr-map?])}
  [& sigs]
  (let [[fn-sym sigs] (name-with-attrs (first sigs) (next sigs))
        new-sigs (fn-sigs :def fn-sym sigs)]
    `(clojure.core/defn ~fn-sym ~@new-sigs)))
