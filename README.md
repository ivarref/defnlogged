# defnlogged

`defnl` is an opinionated `defn` macro for building more robust systems.
It supports declarative timeout and exception handling, as well as
default return values.

## Installation

```clojure
com.github.ivarref/defnlogged {:git/tag "0.0.1" :git/sha "..."}
```

Add
```
{:lint-as {com.github.ivarref.defnlogged/defnl clojure.core/defn}}
```
to your `.clj-kondo/config.edn`.

## Usage

```clojure
; Add
(:require 
  [com.github.ivarref.defnlogged :refer [defnl]])
; to your ns declaration


(defnl brittle-fn
       {:timeout-ms  600000 ; or (java.time.Duration/ofMinutes 10)
        :default-val nil} ; return nil on failures/timeouts
       []
       (some-legacy-lib/unstable-network-io))
```
