(ns skipper.process
  (:require
   [clojure.string :as str]
   [clojure.core.async :as async :refer [chan go go-loop <! >! <!! >!!]]
   [clojure.java.io :as io]
   [clojure.java.process :as process]))

(defn line-chan
  "Given an input stream, return a core.async channel
   that emits the line buffered values of that stream."
  [input]
  (let [ch (chan 128)]
    (async/thread
      (with-open [reader (io/reader input)]
        (loop [vs (line-seq reader)]
          (if (and vs (>!! ch (first vs)))
            (recur (next vs))
            (async/close! ch)))))
    ch))

(defn stdbuf-chan
  "Takes stdout and stderr input streams from a java.lang.Process
   and returns a core.async channel with the tagged line buffered output values."
  [out err]
  (let [outc (async/map #(do {:stream :out :value %}) [(line-chan out)])
        errc (async/map #(do {:stream :err :value %}) [(line-chan err)])]
    (async/merge [outc errc] 128)))

(defn start
  "Start an external command.

   Additional options are passed directly to clojure.java.process/start.
   The standard output streams are redirected to a core.async channel,
   that receives line buffered values from both stdout and stderr.

   Returns the exit ref and the streams channel of the process."
  [command & [opts]]
  (let [opts (merge opts {:out :pipe :err :pipe})
        proc (apply process/start opts command)
        stdc (stdbuf-chan (process/stdout proc) (process/stderr proc))
        exit (process/exit-ref proc)]
    {:exit exit :streams stdc}))

(defn streams-reduce
  "Takes the 'streams' channel returned by 'start'
   and returns a channel containing a single reduced result vector
   with the stdout and stderr captures."
  [stdc]
  (->> stdc
       (async/reduce
        (fn [[out err] {:keys [stream value]}]
          (case stream
            :out [(conj out value) err]
            :err [out (conj err value)]))
        [[] []])
       (vector)
       (async/map
        (fn [[out err]]
          [(when (seq out) (str (str/join "\n" out) "\n"))
           (when (seq err) (str (str/join "\n" err) "\n"))]))))

(defn wait
  "Waits for a process launched by 'start' to finish.
   Returns exit code along with captured output."
  [{:keys [exit streams]}]
  (let [c (go
            (let [iopc (go (<! (streams-reduce streams)))
                  exit (deref exit)
                  [out err] (<! iopc)]
              {:exit exit :out out :err err}))]
    (<!! c)))

(defn tap-streams
  "Taps into the 'streams' channel returned by 'start'
   calling the given function on each value passed through."
  [streams f]
  (let [m (async/mult streams)
        ca (chan 128)
        cb (chan 128)]
    (async/tap m ca)
    (async/tap m cb)
    (go-loop []
      (when-let [v (<! ca)]
        (f v)
        (recur)))
    cb))

(defn std-printer
  "Prints the tagged line value passed to the relevant standard stream."
  [{:keys [stream value]}]
  (case stream
    :out (println value)
    :err (binding [*out* *err*]
           (println value))))

(defn exec
  "Execute a command and return the captured output.

   Args are the same as 'start'.
   An optional printer function can be passed that will be called
   for each stdout/stderr line produced by the started process.

   Throws an exception on unsuccessful exit by default."
  [command & {:keys [printer throw?] :or {throw? true} :as opts}]
  (let [p (cond-> (start command opts)
            printer (update :streams #(tap-streams % printer)))
        {:keys [exit] :as result} (wait p)]
    (if (or (not throw?) (zero? exit))
      result
      (let [msg (format "command `%s` failed with exit %d"
                        (str/join " " command)
                        exit)]
        (throw (ex-info msg result))))))
