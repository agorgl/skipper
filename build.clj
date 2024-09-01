(ns build
  (:refer-clojure :exclude [test])
  (:require
   [clojure.string :as str]
   [clojure.java.io :as io]
   [clojure.tools.build.api :as b]))

(def lib 'net.clojars.skipper/skipper)
(def version "0.1.0-SNAPSHOT")
(def main 'skipper.core)
(def class-dir "target/classes")

(defn test
  "Run all the tests"
  [opts]
  (let [basis (b/create-basis {:aliases [:test]})
        cmds  (b/java-command
               {:basis     basis
                :main      'clojure.main
                :main-args ["-m" "cognitect.test-runner"]})
        {:keys [exit]} (b/process cmds)]
    (when-not (zero? exit)
      (throw (ex-info "Tests failed" {}))))
  opts)

(defn- uber-opts [opts]
  (assoc opts
         :lib lib
         :main main
         :uber-file (format "target/%s-%s.jar" lib version)
         :basis (b/create-basis (:basis-opts opts))
         :class-dir class-dir
         :src-dirs ["src"]
         :ns-compile [main]))

(defn uber
  "Build the uberjar"
  [opts]
  (b/delete {:path "target"})
  (let [opts (uber-opts opts)]
    (println "Copying source...")
    (b/copy-dir {:src-dirs ["resources" "src"] :target-dir class-dir})
    (println (str "Compiling " main "..."))
    (b/compile-clj opts)
    (println "Building JAR...")
    (b/uber opts))
  opts)

(defn ci
  "Run the CI pipeline of tests and build the uberjar"
  [opts]
  (test opts)
  (uber opts)
  opts)

(defn native
  "Build the native binary"
  [opts]
  (let [opts (assoc opts :basis-opts {:aliases [:native]})]
    (uber opts)
    (println "Building native binary...")
    (if-let [graalvm-home (System/getenv "GRAALVM_HOME")]
      (let [jar (:uber-file (uber-opts opts))
            binary (str/replace jar #"\.jar$" "")
            command [(str (io/file graalvm-home "bin" "native-image"))
                     "-jar" jar
                     "-o" binary
                     "-H:+ReportExceptionStackTraces"
                     "-J-Dclojure.compiler.direct-linking=true"
                     "-J-Dclojure.spec.skip-macros=true"
                     "--features=clj_easy.graal_build_time.InitClojureClasses"
                     "--no-fallback"
                     "--static"
                     "--libc=musl"
                     "--native-image-info"
                     "--verbose"]]
        (b/process {:command-args command}))
      (throw (ex-info "Environment variable GRAALVM_HOME is not set" {})))
    opts))
