(ns skipper.container
  (:require
   [clojure.string :as str]
   [clojure.java.io :as io]
   [clj-yaml.core :as yaml]
   [skipper.config :as config]
   [skipper.process :as process]))

(def label "skipper")

(defn cntrdir []
  (let [datadir (-> (config/config-map) :datadir)
        cntrdir (-> (io/file datadir "containers"))]
    (.getPath cntrdir)))

(defn pod-yaml [app & {:keys [image env ports]}]
  (let [image (or image (str app ":latest"))
        env (->> env
                 (map (fn [[n v]]
                        {:name n
                         :value v})))
        ports (->> ports
                   (map (fn [[hp cp]]
                          {:hostPort hp
                           :containerPort cp})))
        pod {:apiVersion "v1"
             :kind "Pod"
             :metadata
             {:name app
              :labels {:app app
                       (keyword label) app}
              :annotations {:io.podman.annotations.infra.name (format "%s-infra" app)}}
             :spec
             {:containers
              [(cond-> {:name app
                        :image image}
                 (seq env) (merge {:env env})
                 (seq ports) (merge {:ports ports}))]}}
        yaml-opts {:indent 2
                   :indicator-indent 2
                   :indent-with-indicator true
                   :flow-style :block}]
    (yaml/generate-string pod :dumper-options yaml-opts)))

(defn pod-manifest [app]
  (let [file (io/file (cntrdir) (str app ".yaml"))]
    (.getPath file)))

(defn pod-manifest-create [app yaml]
  (let [file (pod-manifest app)]
    (io/make-parents file)
    (spit file yaml)
    file))

(defn pod-manifest-delete [app]
  (let [file (pod-manifest app)]
    (io/delete-file file true)))

(defn pod-exists [app]
  (let [command ["podman" "pod" "exists" app]
        {:keys [exit]} (process/exec command {:throw? false})]
    (zero? exit)))

(defn pod-running [app]
  (when (pod-exists app)
    (let [command ["podman" "pod" "inspect" "-f" "{{.State}}" app]
          {:keys [out]} (process/exec command)]
      (= (-> out (str/trim-newline) (str/lower-case)) "running"))))

(defn pod-create [app & [opts]]
  (let [opts (or opts {:image (str app ":latest")})
        yaml (pod-yaml app opts)
        file (pod-manifest-create app yaml)
        command ["podman" "kube" "play" "-q" "--start=false" file]]
    (process/exec command)))

(defn pod-start [app]
  (when-not (pod-running app)
    (let [command ["podman" "pod" "start" app]]
      (process/exec command))))

(defn pod-logs [app]
  (let [command ["podman" "pod" "logs" app]]
    (process/exec command {:printer process/std-printer})))

(defn pod-list []
  (let [command ["podman" "pod" "ps" "-n" "--filter" (str "label=" label) "--format" "{{.Name}}"]
        {:keys [out]} (process/exec command)]
    (-> out
        str/split-lines)))

(defn pod-stop [app]
  (when (pod-running app)
    (let [command ["podman" "pod" "stop" app]]
      (process/exec command))))

(defn pod-delete [app]
  (let [command ["podman" "pod" "rm" app]]
    (process/exec command))
  (pod-manifest-delete app))
