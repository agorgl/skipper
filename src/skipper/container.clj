(ns skipper.container
  (:require
   [clojure.java.io :as io]
   [clj-yaml.core :as yaml]
   [skipper.config :as config]))

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
