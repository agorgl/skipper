(ns skipper.image
  (:require
   [clojure.java.io :as io]
   [skipper.config :as config]
   [skipper.repo :as repo]
   [skipper.process :as process]))

(defn builddir
  ([]
   (builddir nil))
  ([build]
   (let [datadir (-> (config/config-map) :datadir)
         builddir (->> [datadir "builds" build] (remove nil?) (apply io/file))]
     (.getPath builddir))))

(defn builddir-create [repo]
  (let [repodir (repo/repodir repo)
        commit (repo/head repodir)
        build (format "%s-%s" repo commit)
        builddir (builddir build)]
    (repo/clone repodir builddir)
    (repo/checkout builddir commit)
    builddir))

(defn builddir-delete [builddir]
  (run! io/delete-file
        (-> builddir (io/file) (file-seq) (reverse))))

(defn containerfile-search [base-dir]
  (first
   (for [dir ["." "deploy" "docker"]
         containerfile ["Containerfile" "Dockerfile"]
         :let [file (io/file base-dir dir containerfile)]
         :when (.exists file)]
     (.getCanonicalPath file))))

(defn build [dir app]
  (let [containerfile (containerfile-search dir)
        image (str app ":latest")
        command ["podman" "build" "-f" containerfile "-t" image]
        {:keys [exit]} (process/exec command {:dir dir :printer process/std-printer})]
    (when (zero? exit)
      image)))

(defn delete [app]
  (let [image (str app ":latest")
        command ["podman" "image" "rm" "-i" image]]
    (process/exec command)))
