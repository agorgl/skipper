(ns skipper.app
  (:refer-clojure :exclude [list])
  (:require
   [clojure.java.io :as io]
   [skipper.repo :as repo]
   [skipper.image :as image]
   [skipper.container :as container]))

(defn list []
  (let [repodir (repo/repodir)
        files (-> repodir (io/file) (.listFiles))]
    (->> files
         (sort)
         (filter #(.isDirectory %))
         (mapv #(.getName %)))))

(defn exists [app]
  (some #{app} (list)))

(defn create [app]
  (when-not (exists app)
    (let [repodir (repo/repodir app)]
      (repo/create repodir))))

(defn build [app]
  (let [builddir (image/builddir-create app)]
    (try
      (image/build builddir app)
      (finally
        (image/builddir-delete builddir)))))

(defn deploy [app]
  (when (container/pod-exists app)
    (when (container/pod-running app)
      (container/pod-stop app))
    (container/pod-delete app))
  (container/pod-create app)
  (container/pod-start app))

(defn logs [app]
  (when (container/pod-running app)
    (container/pod-logs app)))

(defn delete [app]
  (when (exists app)
    (when (container/pod-exists app)
      (when (container/pod-running app)
        (container/pod-stop app))
      (container/pod-delete app))
    (image/delete app)
    (repo/delete (repo/repodir app))))
