(ns skipper.repo
  (:require
   [clojure.string :as str]
   [clojure.java.io :as io]
   [skipper.config :as config]
   [skipper.process :as process]))

(defn repodir
  ([]
   (repodir nil))
  ([repo]
   (let [datadir (-> (config/config-map) :datadir)
         repodir (->> [datadir "repos" repo] (remove nil?) (apply io/file))]
     (.getPath repodir))))

(defn create [repodir]
  (let [command ["git" "init" "--bare" repodir]]
    (process/exec command {:printer process/std-printer})
    repodir))

(defn clone [repodir targetdir]
  (let [command ["git" "clone" repodir targetdir]]
    (process/exec command {:printer process/std-printer})))

(defn head [repodir]
  (let [command ["git" "rev-parse" "--short" "HEAD"]
        {:keys [exit out]} (process/exec command {:dir repodir :throw? false})]
    (if (zero? exit)
      (str/trim-newline out)
      (let [msg (format "could not find HEAD commit for repository in `%s` (repository is empty?)" repodir)]
        (throw (ex-info msg {}))))))

(defn checkout [repodir commit]
  (let [command ["git" "checkout" commit]]
    (process/exec command {:dir repodir :printer process/std-printer})))

(defn delete [repodir]
  (run! io/delete-file
        (-> repodir (io/file) (file-seq) (reverse))))
