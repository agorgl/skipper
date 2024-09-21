(ns skipper.core
  (:require
   [clojure.string :as str]
   [clojure.java.io :as io]
   [clojure.core.match :as match]
   [skipper.cli :as cli]
   [skipper.command :as cmd])
  (:gen-class))

(defn version []
  (str/trim (slurp (io/resource "version"))))

(defn error-str [error & {:keys [command]}]
  (match/match error
    [:unknown-option opt]        (format "unknown option '%s'" opt)
    [:missing-required opt]      (format "missing required argument for '%s'" (str/join " " opt))
    [:parsing-error opt msg]     (format "error while parsing option '%s': %s" (str/join " " opt) msg)
    [:validation-error opt msg]  (format "failed to validate '%s': %s" (str/join " " opt) msg)
    [:unrecognized-command]      (format "unrecognized command '%s'" (str/join " " command))
    [:missing-command]           (format "missing command '%s'" (cli/usage cli/spec command))
    [:incorrect-arguments]       (format "incorrect arguments '%s'" (cli/usage cli/spec command))
    x (format "unknown error: %s" (->> x (map #(str/replace (name %) "-" " ")) (str/join " ")))))

(defn show-help [{:keys [command]}]
  (cli/help command))

(defn show-version [{:keys [command]}]
  (println (format "%s %s" (first command) (version))))

(defn show-error [{:keys [errors command] :as args}]
  (let [error (first errors)
        valid-command (if (= (first error) :unrecognized-command)
                        (drop-last command)
                        command)]
    (binding [*out* *err*]
      (println (format "error: %s" (error-str error args)))
      (println (format "see '%s --help'" (str/join " " valid-command))))))

(defn command [{:keys [command options arguments]}]
  (case (rest command)
    ["app" "create"] [:app-create (:app arguments)]
    ["app" "list"] [:app-list]
    ["app" "build"] [:app-build (:app arguments)]
    ["app" "deploy"] [:app-deploy (:app arguments)]
    ["app" "logs"] [:app-logs (:app arguments) options]
    ["app" "delete"] [:app-delete (:app arguments)]))

(defn dispatch [args]
  (let [command (command args)]
    (cmd/dispatch command)))

(defn -main [& args]
  (let [{:keys [options errors] :as args}
        (cli/parse-args cli/spec (into ["skipper"] args))]
    (cond
      (:help options) (show-help args)
      (:version options) (show-version args)
      (seq errors) (show-error args)
      :else (dispatch args))))
