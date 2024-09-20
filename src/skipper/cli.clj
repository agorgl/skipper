(ns skipper.cli
  (:require
   [clojure.string :as str]))

(def spec
  {:name "skipper"
   :desc "Make deployments simple"
   :opts [{:name "version"
           :desc "Show program version"
           :alias "v"}
          {:name "help"
           :desc "Show help summary"
           :alias "h"}]
   :cmds [{:name "app"
           :desc "Manage apps"
           :cmds [{:name "create"
                   :desc "Create a new app"
                   :args [{:name "app"}]}
                  {:name "list"
                   :desc "List apps"}
                  {:name "build"
                   :desc "Build and prepare an app for deployment"
                   :args [{:name "app"}]}
                  {:name "deploy"
                   :desc "Deploy an app instance"
                   :args [{:name "app"}]}
                  {:name "logs"
                   :desc "Fetch logs for running app instance"
                   :args [{:name "app"}]}
                  {:name "delete"
                   :desc "Remove an app"
                   :args [{:name "app"}]}]}]})

(defn find-item
  "Finds the first item in a collection of maps
   that has a :name value equal to n."
  [coll n]
  (->> coll
       (filter #(= (:name %) n))
       (first)))

(defn cmd-spec
  "Recursively searches spec for (sub)spec with matching (sub)command.
   All (sub)commands recursively inherit their parents' options.
   Returns matching spec or nil when no match found."
  [spec cmd]
  (loop [spec {:cmds [spec] :opts []}
         cmd cmd]
    (if (seq cmd)
      (let [subspec (find-item (:cmds spec) (first cmd))
            subcmd (rest cmd)]
        (if subspec
          (recur (update subspec :opts #(into (or % []) (:opts spec)))
                 subcmd)
          (when (:args spec)
            spec)))
      spec)))

(defn usage
  "Constructs usage string for command in spec."
  [spec cmd]
  (when-let [{:keys [opts args cmds]} (cmd-spec spec cmd)]
    (->> [(str/join " " cmd)
          (when (some? opts) "[options]")
          (when (some? cmds) "[command]")
          (when (some? args)
            (let [format-arg
                  (fn [{:keys [name optional]}]
                    (let [fmt (if optional "[%s]" "<%s>")]
                      (format fmt name)))]
              (->> args
                   (map format-arg)
                   (str/join " "))))]
         (filter some?)
         (str/join " "))))

(defn summary
  "Constructs summary string for command in spec."
  [spec cmd]
  (let [{:keys [desc cmds opts]} (cmd-spec spec cmd)
        usage (usage spec cmd)
        str-cmd (fn [{:keys [name desc]}]
                  (format "%-8s %s" name desc))
        str-opt (fn [{:keys [name alias desc]}]
                  (format "-%s, --%-8s  %s" alias name desc))
        str-section (fn [[title lines]]
                      (let [lines (map #(str "  " %) lines)
                            section-lines (into [title] lines)]
                        (str/join "\n" section-lines)))
        sections (-> []
                     (cond-> desc
                       (conj [desc]))
                     (cond-> usage
                       (conj ["Usage:" [usage]]))
                     (cond-> (seq cmds)
                       (conj ["Commands:" (map str-cmd cmds)]))
                     (cond-> (seq opts)
                       (conj ["Options:" (map str-opt opts)])))]
    (when (seq sections)
      (->> sections
           (map str-section)
           (str/join "\n\n")))))

(defn help
  "Prints command summary to stdout."
  ([]
   (help [(:name spec)]))
  ([cmd]
   (println (summary spec cmd))))
