(ns skipper.cli-test
  (:require
   [clojure.test :refer :all]
   [clojure.string :as str]
   [skipper.cli :refer :all :exclude [spec]]))

(def spec
  {:name "farmctl"
   :desc "Manage farm barns and animals"
   :opts [{:name "farm"
           :desc "Farm to manage"
           :alias "f"
           :type :string}
          {:name "version"
           :desc "Show program version"
           :alias "v"}
          {:name "help"
           :desc "Show help summary"
           :alias "h"}]
   :cmds [{:name "barn"
           :desc "Manage barns"
           :cmds [{:name "add"
                   :desc "Add barn"
                   :args [{:name "name"}]}
                  {:name "ls"
                   :desc "List barns"}
                  {:name "rm"
                   :desc "Remove barn"
                   :args [{:name "name"}]}]}
          {:name "animal"
           :desc "Manage animals"
           :opts [{:name "barn"
                   :desc "Barn to manage"
                   :alias "b"
                   :type :string}]
           :cmds [{:name "add"
                   :desc "Add animal"
                   :args [{:name "name"}]}
                  {:name "ls"
                   :desc "List animals"}
                  {:name "rm"
                   :desc "Remove animal"
                   :args [{:name "name"}]}]}]})

(deftest test-find-item
  (let [coll [{:name "a"} {:name "b"} {:name "c"}]]
    (testing "item exists in collection"
      (is (= {:name "b"} (find-item coll "b"))))
    (testing "item doesn't exist in collection"
      (is (= nil (find-item coll "foo"))))))

(deftest test-cmd-spec
  (testing "command exists"
    (is (= spec (cmd-spec spec ["farmctl"])))
    (is (= (-> (get-in spec [:cmds 0])
               (update :opts #(into (or % []) (get-in spec [:opts]))))
           (cmd-spec spec ["farmctl" "barn"])))
    (is (= (-> (get-in spec [:cmds 0 :cmds 1])
               (update :opts #(into (or % []) (get-in spec [:cmds 0 :opts])))
               (update :opts #(into (or % []) (get-in spec [:opts]))))
           (cmd-spec spec ["farmctl" "barn" "ls"]))))
  (testing "command doesn't exist"
    (is (= nil (cmd-spec spec ["farmctl" "foo"])))))

(deftest test-usage
  (testing "command exists"
    (is (= "farmctl [options] [command]" (usage spec ["farmctl"])))
    (is (= "farmctl barn [options] [command]" (usage spec ["farmctl" "barn"])))
    (is (= "farmctl barn ls [options]" (usage spec ["farmctl" "barn" "ls"]))))
  (testing "command doesn't exist"
    (is (= nil (usage spec ["farmctl" "foo"])))))

(deftest test-summary
  (testing "command exists"
    (is (= ["Manage farm barns and animals"
            ""
            "Usage:"
            "  farmctl [options] [command]"
            ""
            "Commands:"
            "  barn     Manage barns"
            "  animal   Manage animals"
            ""
            "Options:"
            "  -f, --farm      Farm to manage"
            "  -v, --version   Show program version"
            "  -h, --help      Show help summary"]
           (-> (summary spec ["farmctl"]) (str/split-lines))))
    (is (= ["Manage barns"
            ""
            "Usage:"
            "  farmctl barn [options] [command]"
            ""
            "Commands:"
            "  add      Add barn"
            "  ls       List barns"
            "  rm       Remove barn"
            ""
            "Options:"
            "  -f, --farm      Farm to manage"
            "  -v, --version   Show program version"
            "  -h, --help      Show help summary"]
           (-> (summary spec ["farmctl" "barn"]) (str/split-lines))))
    (is (= ["List barns"
            ""
            "Usage:"
            "  farmctl barn ls [options]"
            ""
            "Options:"
            "  -f, --farm      Farm to manage"
            "  -v, --version   Show program version"
            "  -h, --help      Show help summary"]
           (-> (summary spec ["farmctl" "barn" "ls"]) (str/split-lines)))))
  (testing "command doesn't exist"
    (is (= nil (summary spec ["farmctl" "foo"])))))

(deftest test-help
  (with-redefs [skipper.cli/spec spec]
    (testing "prints summary"
      (is (= ["Manage farm barns and animals"
              ""
              "Usage:"
              "  farmctl [options] [command]"
              ""
              "Commands:"
              "  barn     Manage barns"
              "  animal   Manage animals"
              ""
              "Options:"
              "  -f, --farm      Farm to manage"
              "  -v, --version   Show program version"
              "  -h, --help      Show help summary"]
             (-> (with-out-str (help)) (str/split-lines)))))
    (testing "prints command summary"
      (is (= ["Manage barns"
              ""
              "Usage:"
              "  farmctl barn [options] [command]"
              ""
              "Commands:"
              "  add      Add barn"
              "  ls       List barns"
              "  rm       Remove barn"
              ""
              "Options:"
              "  -f, --farm      Farm to manage"
              "  -v, --version   Show program version"
              "  -h, --help      Show help summary"]
             (-> (with-out-str (help ["farmctl" "barn"])) (str/split-lines)))))))

(deftest test-parse-error
  (testing "with unknown option"
    (is (= [:unknown-option "-g"]
           (parse-error "Unknown option: \"-g\""))))
  (testing "with missing required"
    (is (= [:missing-required ["-f" "FARM"]]
           (parse-error "Missing required argument for \"-f FARM\""))))
  (testing "with parsing error"
    (is (= [:parsing-error ["-f" "foo"] "java.lang.NumberFormatException: For input string: \"foo\""]
           (parse-error "Error while parsing option \"-f foo\": java.lang.NumberFormatException: For input string: \"foo\""))))
  (testing "with validation error"
    (is (= [:validation-error ["-f" "0"] nil]
           (parse-error "Failed to validate \"-f 0\"")))
    (is (= [:validation-error ["-f" "0"] "Value must be positive"]
           (parse-error "Failed to validate \"-f 0\": Value must be positive")))))

(deftest test-parse-args
  (testing "valid options"
    (is (= {:command ["farmctl"],
            :options {:help true},
            :arguments {},
            :errors [[:missing-command]]}
           (parse-args spec ["farmctl" "-h"])))
    (is (= {:command ["farmctl"],
            :options {:help true},
            :arguments {},
            :errors [[:missing-command]]}
           (parse-args spec ["farmctl" "--help"]))))
  (testing "valid command"
    (is (= {:command ["farmctl" "barn" "ls"],
            :options {},
            :arguments {},
            :errors []}
           (parse-args spec ["farmctl" "barn" "ls"]))))
  (testing "valid command with valid options"
    (is (= {:command ["farmctl" "animal" "ls"],
            :options {:help true},
            :arguments {},
            :errors []}
           (parse-args spec ["farmctl" "animal" "ls" "-h"])))
    (is (= {:command ["farmctl" "animal" "ls"],
            :options {:barn "foo"},
            :arguments {},
            :errors []}
           (parse-args spec ["farmctl" "animal" "ls" "-b" "foo"]))))
  (testing "valid command with valid arguments"
    (is (= {:command ["farmctl" "barn" "add"],
            :options {},
            :arguments {:name "foo"},
            :errors []}
           (parse-args spec ["farmctl" "barn" "add" "foo"]))))
  (testing "valid command with valid options and arguments"
    (is (= {:command ["farmctl" "animal" "add"],
            :options {:barn "foo"},
            :arguments {:name "moo"},
            :errors []}
           (parse-args spec ["farmctl" "animal" "add" "-b" "foo" "moo"]))))
  (testing "unrecognized command"
    (is (= {:command ["farmctl" "foo"],
            :options {},
            :arguments {},
            :errors [[:unrecognized-command]]}
           (parse-args spec ["farmctl" "foo"])))
    (is (= {:command ["farmctl" "barn" "foo"],
            :options {},
            :arguments {},
            :errors [[:unrecognized-command]]}
           (parse-args spec ["farmctl" "barn" "foo"]))))
  (testing "missing command"
    (is (= {:command ["farmctl"],
            :options {},
            :arguments {},
            :errors [[:missing-command]]}
           (parse-args spec ["farmctl"])))
    (is (= {:command ["farmctl" "barn"],
            :options {},
            :arguments {},
            :errors [[:missing-command]]}
           (parse-args spec ["farmctl" "barn"]))))
  (testing "unknown option"
    (is (= {:command ["farmctl"],
            :options {},
            :arguments {},
            :errors [[:unknown-option "-g"] [:missing-command]]}
           (parse-args spec ["farmctl" "-g"])))
    (is (= {:command ["farmctl" "barn" "ls"],
            :options {},
            :arguments {},
            :errors [[:unknown-option "-g"]]}
           (parse-args spec ["farmctl" "barn" "ls" "-g"])))))
