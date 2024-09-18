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
           :alias "f"}
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
                   :alias "b"}]
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
    (is (= (-> spec :cmds first) (cmd-spec spec ["farmctl" "barn"])))
    (is (= (-> spec :cmds first :cmds second) (cmd-spec spec ["farmctl" "barn" "ls"]))))
  (testing "command doesn't exist"
    (is (= nil (cmd-spec spec ["farmctl" "foo"])))))

(deftest test-usage
  (testing "command exists"
    (is (= "farmctl [options] [command]" (usage spec ["farmctl"])))
    (is (= "farmctl barn [command]" (usage spec ["farmctl" "barn"])))
    (is (= "farmctl barn ls" (usage spec ["farmctl" "barn" "ls"]))))
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
            "  farmctl barn [command]"
            ""
            "Commands:"
            "  add      Add barn"
            "  ls       List barns"
            "  rm       Remove barn"]
           (-> (summary spec ["farmctl" "barn"]) (str/split-lines))))
    (is (= ["List barns"
            ""
            "Usage:"
            "  farmctl barn ls"]
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
              "  farmctl barn [command]"
              ""
              "Commands:"
              "  add      Add barn"
              "  ls       List barns"
              "  rm       Remove barn"]
             (-> (with-out-str (help ["farmctl" "barn"])) (str/split-lines)))))))
