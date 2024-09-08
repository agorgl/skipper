(ns skipper.process-test
  (:require
   [clojure.test :refer :all]
   [clojure.string :as str]
   [clojure.core.async :as async]
   [clojure.java.io :as io]
   [skipper.process :refer :all])
  (:import
   [java.nio.charset StandardCharsets]))

(deftest test-line-chan
  (let [cases [["with single line input" 1]
               ["with few lines input" 32]
               ["with lots of lines input" 16384]]]
    (doseq [[t i] cases]
      (testing t
        (let [data (->> (range i) (mapv str))
              input (-> (str/join "\n" data)
                        (.getBytes  StandardCharsets/UTF_8)
                        (io/input-stream))
              channel (line-chan input)]
          (is (= data (async/<!! (async/into [] channel)))))))))

(deftest test-start
  (let [r (start ["bash" "-c" "echo hi"])]
    (is (= 0 @(:exit r)))
    (is (= (async/<!! (:streams r)) {:stream :out :value "hi"}))))

(deftest test-streams-reduce
  (testing "only stdout"
    (let [vals [{:stream :out :value "hi"}]
          stdc (async/to-chan! vals)
          [out err] (async/<!! (streams-reduce stdc))]
      (is (= "hi\n" out))
      (is (= nil err))))
  (testing "both stdout and stderr"
    (let [vals [{:stream :out :value "hi"}
                {:stream :err :value "oups"}]
          stdc (async/to-chan! vals)
          [out err] (async/<!! (streams-reduce stdc))]
      (is (= "hi\n" out))
      (is (= "oups\n" err)))))

(deftest test-wait
  (let [r (wait (start ["bash" "-c" "echo hi"]))]
    (is (= 0 (:exit r)))
    (is (= "hi\n" (:out r)))))

(deftest test-tap-streams
  (let [vals (->> (range 256) (mapv #(do {:stream :out :value %})))
        stdc (async/to-chan! vals)
        accm (atom [])
        stdc (tap-streams stdc #(swap! accm conj %))]
    (is (= vals (async/<!! (async/into [] stdc))))
    (is (= vals @accm))))

(deftest test-exec
  (testing "successful exit"
    (let [r (exec ["bash" "-c" "echo hi"])]
      (is (= 0 (:exit r)))
      (is (= "hi\n" (:out r)))))
  (testing "unsuccessful exit"
    (is (thrown? Exception (exec ["bash" "-c" "exit 1"]))))
  (testing "with printer"
    (let [out
          (with-out-str
            (let [r (exec ["bash" "-c" "echo hi"] {:printer std-printer})]
              (is (= 0 (:exit r)))
              (is (= "hi\n" (:out r)))))]
      (is (= "hi\n" out)))))
