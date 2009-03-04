(ns http-client.test.main
  (:require [http-client :as hc])
  (:use clojure.contrib.test-is))

(def client (hc/client "http://www.clojure.org"))

(deftest client 
  (let [host "www.clojure.org"
        client (hc/client (str "http://" host))]
    (is (= (.. client (getHostConfiguration) getHost) host))))

(deftest method
   (let [default-get-method (hc/method "/")
         path-post-method (hc/method "/api" :post)
         params-method (hc/method "/" :get {:language "clojure" :happy "yes"})]
     (is (= "GET" (.getName default-get-method)))
     (is (= "/" (.getPath default-get-method)))

     (is (= "POST" (.getName path-post-method)))
     (is (= "/api" (.getPath path-post-method)))

     (is (= "POST" (.getName params-method)))
     (is (= "clojure" (.. params-method (getParameter "language") (getValue))))
     (is (= "yes" (.. params-method (getParameter "happy") (getValue))))))

(run-tests)
