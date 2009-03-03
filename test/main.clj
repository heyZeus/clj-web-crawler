(ns http-client.test.main
  (:require [http-client :as hc])
  (:use clojure.contrib.test-is))

(def client (hc/client "http://www.clojure.org"))

(deftest client 
  (let [host "www.clojure.org"
        client (hc/client (str "http://" host))]
    (is (= (.. client (getHostConfiguration) getHost) host))))

(deftest uri
   (let [default-get-uri (hc/uri "/")
         path-post-uri (hc/uri "/api" :post)
         params-uri (hc/uri "/" :get {:language "clojure" :happy "yes"})]
     (is (= "GET" (.getName default-get-uri)))
     (is (= "/" (.getPath default-get-uri)))

     (is (= "POST" (.getName path-post-uri)))
     (is (= "/api" (.getPath path-post-uri)))

     (is (= "POST" (.getName params-uri)))
     (is (= "clojure" (.. params-uri (getParameter "language") (getValue))))
     (is (= "yes" (.. params-uri (getParameter "happy") (getValue))))))

(run-tests)
