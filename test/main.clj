(ns clj-web-crawler.test.main
  (:require [clj-web-crawler :as wc])
  (:use clojure.contrib.test-is))

(def clj-ws (wc/client "http://www.clojure.org"))
(def home (wc/method "/"))

(deftest client 
  (let [host "www.clojure.org"
        clj-ws (wc/client (str "http://" host))]
    (is (= (.. clj-ws (getHostConfiguration) getHost) host))))

(deftest method
   (let [default-get-method (wc/method "/")
         path-post-method (wc/method "/api" :post)
         params-method (wc/method "/" :get {:language "clojure" :happy "yes"})]
     (is (= "GET" (.getName default-get-method)))
     (is (= "/" (.getPath default-get-method)))

     (is (= "POST" (.getName path-post-method)))
     (is (= "/api" (.getPath path-post-method)))

     (is (= "POST" (.getName params-method)))
     (is (= "clojure" (.. params-method (getParameter "language") (getValue))))
     (is (= "yes" (.. params-method (getParameter "happy") (getValue))))))

(deftest scrape
  (let [html (wc/scrape clj-ws home)]
    (is (= (.contains html "Clojure")))))

(run-tests)
