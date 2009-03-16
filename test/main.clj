(ns clj-web-crawler.test.main
  (:require [clj-web-crawler :as wc])
  (:use clojure.contrib.test-is))

(def clj-ws (wc/client "http://www.clojure.org"))
(def home (wc/method "/"))
(def clj-home-page-text "Clojure is a dialect of Lisp")

(deftest client 
  (let [host "www.clojure.org"
        ws (wc/client (str "http://" host))]
    (is (= (.. ws (getHostConfiguration) getHost) host))))

(deftest method
   (let [default-get-method (wc/method "/")
         path-post-method (wc/method "/api" :post)
         params-method (wc/method "/" :get {:language "clojure" :happy "yes"})]
     (is (= "GET" (.getName default-get-method)))
     (is (= "/" (.getPath default-get-method)))

     (is (= "POST" (.getName path-post-method)))
     (is (= "/api" (.getPath path-post-method)))

     (is (= "/api" (.getPath (wc/method "api"))))

     (is (= "POST" (.getName params-method)))
     (is (= "clojure" (.. params-method (getParameter "language") (getValue))))
     (is (= "yes" (.. params-method (getParameter "happy") (getValue))))))

(deftest crawl
  (wc/crawl clj-ws home
    (is (.contains (wc/response-str home) clj-home-page-text)))
  (wc/crawl clj-ws home
    (is (.contains (wc/response-str home) clj-home-page-text)))
  (let [api (wc/method "/api")]
    (wc/crawl clj-ws api
      (is (.contains (wc/response-str api) "API")))))

(deftest crawl-response
   (is (.contains (wc/crawl-response "http://www.clojure.org" "/") clj-home-page-text))
   (is (.contains (wc/crawl-response "http://www.clojure.org" "/api") "API"))
   (is (.contains (wc/crawl-response (wc/client "http://www.clojure.org") (wc/method "/api")) 
                  "API"))
   (let [res (wc/crawl-response "http://www.ask.com" 
                                (wc/method "/web" :post {:q "clojure"}))]
     (is (.contains res "targets the Java Virtual Machine"))))

; this test depends on a website that i don't have any control over,  
; this test is fragile, but better than no test
(deftest redirect
  ; i setup this redirect at shorturl.com
  (let [redirect-site (wc/client "http://alturl.com/") 
        home (wc/method "/yew")] 
    (wc/crawl redirect-site home
      (is (.contains (wc/response-str home) clj-home-page-text))))) 

(deftest to-str
  (is (= "blah" (wc/to-str "blah")))
  (is (= "blah" (wc/to-str :blah)))
  (is (= "2" (wc/to-str 2))))

(deftest keys-values-to-strs
  (is (= {"blah" "blah" "1" "2" "three" "four"} 
         (wc/keys-values-to-strs {:blah :blah 1 2 "three" "four"}))))

(deftest cookie-names
  (wc/crawl clj-ws home)
  (is (= (wc/cookie-names clj-ws) #{"test" "master" "slave"})))
   
(run-tests)
