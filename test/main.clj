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


; this test depends on a website that i don't have any control over,  
; this test is fragile, but better than no test
(deftest cookie-names
  (wc/send-method clj-ws home)
  ;(println "Cookies from clojure.org ") 
  ;(wc/print-cookies clj-ws)
  (is (wc/assert-cookie-names clj-ws "test" "master"))
  (is (not (wc/assert-cookie-names clj-ws "does-not-exist-at-all"))))

; this test depends on a website that i don't have any control over,  
; this test is fragile, but better than no test
(deftest redirect
  ; i setup this redirect at shorturl.com
  (let [redirect-site (wc/client "http://alturl.com/") 
        home (wc/method "/yew")] 
    (wc/crawl redirect-site home
      (is (.contains (wc/response-str home) clj-home-page-text))))) 
   
(run-tests)
