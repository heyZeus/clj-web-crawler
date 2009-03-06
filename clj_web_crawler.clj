(ns clj-web-crawler
  (:import (org.apache.commons.httpclient HttpClient NameValuePair URI HttpStatus)
           (org.apache.commons.httpclient.cookie CookiePolicy CookieSpec)
           (org.apache.commons.httpclient.methods GetMethod PostMethod DeleteMethod 
                                                  TraceMethod HeadMethod PutMethod))
  (:use [clojure.contrib.duck-streams :only (slurp*)]))


(defmacro send-method
  "Sends a request to the given method and client.  The reponse from the server is 
  stored in the method and any cookies are stored in the client.  The response and 
  any resources associated with the request are cleared from the method after this
  function is called."
  [client method & body]
  `(try 
     (.executeMethod ~client ~method)
     ~@body
     (finally (.releaseConnection ~method))))

(defn response-str
  "Returns the response from the method as a string."
  [method]
  ; uses slurp* here otherwise we get a annoying warning from commons-client
  (slurp* (.getResponseBodyAsStream method)))

(defmacro scrape
  "Returns the HTML as a string. It will free up any resources associated 
  with the method. If the resulting page is a redirect the redirect page 
  will be returned.  Also the optional body will be run against the 
  redirected page."
  [client method & body]
  `(send-method ~client ~method
    (let [location# (redirect-location ~method)]
      (if location#
        (do 
          (let [redirect-method# (method location#)]
            (send-method ~client redirect-method#
              ~@body
              (response-str redirect-method#))))
        (do 
          ~@body
          (response-str ~method))))))

(defn client 
  "Creates a HttpClient for the given server." 
  [host]
  (let [c (HttpClient.)]
    (.. c (getHostConfiguration) (setHost (URI. host true)))
    c))

(defn method
  "Creates a commons-client method type object with the given path and type.  
  A type can be one of: :post, :get, :put, :delete, :trace or :head.  If no 
  type is supplied :get is the default.  You can supply a url-params hash like: 
  {:login \"foo\" :password \"bar\"}."
  ([path type url-params]
   (let [key-type (cond 
                      (> (count url-params) 0) :post 
                      (nil? type) :get 
                      :else (keyword type))
         m (cond 
             (= :post key-type) (PostMethod. path)
             (= :delete key-type) (DeleteMethod. path)
             (= :put key-type) (PutMethod. path)
             (= :trace key-type) (TraceMethod. path)
             (= :head key-type) (HeadMethod. path)
             :else (GetMethod. path))]
     (doseq [[k v] url-params]
       (.addParameter m (name k) (str v)))
     m))
  ([path type] (method path type nil)) 
  ([path] (method path nil nil))) 

(defn cookies
  "Convience function to get the cookies from the client."
  [client]
  (.. client (getState) (getCookies)))

(defn print-cookies
  "Prints the cookies from the client."
  [client]
  (doseq [c (cookies client)] (println c)))

(defn assert-cookie-names
  "Returns true if all of the given cookie-names exist in the client."
  [client & cookie-names]
  (let [actual-cookies (cookies client)]
    (every? (fn [exp-cookie-name] 
              (some #(= exp-cookie-name (.getName %1)) actual-cookies))
            cookie-names)))

(defn redirect-location
  "Returns the redirection location string in the method, nil or false if
  not being redirected."
  [method]
  (let [status-code (.getStatusCode method)
        header (.getResponseHeader method "location")]
    (if (or (= status-code (HttpStatus/SC_MOVED_TEMPORARILY))
            (= status-code (HttpStatus/SC_MOVED_PERMANENTLY))
            (= status-code (HttpStatus/SC_SEE_OTHER))
            (= status-code (HttpStatus/SC_TEMPORARY_REDIRECT)))
      (if-let [location (and header (.getValue header))]
        location))))

(comment 

; Prints the HTML of the clojure.org website
(let [server (client "http://www.clojure.org")
      home  (method "/")] 
  (println (scrape server home)))  

; If you don't care about the HTML from the query you should just call
; send-method. In this example you are posting the login form and need 
; to make sure a cookie is set to validate the login was successful.
(let [server (client "http://www.example.com")
      login  (method "/accounts/login" :post {:login "mr_cool" :password "clojurerox"})] 
  (send-method server login) 
  (if (assert-cookie-names server "username")  
    (println "yeah, I'm in")
    (println "i can't remember my password again!")))

; You can also pass in a body to the send-method macro to do something
; like check the response status code. Note you can't check the response
; code outside of the send-method call since all associated resources are
; released at that point.
(let [server (client "http://www.clojure.org")
             login  (method "/")]
  (send-method server login 
    (println (.getStatusCode login))))

)


