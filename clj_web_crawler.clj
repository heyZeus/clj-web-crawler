(ns clj-web-crawler
  (:import (org.apache.commons.httpclient HttpClient NameValuePair URI HttpStatus)
           (org.apache.commons.httpclient.cookie CookiePolicy CookieSpec)
           (org.apache.commons.httpclient.methods GetMethod PostMethod DeleteMethod 
                                                  TraceMethod HeadMethod PutMethod)))

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

(defn to-str 
  "Converts a value to a string, accounts for keyword"
  [s] 
  (if (keyword? s) (name s) (str s)))

(defn keys-values-to-strs
  "Converts the given map keys and values to strings."
  [map1]
  (apply hash-map (mapcat (fn [[k v]] [(to-str k) (to-str v)]) map1))) 

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
         p (if (.startsWith path "/") path (str \/ path))
         m (cond 
             (= :post key-type) (PostMethod. p)
             (= :delete key-type) (DeleteMethod. p)
             (= :put key-type) (PutMethod. path)
             (= :trace key-type) (TraceMethod. p)
             (= :head key-type) (HeadMethod. p)
             :else (GetMethod. p))]
     (doseq [[k v] (keys-values-to-strs url-params)]
       (.addParameter m k v))
     m))
  ([path type] (method path type nil)) 
  ([path] (method path nil nil))) 

(defn client 
  "Creates a HttpClient for the given server." 
  [host]
  (let [c (HttpClient.)]
    (.. c (getHostConfiguration) (setHost (URI. host true)))
    c))

(defmacro crawl
  "Sends an HTTP request to the server. Pass in a body to examine
  the status code, response, etc.  All resource associated with 
  the method will be freed up at the end of the macro."
  ([#^org.apache.commons.httpclient.HttpClient server 
    #^org.apache.commons.httpclient.HttpMethodBase method & body]
    `(try
       (.executeMethod ~server ~method)
       ~@body
       (finally (.releaseConnection ~method))))
  ([server] (crawl server (method "/"))))

(defn response-reader
  "Returns the response from the method as a java.io.Reader.

  Should be used inside with-open to ensure the Reader is properly
  closed."
  ([method]
     (clojure.java.io/reader (.getResponseBodyAsStream method) :encoding (.getResponseCharSet method)))
  ([method client]
   (let [redirect (redirect-location method)
         new-method (if redirect (method redirect))]
     (if new-method
       (crawl client new-method
         (response-reader new-method))
       (response-reader method)))))

(defn response-str
  "Returns the response from the method as a string."
  ([method]
     (with-open [reader (response-reader method)]
       (slurp reader)))
  ([method client]
     (with-open [reader (response-reader method client)]
       (slurp reader))))

(defmulti crawl-response (fn [server method] [(class server) (class method)]))

(defmethod crawl-response 
   [String String] [server http-method]
   (let [c (client server)
         m (method http-method)]
     (crawl c m
       (response-str m c))))

(defmethod crawl-response 
  [String org.apache.commons.httpclient.HttpMethodBase] [server http-method]
  (let [c (client server)]
    (crawl c http-method
      (response-str http-method c))))

(defmethod crawl-response 
  [org.apache.commons.httpclient.HttpClient String] 
  [server http-method]
  (let [m (method http-method)]
    (crawl server m
      (response-str m server))))

(defmethod crawl-response 
  [org.apache.commons.httpclient.HttpClient org.apache.commons.httpclient.HttpMethodBase] 
  [server http-method]
  (crawl server http-method
    (response-str http-method server)))

(defn cookies
  "Convience function to get the cookies from the client."
  [client]
  (.. client getState getCookies))

(defn print-cookies
  "Prints the cookies from the client."
  [client]
  (doseq [c (cookies client)] (println c)))

(defn cookie-map
  [client]
  (reduce (fn [ret cookie] (conj ret [(.getName cookie) (.getValue cookie)]))
          {}
          (cookies client)))

(defn cookie-names
  [client]
  (set (keys (cookie-map client))))





