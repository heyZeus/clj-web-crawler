(ns http-client
  (:import (org.apache.commons.httpclient HttpClient NameValuePair URI)
           (org.apache.commons.httpclient.cookie CookiePolicy CookieSpec)
           (org.apache.commons.httpclient.methods GetMethod PostMethod DeleteMethod 
                                                  TraceMethod HeadMethod PutMethod)))


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
  (.getResponseBodyAsString method))

(defn scrape
  [client method]
  (send-method client method 
    (response-str method)))

(defn client 
  "Creates a HttpClient for the given server." 
  [server]
  (let [c (HttpClient.)]
    (.. c (getHostConfiguration) (setHost (URI. server true)))
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


(comment 

; Prints the HTML of the clojure.org website
(let [client (client "http://www.clojure.org")
      home  (method "/")] 
  (println (scrape client home)))  
)
