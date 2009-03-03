(ns http-client
  (:import (org.apache.commons.httpclient HttpClient NameValuePair URI)
           (org.apache.commons.httpclient.cookie CookiePolicy CookieSpec)
           (org.apache.commons.httpclient.methods GetMethod PostMethod DeleteMethod 
                                                  TraceMethod HeadMethod PutMethod)))

(defmacro send-uri
  "Sends a request to the given uri and client.  The reponse from the server is 
  stored in the uri and any cookies are stored in the client.  The response and 
  any resources associated with the request are cleared from the uri after this
  function is called."
  [client uri & body]
  `(try 
     (.executeMethod ~client ~uri)
     ~@body
     (finally (.releaseConnection ~uri))))

(defn client 
  "Creates a HttpClient for the given server." 
  [server]
  (let [c (HttpClient.)]
    (.. c (getHostConfiguration) (setHost (URI. server true)))
    c))

(defn uri
  "Creates a commons-client method type object with the given path and method.  
  A method can be one of: :post, :get, :put, :delete, :trace or :head.  If no 
  method is supplied :get is the default.  You can supply a url-params hash like: 
  {:login \"foo\" :password \"bar\"}."
  ([path method url-params]
   (let [key-method (cond 
                      (> (count url-params) 0) :post 
                      (nil? method) :get 
                      :else (keyword method))
         m (cond 
             (= :post key-method) (PostMethod. path)
             (= :delete key-method) (DeleteMethod. path)
             (= :put key-method) (PutMethod. path)
             (= :trace key-method) (TraceMethod. path)
             (= :head key-method) (HeadMethod. path)
             (= :get key-method) (GetMethod. path)
             :else (GetMethod. path))]
     (doseq [[k v] url-params]
       (.addParameter m (name k) (str v)))
     m))
  ([path method] (uri path method nil)) 
  ([path] (uri path nil nil))) 

(defn cookies
  "Convience function to get the cookies from the client."
  [client]
  (.. client (getState) (getCookies)))

(defn print-cookies
  "Prints the cookies from the client."
  [client]
  (doseq [c (cookies client)] (println c)))

(defn response-str
  "Returns the response from the uri as a string."
  [client]
  [uri]
  (.getResponseBodyAsString uri))

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
      login (uri "/")] 
  (send-uri client login   
    (println (response-str login))))

)
