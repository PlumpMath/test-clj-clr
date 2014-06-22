(ns test-clj-clr.asynchronous-server-socket
  (:refer-clojure :exclude [send])
  (:require [test-clj-clr.utils :as u])
  (:use clojure.repl
        clojure.pprint
        [clojure.reflect :only [reflect]]
        [clojure.walk :only [prewalk postwalk]])
  (:import [System
            AsyncCallback]
           [System.Net.Sockets
            Socket
            SocketType
            ProtocolType
            SocketShutdown
            AddressFamily]
           [System.Net
            Dns
            IPHostEntry
            IPAddress
            IPEndPoint]
           [System.Threading
            ManualResetEvent]
           [System.Text
            Encoding]))

;; kind of horrible, isn't it?
;; kind of from http://msdn.microsoft.com/en-us/library/fx6588te(v=vs.110).aspx

(comment
  (do 
    (set! *print-length* 20)
    (set! *print-level* 20)))

;; StateObject. ----------------------------------------------

(def default-buffer-size (int 1024))

;; the mutable data in StateObject's going to mutate

(defrecord StateObject ;; these typehints are distressingly impotent
    [^Socket work-socket
     ^int buffer-size
     ^bytes buffer
     ^StringBuilder sb])

(defn make-StateObject 
  (^StateObject [] (make-StateObject {}))
  (^StateObject [opts]
    (map->StateObject
      (merge opts
        {:work-socket nil
         :buffer-size default-buffer-size
         :buffer (byte-array default-buffer-size)
         :sb (StringBuilder.)}))))

;; listener --------------------------------------

;; weird all-done thing 
(def ^ManualResetEvent all-done (ManualResetEvent. false))

(defn shutdown-socket [^Socket s]
  (.Shutdown s SocketShutdown/Both)
  (.Close s))

(defn send-callback [^IAsyncResult ar]
  (try
    (let [handler ^Socket (.AsyncState ar) ; Retrieve the socket from the state object.
          bytes-sent (.EndSend handler ar)] ; Complete sending the data to the remote device...
      (println (format "Sent {%s} bytes to client.", bytes-sent))
      (shutdown-socket handler))
    (catch Exception e
      (println (.ToString e)))))

(defn send [^Socket handler, ^String data]
  ; Convert the string data to byte data using ASCII encoding.
  (let [byte-data ^bytes (.GetBytes Encoding/ASCII data)]
    (.BeginSend handler ; Begin sending the data to the remote device.
      byte-data, 0, (.Length byte-data), 0,
      (gen-delegate AsyncCallback [x] (send-callback x))
      handler)))

(declare read-callback)

(defn begin-receive [^Socket handler, ^StateObject state]
  (.BeginReceive handler            
    (.buffer state), 0, (.buffer-size state), 0,
    (gen-delegate AsyncCallback [x] (read-callback x)),
    state))

(defn store-data [^StateObject state, bytes-read]
  (.Append ^StringBuilder (.sb state) ; not sure why this needs type hint :(
    (.GetString Encoding/ASCII
      (.buffer state), 0, (int bytes-read))))

(defn complete-read [^Socket handler, ^String content]
  (println
    (format "Read {%s} bytes from socket. \n Data : {%s}"
      (.Length content)
      content))
  (send handler, content))         ; Echo the data back to the client.

(defn read-callback [^IAsyncResult ar]
  (let [^StateObject state (.AsyncState ar)
        ^Socket handler    (.work-socket state)
        bytes-read         (int (.EndReceive handler ar))]
    (when (< 0 bytes-read)
      (store-data state, bytes-read) ; There  might be more data, so store the data received so far.
      (let [content (.. state sb (ToString))]
        (if (< -1 (.IndexOf content "<EOF>")) ; here's the stupid encoding thing
          (complete-read handler, content) 
          (begin-receive handler, state)))))) ; Not all data received. Get more.

(defn accept-callback [^IAsyncResult ar]                      
  (.Set all-done)                 ; Signal the main thread to continue
  (let [^Socket listener (.AsyncState ar) ; still weirds me, see line 74 in example
        ^Socket handler (.EndAccept listener ar)
        ^StateObject state (make-StateObject {:work-socket handler})]
    (begin-receive handler, state) ;; on to next step
    nil))

(defn get-local-end-point ^IPEndPoint []
  (IPEndPoint.
    ^IPAddress (first (.AddressList (Dns/GetHostByName (Dns/GetHostName))))
    11000))

(defn get-listener ^Socket []
  (Socket. AddressFamily/InterNetwork,
    SocketType/Stream,
    ProtocolType/Tcp))

(def kill-switch (atom false))

(defn begin-accept [^Socket listener]
  (.BeginAccept listener
    (gen-delegate AsyncCallback [x] (accept-callback x))
    listener))

(defn start-listening []
  (reset! kill-switch false)
  (let [^IPEndPoint local-end-point (get-local-end-point)
        ^Socket listener (get-listener)]
    (try
      (doto listener
        (.Bind local-end-point)
        (.Listen 100))

      (while (not @kill-switch) ;;supposed to be infinitish.
        ;; kill-switch is probably a huge async/comms problem, since
        ;; it doesn't clean anything up. A couple ways to ensure clean-up
        ;; occur to me, but let's get it working first.
        (.Reset all-done)
        (println
          (format "local end point: %s \n Waiting for connection..."
            local-end-point))
        (begin-accept listener)
        (.WaitOne all-done)) ;; this is mysterious to me; does it block? who?

      (catch Exception e
        (println (.ToString e))))
    (println "\nPress ENTER to continue...")
    (println "now I guess I need to tell it to continue somehow")
    (println "see loc 65 in example")))

(defn stop-listening [] (reset! kill-switch true))
