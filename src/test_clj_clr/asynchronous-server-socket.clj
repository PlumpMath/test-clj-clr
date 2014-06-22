(ns test-clj-clr.core ;; WRONG name. Change this by next session
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

(do 
  (set! *print-length* 20)
  (set! *print-level* 20))

;; StateObject. ----------------------------------------------

(def default-buffer-size (int 1024))

(defrecord StateObject
    [^Socket work-socket
     ^int buffer-size
     ^bytes buffer
     ^StringBuilder sb])

(defn make-StateObject 
  (^StateObject [] (make-StateObject {}))
  (^StateObject [opts]
    (let [bs default-buffer-size]
       (map->StateObject
         (merge opts
           {:work-socket nil
            :buffer-size bs
            :buffer (byte-array bs)
            :sb (StringBuilder.)})))))

;; listener --------------------------------------

;; weird all-done thing 
(def ^ManualResetEvent all-done (ManualResetEvent. false))

(defn send-callback [^IAsyncResult ar]
  (try
    (let [handler ^Socket (.AsyncState ar) ; Retrieve the socket from the state object.
          bytes-sent (.EndSend handler ar)] ; Complete sending the data to the remote device...
      (println "Sent {0} bytes to client.", bytes-sent)
      (.Shutdown handler SocketShutdown/Both)
      (.Close handler))))

(defn send [^Socket handler, ^String data]
  ; Convert the string data to byte data using ASCII encoding.
  (let [byte-data ^bytes (.GetBytes Encoding/ASCII data)]
    (.BeginSend handler ; Begin sending the data to the remote device.
      byte-data, 0, (.Length byte-data), 0,
      (gen-delegate AsyncCallback [x] (send-callback x))
      handler)))

(defn read-callback [^IAsyncResult ar]
  (let [content  String/Empty
        ^StateObject state (.AsyncState ar)
        ^Socket handler (.work-socket state)
        bytes-read (int (.EndReceive handler ar))]
    (when (< 0 bytes-read)
      (.. state sb ; There  might be more data, so store the data received so far.
        (Append
          (.GetString Encoding/ASCII
            (.buffer state), 0, bytes-read)))
      (let [content (.. state sb (ToString))]
        (if (< -1 (.IndexOf content "<EOF>")) ;; here's the stupid encoding thing
          (do
            (println "Read {0} bytes from socket. \n Data : {1}")
            (send handler, content)) ; Echo the data back to the client.
          (.BeginReceive handler    ; Not all data received. Get more.
            (.buffer state), 0, (.buffer-size state), 0,
            (gen-delegate AsyncCallback [x] (read-callback x)),
            state))))))

(defn accept-callback [^IAsyncResult ar]                      
  (.Set all-done)                 ; Signal the main thread to continue
  (let [^Socket listener (.AsyncState ar) ; still weirds me, see line 74 in example
        ^Socket handler (.EndAccept listener ar)
        ^StateObject state (make-StateObject {:work-socket handler})] 
    (.BeginReceive handler              
      (:buffer state), 0, (:buffer-size state), 0,
      ;; sketched out by this next thing:
      (gen-delegate AsyncCallback [x] (read-callback x)),   ; on to next step
      state)
    nil))

(defn get-local-end-point []
  (IPEndPoint.
    ^IPAddress (->
                 (Dns/GetHostName)
                 (Dns/GetHostByName)
                 .AddressList
                 first)
    11000))

(defn get-listener []
  (Socket. AddressFamily/InterNetwork,
    SocketType/Stream,
    ProtocolType/Tcp))

(def kill-switch (atom false))

(defn start-listening []
  (reset! kill-switch false)
  (let [^IPEndPoint local-end-point (get-local-end-point)
        ^Socket listener (get-listener)]
    (try
      (doto listener
        (.Bind local-end-point)
        (.Listen 100))

      (while (not @kill-switch) ;;supposed to be infinitish
        (.Reset all-done)
        (println "Waiting for connection...")
        (.BeginAccept socket
          (AsyncCallback. accept-callback)
          listener)
        (.WaitOne all-done))

      (catch Exception e
        (println (.ToString e))))
    (println "\nPress ENTER to continue...")
    (println "now I guess I need to tell it to continue somehow")
    (println "see loc 65 in example")))

(defn stop-listening (reset! kill-switch true))
