(ns test-clj-clr.core
  (:refer-clojure :exclude [send])
  (:require [test-clj-clr.utils :as u])
  (:use clojure.repl
        clojure.pprint
        [clojure.reflect :only [reflect]]
        [clojure.walk :only [prewalk postwalk]])
  (:import [System.Net.Sockets
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
            ManualResetEvent]))

(do 
  (set! *print-length* 20)
  (set! *print-level* 20))

;; StateObject. ----------------------------------------------

(defrecord StateObject
    [^Socket work-socket
     ^int buffer-size
     ^bytes buffer
     ^StringBuilder sb])

(defn make-StateObject 
  ([] (make-StateObject {}))
  ([opts]
     (let [bs (int 1024)]
       (map->StateObject
         (merge opts
           {:work-socket nil
            :buffer-size bs
            :buffer (byte-array bs)
            :sb (StringBuilder.)})))))
n
;; listener --------------------------------------

;; weird all-done thing 
(def ^ManualResetEvent all-done (ManualResetEvent. false))

(defn send-callback [])

(defn send []
  :bla)

(defn read-callback)

(defn accept-callback [^IAsyncResult ar]
  ;;Signal the main thread to continue
  (.Set all-done)
  (let [^Socket listener (ar.AsyncState) ;; still weirds me, see line 74 in example
        ^Socket handler (.EndAccept listener ar)
        ^StateObject state (make-StateObject {:work-socket handler})] 
    (.BeginReceive handler
      (:buffer state), 0, (:buffer-size state), 0,
      (AsyncCallback. read-callback),
      state)
    nil))

(defn get-local-end-point []
  (let [^IPAddress ipAddress (->
                               (Dns/GetHostName)
                               (Dns/GetHostByName)
                               .AddressList
                               first)]
    (IPEndPoint. ipAddress, 11000)))

(defn get-listener []
  (Socket. AddressFamily/InterNetwork,
    SocketType/Stream,
    ProtocolType/Tcp))

(def kill-switch (atom false))

(defn start-listening []
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

