(ns test-clj-clr.core
  (:use clojure.repl clojure.pprint)
  (:import [System.Net.Sockets
            Socket
            SocketType
            ProtocolType
            SocketShutdown] ;DM: (java.net Socket ServerSocket InetSocketAddress)
           [System.Net
            IPAddress
            IPEndPoint]))
