(ns com.phronemophobic.easel.flowstorm-debugger
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [com.phronemophobic.easel.settings :as settings]
            [flow-storm.debugger.main :as debugger]
            [flow-storm.debugger.websocket])
  (:import [java.net InetSocketAddress]
           [org.java_websocket.server WebSocketServer]))

(defn loopback-server [{:keys [port on-message on-open on-close on-start]} ready-file]
  (proxy [WebSocketServer] [(InetSocketAddress. "127.0.0.1" (int port))]
    (onStart []
      (settings/write-data! ready-file {:port (.getPort ^WebSocketServer this)})
      (on-start))
    (onOpen [connection handshake] (when on-open (on-open connection)))
    (onMessage [connection message] (on-message connection message))
    (onClose [connection code reason remote?] (on-close code reason remote?))
    (onError [connection error] (.printStackTrace ^Throwable error))))

(defn -main [config-string]
  (let [{:keys [directory theme]} (edn/read-string config-string)
        closed (promise) started (promise)]
    ;; The pinned FlowStorm server binds all interfaces; this adapter keeps the bridge local.
    (alter-var-root (ns-resolve 'flow-storm.debugger.websocket 'create-ws-server)
                    (constantly #(loopback-server % (io/file directory "ready.edn"))))
    (future (try (read-line) (finally (deliver closed true))))
    (future
      (try
        (debugger/start-debugger {:local? false :ws-port 0 :theme theme
                                  :title "Easel · FlowStorm"})
        (spit (io/file directory "connected") "ready")
        (deliver started true)
        (catch Throwable e (.printStackTrace e) (deliver closed true))))
    (try
      (loop []
        (when (and (not (deref closed 200 false))
                   (or (not (realized? started)) @(ns-resolve 'flow-storm.debugger.ui.main 'ui)))
          (recur)))
      (finally (debugger/stop-debugger)))
    (shutdown-agents)
    (System/exit 0)))
