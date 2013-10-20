(ns @@APPDOMAIN@@.core )

(use '[clojure.tools.logging :only (info warn error debug)])

(deftype MyAppMain [] 
  comzotohcljc.tardis.impl.ext.CljAppMain
  (contextualize [_ container]
    (info "My AppMain contextualized by container " container))
  (configure [_ options]
    (info "My AppMain configured with options " options))
  (initialize [_]
    (info "My AppMain initialized!"))
  (start [_]
    (info "My AppMain started"))
  (stop [_]
    (info "My AppMain stopped"))
  (dispose [_]
    (info "My AppMain finz'ed"))
)

(def ^:private core-eof nil)

