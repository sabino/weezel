(ns com.phronemophobic.easel.model
  "The small protocols that connect applets to Easel.")


(defprotocol IResizable
  "An applet that can adapt to a pane's logical size and display scale."
  (-resize [this [w h] content-scale]
    "Return the resized applet. Sizes are logical pixels; content-scale is [x y]."))

(defprotocol IUI
  "An applet that produces a Membrane view from its current value."
  (-ui [this $context context]
    "Return a view. $context is the writable path to the shared UI context."))

(defprotocol IApplet
  "The resource lifecycle of an applet. Hiding a pane does not stop its applet."
  (-start [this $ref size content-scale]
    "Return the started applet. $ref addresses this instance in application state.
    Queue external work under ::queue so it runs after this value is installed.")
  (-stop [this]
    "Release watches, workers, and other resources when the applet is removed.
    Cleanup should be idempotent and tolerate a start that has not run yet."))

(defprotocol IEasel
  "Value transformations for applets and their placement in an Easel."
  (-add-applet [this info]
    "Return an Easel containing the applet made by (:make-applet info).
    The factory receives the effect handler. :pane-id can target an empty pane.")
  (-remove-applet [this id]
    "Remove an applet and queue its cleanup, including saved workspace references.")
  (-visible-applets [this]
    "Return the applets currently placed in visible panes.")
  #_(-ui [this])
  (-show-applet [this id] "Place an existing applet in a pane.")
  (-hide-applet [this id] "Remove its pane while keeping the applet alive.")
  (-applets [this] "Return the map from applet IDs to applet values."))

