(ns com.phronemophobic.easel.guides
  "Guides are data: prose, runnable forms, and links into the running system.")

(def guides
  '[{:id :start
    :title "A workspace you can ask questions of"
    :paragraphs
    ["Easel is a native, REPL-driven workspace built from Clojure values and Membrane views. Its tools are applets: values that render, resize, and manage their own resources."
     "Start with Guides, browse the loaded Namespaces, or visit Live applets to inspect the workspace you are using. Follow a var to its metadata and source, then return with Back."
     "Examples run only when you press Run. Each Documentation applet owns a scratch namespace; definitions persist between examples until the applet closes. Output appears here and in Logs. You can edit an example in Clobber and evaluate it there."
     "From a terminal, start with clojure -M, then evaluate (require '[com.phronemophobic.easel :as easel]) and (easel/run). A running REPL keeps the environment open to inspection and redefinition."]
    :examples
    [{:title "Ask the runtime about its contracts"
      :source "(require '[com.phronemophobic.easel.model :as model])\n(select-keys (meta #'model/-start) [:name :arglists :doc])"}]
    :links [{:kind :guide :id :applets}
            {:kind :guide :id :components}
            {:kind :guide :id :repl}
            {:kind :guide :id :lifecycle}
            {:kind :guide :id :documentation}
            {:kind :var :symbol com.phronemophobic.easel/run}]}
   {:id :applets
    :title "An applet is a value with three contracts"
    :paragraphs
    ["IUI produces a Membrane view. IResizable returns a value adapted to a logical size and display scale. IApplet starts and stops the resources that surround that value. Easel expects applets to implement all three."
     "For a small tool, tool-applet supplies these contracts. Give it a label, a view var, and initial state. Its view receives :state, :$state, :size, and the shared context. Keeping the view as a var makes redefinition visible to the running workspace."
     "An applet factory receives Easel's effect handler. Dispatch :com.phronemophobic.easel/add-applet with {:make-applet factory} to open one. UI event handlers return intent vectors; background work uses the handler directly."]
    :examples
    [{:title "Construct a tool without opening a pane"
      :source "(require '[com.phronemophobic.easel.applet :as applet]\n         '[com.phronemophobic.easel.model :as model]\n         '[membrane.ui :as ui])\n(defn greeting-view [{:keys [state]}]\n  (ui/label (str \"Hello, \" (:name state))))\n(def greeting (applet/tool-applet \"Greeting\" #'greeting-view {:name \"Clojure\"}))\n{:label (:label greeting)\n :state (:state greeting)\n :contracts (mapv #(satisfies? % greeting)\n                  [model/IApplet model/IUI model/IResizable])}"}]
    :links [{:kind :var :symbol com.phronemophobic.easel.model/IApplet}
            {:kind :var :symbol com.phronemophobic.easel.model/IUI}
            {:kind :var :symbol com.phronemophobic.easel.model/IResizable}
            {:kind :var :symbol com.phronemophobic.easel.applet/tool-applet}
            {:kind :guide :id :lifecycle}]}
   {:id :components
    :title "Views describe; intents change"
    :paragraphs
    ["A Membrane defui component describes a view as data. Its arguments have writable paths, spelled with a leading $, so a handler can return [:update $count inc] without knowing where the component lives in the application."
     "add-component-as-applet adapts an existing defui var and a map of initial arguments. This is the shortest route from a REPL experiment to a native tool. The example defines a counter; it opens a pane only if you evaluate the final commented form yourself."
     "tool-applet is useful when a tool owns one state map or needs resource cleanup. ComponentApplet is useful when you already have a component with individual arguments. Both participate in the same pane and lifecycle protocols."]
    :examples
    [{:title "A component ready to become an applet"
      :source "(require '[membrane.component :refer [defui]]\n         '[com.phronemophobic.membrandt :as ant]\n         '[com.phronemophobic.easel :as easel])\n(defui counter-view [{:keys [count]}]\n  (ant/button {:text (str \"Count: \" count)\n               :on-click #(vector [:update $count inc])}))\n;; Evaluate in Clobber or your REPL to open it:\n;; (easel/add-component-as-applet #'counter-view {:count 0})\n(select-keys (meta #'counter-view) [:name :arglists])"}]
    :links [{:kind :var :symbol com.phronemophobic.easel/add-component-as-applet}
            {:kind :var :symbol membrane.component/defui}
            {:kind :guide :id :applets}]}
   {:id :repl
    :title "Keep the feedback loop inside the workspace"
    :paragraphs
    ["Clobber's Clojure buffers evaluate the preceding form with C-x C-e, the top-level form with C-M-x, and the whole buffer with C-c C-k. Results appear inline; printed output goes to Logs and failures go to Exceptions. The buffer's namespace determines where forms run."
     "Use Data Inspector to navigate a returned value, its metadata, or its datafy representation. Tap Watcher follows tap> values. NS Watcher follows vars in a chosen namespace. These are different views of the same running Clojure process."
     "Redefine a view var to change presentation while keeping applet state. A failed applet offers Retry, which makes a fresh instance from its factory. Hiding a pane preserves its resources and state; closing it releases them."
     "Install FlowStorm from Tools, then open its applet to record and inspect execution. It joins the running Clojure process without requiring a separate launch command."]
    :examples
    [{:title "Metadata is ordinary data"
      :source "(require '[clojure.datafy :as datafy]\n         '[com.phronemophobic.easel.model :as model])\n{:namespace (ns-name (:ns (meta #'model/IApplet)))\n :methods (-> model/IApplet :sigs keys sort vec)\n :metadata (datafy/datafy (meta #'model/IApplet))}"}]
    :links [{:kind :var :symbol com.phronemophobic.easel.evaluation/evaluate}
            {:kind :var :symbol com.phronemophobic.easel.inspector/inspector-applet}
            {:kind :applets}
            {:kind :guide :id :lifecycle}]}
   {:id :lifecycle
    :title "Own resources; return cleanup"
    :paragraphs
    ["Applet records live in Easel's :applets map. When a visible pane first receives an applet, -start returns its started value. External startup is queued under :com.phronemophobic.easel.model/queue and runs after that value has been installed. Resize returns a new value; -ui reads it."
     "tool-applet accepts an optional on-start function. It receives the writable path to the state map and may return a zero-argument cleanup function. Use it to remove watches, unsubscribe listeners, and stop workers. Cleanup runs once when the applet closes, including when close races with startup."
     "applet/task! runs work outside the UI thread, sets :busy? and :error, and merges its result into the state map. A newer request supersedes older results. The instance path prevents a closed or retried applet from accepting an old worker's result."
     "Live applets shows visibility, startup or failure state, size, implementation links, and state snapshots. Its bounded change log records changes observed while Documentation is open. Inspect captures a value for navigation; the live view continues to follow the current one."]
    :examples
    [{:title "A subscription belongs to its applet"
      :source "(require '[com.phronemophobic.easel.applet :as applet]\n         '[com.phronemophobic.easel.events :as events]\n         '[membrane.ui :as ui])\n(defn messages-view [{:keys [state]}]\n  (ui/label (str (count (:entries state)) \" events\")))\n(defn messages-applet [handler]\n  (applet/tool-applet\n    \"Messages\" #'messages-view {:entries []}\n    (fn [$state]\n      (events/subscribe!\n        :log #(handler :update $state assoc :entries %)))))\n;; The returned subscription cleanup is used by tool-applet.\n(:label (messages-applet (fn [& _])))"}]
    :links [{:kind :var :symbol com.phronemophobic.easel.applet/tool-applet}
            {:kind :var :symbol com.phronemophobic.easel.applet/task!}
            {:kind :var :symbol com.phronemophobic.easel.model/-start}
            {:kind :var :symbol com.phronemophobic.easel.model/-stop}
            {:kind :var :symbol com.phronemophobic.easel.events/subscribe!}
            {:kind :applets}]}
   {:id :documentation
    :title "Teach a var to explain itself"
    :paragraphs
    ["Namespace and var pages come from the running namespaces: docstrings, arglists, protocol signatures, and source locations are read from Clojure metadata. Nothing needs to be generated after redefining a var; Refresh reads the current definitions. Namespace pages include private vars, marked as private."
     "Add :easel/examples to a var's metadata to supply a vector of source strings or maps with :title and :source. Add :easel/see-also for links: a qualified symbol links to a var, a keyword links to a guide, and a target map can link to a namespace or live applet. Examples execute explicitly in the documentation applet's scratch namespace."
     "The guides themselves are a vector of maps in com.phronemophobic.easel.guides/guides. This applet renders that data alongside live metadata and runtime values. Source opens in Clobber; Inspect opens the existing data inspector."]
    :examples
    [{:title "An executable docstring companion"
      :source "(defn twice\n  \"Return x added to itself.\"\n  {:easel/examples [\"(twice 21)\"]\n   :easel/see-also [clojure.core/+ :applets]}\n  [x]\n  (+ x x))\n(select-keys (meta #'twice) [:doc :arglists :easel/examples :easel/see-also])"}]
    :links [{:kind :var :symbol com.phronemophobic.easel.guides/guides}
            {:kind :var :symbol com.phronemophobic.easel.documentation/documentation-applet}
            {:kind :namespaces}
            {:kind :guide :id :start}]}])

(defn guide [id]
  (some #(when (= id (:id %)) %) guides))
