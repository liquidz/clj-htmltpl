(ns clj-htmltpl.core
  (:use
     [simply :only [defnk ! fold]]
     [hiccup.core :only [html]]
     )
  (:require
     [clojure.contrib.str-utils2 :as st]
     [clojure.contrib.seq-utils :as se]
     )
  )

(def *doc-type*
  {"xhtml" "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">"
   "html5" "<!DOCTYPE html>"
   "no" ""
   }
  )

; =tags {{{
(defn meta-tag
  ([key value] [:meta {:http-equiv key :content value}])
  ([m] (if (map? m) (map (fn [[k v]] (meta-tag (name k) v)) m) nil))
  )

(defn- header-tag [tag-name base-attr add-attr-name coll]
  (map #(identity [tag-name (assoc base-attr add-attr-name %)]) coll)
  )

(def js (partial header-tag :script {:type "text/javascript"} :src))
(def css (partial header-tag :link {:rel "stylesheet" :type "text/css"} :href))
(def rss (partial header-tag :link {:rel "alternate" :type "application/rss+xml" :title "no-title"} :href))

(defn xhtml-meta [lang content-type charset]
  (meta-tag {:Content-Language lang
             :Content-Type (str content-type "; charset=" charset)
             :Content-Script-Type "text/javascript"
             :Content-Style-Type "text/css"
             })
  )

(defnk ul [coll :attr {}]
  [:ul attr
   (map
     (fn [x]
       (if (map? x)
         [:li {:class (:class x)} (if (nil? (:href x)) (:text x) [:a {:href (:href x)} (:text x)])]
         [:li (if (and (coll? x) (> (count x) 1)) [:a {:href (first x)} (second x)] x)]
         )
       )
     coll)
   ]
  )

(defnk form [:method "" :action "" :legend "" :body ()]
  [:form {:method method :action action}
   [:fieldset
    (if (! st/blank? legend) [:legend legend])
    body
    ]
   ]
  )

(defn input [type & attr] [:input (apply assoc (cons {:type type} attr))])
(def text (partial input "text"))
(def submit (partial input "submit"))


(defnk table [body :header () :attr {} :footer? true]
  [:table attr
   (when (! empty? header)
     [:thead [:tr (map (fn [h] [:th h]) header)]]
     )
   (when (and (! empty? header) footer?)
     [:tfoot [:tr (map (fn [h] [:td h]) header)]]
     )
   [:tbody
    (map
      (fn [[i r]]
        [:tr {:class (if (odd? i) "odd" "even")}
         (map
           (fn [[j d]] [:td {:class (if (odd? j) "odd" "even")} d])
           (se/indexed r))
         ]
        )
      (se/indexed body))
    ]
   ]
  )
; }}}

(defn- deep-map [f coll]
  (let [res (map #(if (coll? %) (deep-map f %) (f %)) coll)]
    (if (vector? coll) (vec res) res)
    )
  )

(defn- deep-replace [smap coll]
  (deep-map #(let [x (get smap %)] #_(println "% = " % " => " x) (if (nil? x) % x)) coll)
  )

(defn- apply-args-to-template [kv-coll args]
  (letfn [(conv [val] (cond
                         (fn? val) (apply val args)
                         (coll? val) (map conv val)
                         :else val
                         ))]
    (fold (fn [[k v] res]
            (concat res (list k (conv v)))
            ) args (partition 2 kv-coll))
    )
  )

(defn bind [base-tpl & more]
  (fn [& args]
    (apply base-tpl (apply-args-to-template more args))
    )
  )

(defn render [tpl data & more]
  (let [doc-type (if (nil? (:doc-type data)) "xhtml" (:doc-type data))
        args (fold #(concat %2 %1) () (dissoc data :doc-type))
        res (apply tpl (apply-args-to-template more args))]
    (str
      (get *doc-type* doc-type)
      (html res)
      )
    )
  )

; test data
(defnk layout [:head () :body () :title "" :html-attr {}]
  [:html html-attr
   [:head [:title title] head]
   [:body body]
   ]
  )

(defnk xhtml-layout [:head () :body () :ns "http://www.w3.org/1999/xhtml" :lang "ja"
                     :content-type "text/html" :charset "UTF-8"]
  (layout
    :html-attr {:xmlns ns}
    :head (list (xhtml-meta lang content-type charset) head)
    )
  )

(defn meta-script [& _] (meta-tag "Content-Script-Type" "text/javascript"))
(defn meta-style [& _] (meta-tag "Content-Style-Type" "text/css"))
(defnk index [:title "" :list-data ()]
  [:div#main [:h1 "hoge - " title] #_(ul list-data)]
  )

(defnk test-footer [:name ""]
  [:p name]
  )

(defnk test-link [:name "" :href ""]
  [:a {:href href} name]
  )

(defnk test-tag [:name "" :attr {} :text ""] [(keyword name) attr text])

;(defmacro add-tpl-param [base bindings])
;
;(add-tpl-param test-tag [:href ""] :attr {:href href})

(defn main [& args]
  (let [data {:title "hello" :list-data [1 2 3]}
        top (bind layout :head [meta-script meta-style] :body index)
        ]
    (println
      ;(render layout data :head [title meta-script meta-style] :body index)
      ;(render top data)
      ;(render (bind test-footer :name test-link) {:name "hoge" :href "www.nifty.com"})
      ;(render layout {:title "hoge" :name "neko"} :body [test-footer (bind test-link :name "inu")])
      nil
      )
    )
  )

