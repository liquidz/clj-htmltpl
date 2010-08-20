(ns clj-htmltpl.util
  (:use [simply :only [defnk !]])
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

(defn meta-tag
  ([key value] [:meta {:http-equiv key :content value}])
  ([m] (if (map? m) (map (fn [[k v]] (meta-tag (name k) v)) m) nil))
  )

(defn- header-tag [tag-name base-attr add-attr-name & coll]
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

(defnk form [:method "" :action "" :legend "" & body]
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

(defn- str-split-at [n s] (map #(apply str %) (split-at n s)))
(defnk string->link [s :url-reg #"(https?|ftp)(:\/\/[-_.!~*\'()a-zA-Z0-9;\/?:\@&=+\$,%#]+)"
                     :link-fn #(identity [:a {:href %} %])]
  (loop [urls (map first (re-seq url-reg s)), rest-str s, res ()]
    (if (empty? urls) (concat res (list rest-str))
      (let [idx (.indexOf rest-str (first urls))
            tmp (str-split-at idx rest-str)]
        (recur (rest urls)
               (su2/drop (second tmp) (-> urls first count))
               (concat res (list (first tmp) (-> urls first link-fn)))
               )
        )
      )
    )
  )

