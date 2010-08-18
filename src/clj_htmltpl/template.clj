(ns clj-htmltpl.template
  (:use
     [simply :only [defnk]]
     [clj-htmltpl core util]
     )
  )

(defnk layout [:head () :body () :title "" :html-attr {}]
  [:html html-attr
   [:head [:title title] head]
   [:body body]
   ]
  )

(def xhtml-layout
  (bind layout [:ns "http://www.w3.org/1999/xhtml" :lang "ja" :content-type "text/html" :charset "UTF-8" :head ()]
        :html-attr {:xmlns ns}
        :head (list (xhtml-meta lang content-type charset) head)
        )
  )

(defnk wrapper [:header () :main () :menu () :footer () :container ()]
  [:div#wrapper
   [:div#header header]
   [:div#container
    [:div#main main]
    [:div#menu menu]
    container
    ]
   [:div#footer footer]
   ]
  )

