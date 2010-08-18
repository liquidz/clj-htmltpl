(ns clj-htmltpl.test.core
  (:use
     [simply :only [fnk]]
     [clj-htmltpl core template util]
     clojure.test
     :reload-all
     )
  (:require [clojure.contrib.str-utils2 :as st])
  )

(deftest bind-test
  (let [tag (fnk [:name "" :attr {} :text ""] [(keyword name) attr text])
        p-tag (bind tag :name "p")
        a-tag (bind tag [:href ""] :name "a" :attr {:href href})
        pa-tag (bind p-tag :text a-tag)
        a-tag2 (bind tag [:href "" :attr {}] :name "a" :attr (assoc attr :href href))
        pas-tag (bind p-tag :text (list a-tag (bind tag :name "span")))
        pas-tag2 (bind p-tag :text (list a-tag (bind tag :name "span" :text "neko")))
        ]
    (are [x y] (= x y)
      "<p>hello</p>" (render tag :name "p" :text "hello")
      "<p>hello</p>" (render p-tag :text "hello")
      "<a href=\"hoge\">hello</a>" (render a-tag :href "hoge" :text "hello")
      "<p><a href=\"hoge\">hello</a></p>" (render pa-tag :href "hoge" :text "hello")
      "<p><a href=\"hoge\">hello</a><span>hello</span></p>" (render pas-tag :href "hoge" :text "hello")
      "<p><a href=\"hoge\">hello</a><span>neko</span></p>" (render pas-tag2 :href "hoge" :text "hello")
      )

    (let [tmp (render a-tag2 :href "hoge" :attr {:class "neko"} :text "hello")]
      (is (and (st/contains? tmp "class=\"neko\"")
               (st/contains? tmp "href=\"hoge\"")))
      )
    )
  )

(deftest header-tag-test
  (let [res (js "a.js" "b.js")]
    (are [x y] (= x y)
      2 (count res)
      true (every? #(-> % first (= :script)) res)
      )
    )
  )


(deftest my-test
  (bind xhtml-layout
        :head (list (js "aa.js" "bb.js"))
        )
  )
