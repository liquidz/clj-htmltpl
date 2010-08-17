(ns clj-htmltpl.test.core
  (:use [simply :only [defnk fnk]]
        [clj-htmltpl.core] :reload-all)
  (:use [clojure.test]))

(defnk tag [:name "" :attr {} :text ""] [(keyword name) attr text])

(deftest bind-test
  (let [ptag (bind tag :name "p")
        ;atag (fnk [:href "" :text ""] (tag :attr {:href href} :text text))
        ]
    (are [x y] (= x y)
      "<p>hello</p>" (render tag {:doc-type "no" :name "p" :text "hello"})
      "<p>hello</p>" (render ptag {:doc-type "no" :text "hello"})
      )
    )
  )

(deftest replace-me ;; FIXME: write
  (main)
  )
