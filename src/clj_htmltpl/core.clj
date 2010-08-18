(ns clj-htmltpl.core
  (:use
     [simply :only [fold]]
     [clj-htmltpl.util :only [*doc-type*]]
     [hiccup.core :only [html]]
     )
  (:require
     [clojure.contrib.str-utils2 :as st]
     [clojure.contrib.seq-utils :as se]
     )
  )

(defn apply-args-to-template [kv-coll args]
  (letfn [(conv [val] (cond
                        (fn? val) (apply val args)
                        (list? val) (map conv val)
                        :else val
                        ))]
    (fold (fn [[k v] res]
            (concat res (list k (conv v)))
            ) args (partition 2 kv-coll))
    )
  )

(defmacro bind [base-tpl & more]
  (let [[new-bind rest-more] (if (->> more first vector?) [(first more) (rest more)] [() more])
        skeys (map #(-> % first name symbol) (partition 2 new-bind))
        nbmap (apply hash-map (map #(if (keyword? %) (-> % name symbol) %) new-bind))
        ]
    `(fn [& args#]
       (let [{:keys [~@skeys] :or ~nbmap} (apply hash-map args#)]
         (apply ~base-tpl (apply-args-to-template (list ~@rest-more) args#))
         )
       )
    )
  )

(defn render [tpl & more]
  (let [data (apply hash-map more)
        doc-type (if (nil? (:doc-type data)) "no" (:doc-type data))
        args (fold #(concat %2 %1) () (dissoc data :doc-type))
        res (apply tpl (apply-args-to-template () args))]
    (str
      (get *doc-type* doc-type)
      (html res)
      )
    )
  )

