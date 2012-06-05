(ns macros)


(defmacro for-all [arg func & args]
  `(for [a# ~arg] (~func ~@args a#)))

(defmacro doseq-to [ arg func & args ]
  `(doseq [a# ~arg] (~func ~@args a#))) 

(defmacro when-let-do [test bindings & forms]
  `(when ~test (let [~@bindings] ~@forms)))


(defmacro doto-when [object test yes & always]
  `(let [obj# ~object]
     (when ~test (doto obj# ~yes))
     (doto obj# ~@always)))

(defmacro doto-if [object test yes no & always]
  `(if ~test
     (doto ~object ~yes ~@always)
     (doto ~object ~no ~@always)))

(defmacro when-let-do [test bindings & forms]
  `(when ~test (let [~@bindings] ~@forms)))

(defmacro make-map [ & args ]
  `(~hash-map ~@(interleave (map (comp keyword str) args) args)))


(defmacro def-object [name bindings funcs ]
  `(def ~name (let ~bindings
                (let [funcs# ~funcs]
                  (fn
                    ([~'f] ((funcs# ~'f)))
                    ([~'f & ~'args] (apply (funcs# ~'f) ~'args)))))))

(defmacro defn-object [name args bindings funcs]
  `(defn ~name ~args (let ~bindings
                       (let [funcs# ~funcs]
                         (fn
                           ([~'f] ((funcs# ~'f)))
                           ([~'f & ~'args] (apply (funcs# ~'f) ~'args)))))))

