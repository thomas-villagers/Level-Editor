(ns macros)

(defmacro member-set! [object & args]
  (let [vals (partition 2 args)
        obj (gensym)]
    `(let [~obj ~object] 
      (do ~@(map #(list 'set! (list (first %) obj) (second %)) vals)
          ~obj))))

;(defmacro when-pressed [key & form]
;  `(when (.isKeyPressed Gdx/input (. com.badlogic.gdx.Input$Keys ~key))
;     ~@form))

(defmacro when-pressed
  ([key form]
  `(when (.isKeyPressed Gdx/input (. com.badlogic.gdx.Input$Keys ~key))
     ~form))
  ([key form & rest]
     `(do
        (when-pressed ~key ~form)
        (when-pressed ~@rest))))

;(defmacro when-key [k & form]
;  `(when (= ~(symbol "key") (. com.badlogic.gdx.Input$Keys ~k))
;     ~@form))

(defmacro when-key
  ([k form]
  `(when (= ~(symbol "key") (. com.badlogic.gdx.Input$Keys ~k))
     (do ~form)))
  ([k form & rest]
     `(do 
        (when-key ~k ~form)
        (when-key ~@rest))))

(defmacro apply-linear-impulse [body impulse-x impulse-y]
  (let [body (with-meta body {:tag 'Body})]
    `(.applyLinearImpulse ~body (Vector2. ~impulse-x ~impulse-y) (.getWorldCenter ~body))))

(defmacro apply-linear-impulse* [body impulse-x impulse-y]
  `(apply-linear-impulse (:dynamic-body ~body) ~impulse-x ~ impulse-y))

(defmacro set-linear-velocity [body vx vy]
  (let [body (with-meta body {:tag 'Body})]
    `(.setLinearVelocity ~body (float ~vx) (float ~vy))))

(defmacro set-linear-velocity* [body vx vy]
  `(set-linear-velocity (:dynamic-body ~body) ~vx ~vy))

(defmacro get-linear-velocity [body]
  (let [body (with-meta body {:tag 'Body})]
    `(vec2 (.getLinearVelocity ~body))))

(defmacro get-linear-velocity* [body]
  `(get-linear-velocity (:dynamic-body ~body)))

(defmacro for-all [arg func & args]
  `(for [a# ~arg] (~func ~@args a#)))

(defmacro doseq-to [ arg func & args ]
  `(doseq [a# ~arg] (~func ~@args a#))) 

(defmacro when-let-do [test bindings & forms]
  `(when ~test (let [~@bindings] ~@forms)))

(defmacro active? [aref]
  `@(:active ~aref))

(defmacro vec2 [v] `[(.x ~v) (.y ~v)])

(defmacro vecmul [v s]
  `(map #(float (* ~s %)) ~v))

(defmacro rad-to-deg [ rad ]
  (list '* rad (/ (float 180) java.lang.Math/PI)))

(defmacro deg-to-rad [ deg ]
  (list '* deg (/ java.lang.Math/PI (float 180))))

(defmacro tex-to-physical [v]
  `(vector (float (/ (first ~v) 32)) (float (/ (- ~(symbol "HEIGHT") (second ~v)) 32))))

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

(defmacro activate-once [aref & action]
  `(when-not @(:active ~aref)
     (do 
       (dosync (ref-set (:active ~aref) true))
       ~@action)))

(defmacro deactivate-once [aref & action]
  `(when @(:active ~aref)
     (do 
       (dosync (ref-set (:active ~aref) false))
       ~@action)))

(defmacro active? [aref]
  `@(:active ~aref))

(defmacro activate
  ([aref] `(dosync (ref-set (:active ~aref) true)))
  ([aref & more]
     `(do (activate ~aref) 
          (activate ~@more))))

(defmacro deactivate 
  ([aref] `(dosync (ref-set (:active ~aref) false)))
  ([aref & more]
     `(do (deactivate ~aref) 
          (deactivate ~@more))))

(defmacro toggle-active
  ([aref] `(dosync (alter (:active ~aref) not)))
  ([aref & more]
     `(do (toggle-active ~aref)
          (toggle-active ~@more))))

;(defmacro wait [time func & args]
;  `(do (Thread/sleep ~time)
;       (~func ~@args)))

(defmacro wait [time & args]
  `(do (Thread/sleep ~time)
       (do ~@args)))

(defmacro with-waits [time & forms]
  (let [x (gensym)
        x (interpose `(Thread/sleep ~time) forms)]
    `(do ~@x)))

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

;; ************* LEVEL GENERATORS *******************

(defmacro textures [ & args]
  (let [strs (vec (map str args))]
    `(def ~'texture-files ~strs)))

;(defmacro borders [ & args ]
;  (let [x 
;        (vec (map #(if (list? %)
;                (apply hash-map (list*  :file (str (first %)) (rest %)))
;                {:file (str %)}) args))]
;    `(def ~'border-shapes ~x)))

(defmacro borders [ & args ]
  (let [x 
        (vec (map (fn [x]
                    (if (list? x)
                      {:polygon (vec (map vec x)) }
                      {:file (str x)})) args))]
    `(def ~'border-shapes ~x)))

(defn target-entry [k c]
  (let [k (keyword (str  k))
        c (vec c)]
    {:type k :center c}))

(defmacro target-line [[k & rst] ]
  (let [args (partition 2 (interleave (repeat k) rst))
        x (map #(target-entry (first %) (second %)) args)]
     `(vector ~@x)))

(defmacro objects [ & args ]
  (let [x (map #(list 'target-line %) args)]
    `(def ~'target-data (vec (concat ~@x)))))
