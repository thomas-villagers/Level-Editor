(ns editor
  (:import (java.awt Color Dimension BorderLayout GridLayout Graphics2D BasicStroke)
           (javax.swing JPanel JFrame Timer JOptionPane JMenu JMenuBar JMenuItem JFileChooser JScrollPane JTextField JLabel JSeparator JSpinner JButton SpinnerNumberModel JDialog JSlider JTable ImageIcon
                        SpringLayout Spring)
           (javax.swing.table AbstractTableModel)
           (javax.imageio ImageIO)
           (javax.swing.event ChangeListener TableModelListener)
           (java.awt.image RescaleOp BufferedImage)
           (java.awt.event ActionListener KeyListener MouseListener MouseMotionListener WindowAdapter))
  (:use macros))

(def screen-sizes { :desire-hd [800 480] :iconia-500 [1280 800]})

(def tile-images (ref nil))
(def levelmap (ref {}))
(def scale (ref 1.0))
(def raster (ref [32 32]))
(def screen-size (ref (screen-sizes :iconia-500)))
(def world-size (ref [1 1]))
(def current-path (ref  "."))
(def level-list (ref '()))

(declare show-tilemap row-height-update draw-grid)

(def-object background
  [image (ref nil)
   file (ref nil)]
  {:load (fn [f]
           (dosync (ref-set file f)
                   (ref-set image (ImageIO/read f))))
   :image (fn [] @image)
   :name (fn [] (.getName @file))
   :path (fn [] (when @file (.getPath @file)))})

(def-object tile
  [size (ref [64 64])
   current (ref 0)
   image (ref nil)
   image-name (ref nil)
   tiles (ref nil)]
  {:get-ref (fn [k] (k {:size size :current current :image image :tiles tiles }))
   :size (fn
           ([] @size)
           ([v] (dosync (ref-set size v))))
   :current (fn
              ([] @current)
              ([c] (dosync (ref-set current c))))
   :image-name (fn [] (.getName @image-name))
   :image (fn
            ([] @image)
            ([file]
               (dosync (ref-set image-name  file)
                       (ref-set image (ImageIO/read file)))))
   :tiles (fn [] @tiles) ;; TODO: ... 
   :set (fn [k v]
          (let [r (k {:size size :current current :image image :tiles tiles })]
            (dosync (ref-set r v))))})

(defn update-tile-image [ image {:keys[x y width height] :as tile}]
  (let [iw (.getWidth image)
        ih (.getHeight image)
        width (min (- iw x) width)
        height (min (- ih y) height)]
    (assoc tile :image (.getSubimage image x y width height))))
                                          
(defn make-tile-images [image w h]
  (let [size [(.getWidth image) (.getHeight image)]
        [nx ny] (map quot size [w h])
        coords (for [y (range ny) x (range nx)]
                 [x y])]
    (vec (map #(hash-map :image (.getSubimage image (* w (first %)) (* h (second %)) w h)
                         :name nil
                         :x (* w (first %)) :y (* h (second %))
                         :width w  :height h) coords))))

(defn init-tilemap [file ]
  (let [image (tile :image file)
        [w h] (tile :size)]
    (dosync (ref-set tile-images (make-tile-images image w h)))
    (show-tilemap)))

(defn chooser-dialog [type parent desc exts title ]
  (let [filter (javax.swing.filechooser.FileNameExtensionFilter. desc exts)
        chooser (doto (JFileChooser. (java.io.File. @current-path))
                  (.setFileFilter filter)
                  (.setDialogTitle title))
        value (if (= type :load)
                (.showOpenDialog chooser parent)
                (.showSaveDialog chooser parent))]
    (when (= value JFileChooser/APPROVE_OPTION)
      (.getSelectedFile chooser))))
  
(defn open-dialog [parent desc exts title]
  (chooser-dialog :load parent desc exts title))

(defn save-dialog [parent desc exts title]
  (chooser-dialog :save parent desc exts title))
                                  
(defn open-dialog-png [parent title]
  (open-dialog parent "PNG Images" (into-array [ "png" ]) title))

(defn background-dialog [parent]
  (when-let [file (open-dialog-png parent "Load Background Image")]
    (background :load file)))

(defn tilemap-dialog [parent]                
  (let [dialog (JDialog. parent)
        [w h] (tile :size)
        spinnerx (JSpinner. (SpinnerNumberModel. w 1 256 1))
        spinnery (JSpinner. (SpinnerNumberModel. h 1 256 1))
        button   (doto (JButton. "OK")
                   (.addActionListener 
                    (proxy [ActionListener] []
                      (actionPerformed [e] 
                        (let [x (.getValue spinnerx)
                              y (.getValue spinnery)]
                          (tile :size [x y])
                          (.dispose dialog)
                          (when-let [file (open-dialog-png parent "Load Tilemap Image")]
                            (init-tilemap file)))))))]
    (doto dialog
      (.setTitle "Tile Size")
      (.setLayout (GridLayout. 4 0))
      (.add (JLabel. "Choose Tile Size"))
      (.add spinnerx)
      (.add spinnery)
      (.add button)
      (.setSize 175 125)
      (.show ))))

(defn set-preferred-width [table num width]
  (.. table getColumnModel (getColumn num) (setPreferredWidth width)))

(defn get-value-at [table row col]
  (.. table getModel (getValueAt row col)))

(defn world-table-model []          
  (let [names ["x" "y" "Tile Nr." "Active"]]
    (proxy [AbstractTableModel] []
      (getColumnCount [] (count names))
      (getRowCount [] (count @levelmap))
      (getColumnName [col]
        (names col))
      (getValueAt [row col]
        (let [[[x y] {:keys [tile active]}] ((vec @levelmap) row)]
          ([x y tile active] col)))
      (isCellEditable [row col] true)
      (setValueAt [val row col]        
        (let [[k v] ((vec @levelmap) row)]
          (case col
            0 (dosync
               (alter levelmap #(dissoc % k))
               (alter levelmap #(conj % { (list val (second k)) v})))
            1 (dosync
               (alter levelmap #(dissoc % k))
               (alter levelmap #(conj % { (list (first k) val) v})))
            2 (when (< val (count @tile-images))
                (dosync (alter levelmap #(assoc % k (assoc v :tile val)))))
            3 (dosync
                (alter levelmap #(assoc % k (assoc v :active val))))))) 
      (getColumnClass [col]
        (let [[[x y] {:keys [tile active]}]  ((vec @levelmap) 0)]
          (class ([x y tile active] col)))))))

(defn watch-table [ model reference tag ]
  (add-watch reference tag (fn [r k old new] (.fireTableDataChanged model))))

(defn row-height-update* [table]
  (fn [row val]
    (.setRowHeight table row val)))

(defn tile-table-model []          
  (let [names ["Tile" "Nr" "Name" "x" "y" "width" "height" "Active"]
        data [(ImageIcon.) 0 "" 0 0 0 0 true]]
    (proxy [AbstractTableModel] []
      (getColumnCount [] (count names))
      (getRowCount [] (count @tile-images))
      (getColumnName [col]
        (names col))
      (getValueAt [row col]
        (let [{:keys [image name x y width height]} (@tile-images row)]
          (case col
            0  (ImageIcon. image)
            1 row
            2 name
            3 x
            4 y
            5 width
            6 height
            7 (= (tile :current) row))))
      (isCellEditable [row col] (> col 1))
      (setValueAt [val row col]
        (let [r (@tile-images row)
              update (partial update-tile-image (tile :image))]
          (case col
            2 (dosync (alter tile-images #(assoc % row (update (assoc r :name val)))))
            3 (dosync (alter tile-images #(assoc % row (update (assoc r :x val)))))
            4 (dosync (alter tile-images #(assoc % row (update (assoc r :y val)))))
            5 (dosync (alter tile-images #(assoc % row (update (assoc r :width val)))))
            6 (do
                (dosync (alter tile-images #(assoc % row (update (assoc r :height val)))))
                (row-height-update row (+ val 2)))
            7 (tile :current row))))
      (getColumnClass [col]
        (class (data col))))))

(defn scroll-pane-frame [component title]
  (doto (JFrame. title)
    (.add (JScrollPane. component))
    (.pack)
    (.setVisible true)))

(defn tile-table []
  (let [model (tile-table-model)
        table (JTable. model)]
    (def row-height-update (row-height-update* table))
    (watch-table model (tile :get-ref :current) :tile-table)
    (watch-table model tile-images  :tile-table2)
    (.setRowHeight table (+ 2 (second (tile :size))))
    (scroll-pane-frame table "Tile Table")))

(defn world-table []
  (let [model (world-table-model)
        table (JTable. model)]
     (watch-table model levelmap :world-table)
     (scroll-pane-frame table "World Table")))

(defn get-relative-path [base path]
  (let [fb (.toURI (java.io.File. base))
        fp (.toURI (java.io.File. path))]
    (.getPath (.relativize fb fp))))

(defn only-path [file]
  (let [filename (.getName file)
        path (.getPath file)]
    (apply str (drop-last (count filename) path))))

(defn save-tile-map [parent]
  (when-let [file (save-dialog parent "Tile Maps" (into-array [ "tile" ]) "Save Tile Map")]
    (with-open [f (clojure.java.io/writer file)]
      (do
        (println (tile :image-name))
        (.write f (println-str (tile :image-name)))
        (let [tiles (remove (comp nil? :name) @tile-images)
              func (juxt :name :x :y :width :height)]
          (doseq-to tiles
                    #(.write f (println-str (apply list (func %))))))))))

(defn load-tile-map* [filename]  
  (let [lines (line-seq (java.io.BufferedReader. (java.io.StringReader. (slurp filename))))
        path (str (only-path (java.io.File. filename)) (first lines))
        image (tile :image  (java.io.File. path))]
    (vec (for [x (rest lines)]
           (update-tile-image image (conj (zipmap [:name :x :y :width :height] (read-string x)) {:image :nil}))))))

(defn load-tile-map [parent]
  (when-let [file (open-dialog parent "Tile files" (into-array [ "tile" ]) "Load Tile Map")]
    (dosync (ref-set tile-images (load-tile-map* (.getPath file)))
            (ref-set current-path (only-path file)))
    (tile :current 0)
    (show-tilemap)))

(defn load-data [filename]
  (with-open [rdr (java.io.PushbackReader. (java.io.StringReader. (str "(" (slurp filename) ")" )))]
    (binding [*read-eval* false]
      (reduce conj [] (read rdr)))))

(defn parse-object-from-list [ line ]
  (let [name (first line)
        {:keys [width height] :as tile} (first (filter #(= (:name %) name) @tile-images))
        data (map #(map - % [(* 0.5 width)  (* 0.5 height)]) (rest line))
        index (.indexOf @tile-images tile )
        value {:tile index :active true}]
    (zipmap data (repeat value))))

(def-object loaded-level
  [data (ref '())
   file (ref nil)]
  {:load (fn [f]
          (dosync
           (ref-set file f)
           (ref-set data (load-data (.getPath f)))))
   :data (fn [] @data)
   :name (fn [] (.getName @file))
   :path (fn [] (.getPath @file))})

(defn load-level* [file]
  (let [x (loaded-level :load file)
        path (only-path file) 
        bgr-image ((comp str second first ) (filter #(= 'textures (first %)) x))
        o (filter #(= 'objects (first %)) x)]
    (background :load (java.io.File. (str path bgr-image)))
    (dosync (ref-set current-path path)
            (ref-set level-list x)
            (ref-set levelmap
                     (into {} (map parse-object-from-list (rest (first o))))))))

(defn load-level [parent]
  (when-not @tile-images
    (JOptionPane/showMessageDialog parent "Load a tile map first","Info", JOptionPane/INFORMATION_MESSAGE)
    (load-tile-map parent))
  (when-let [file (open-dialog parent "Level files" (into-array [ "clj" ]) "Load Level")]
    (load-level* file)))

;; 1. step: levelmap to list !
(defn level-to-list [level]
  (let [tile-nrs (distinct (map (comp :tile val) level))
        to-int (fn [x] (map #(int %) x))]
    (cons 'objects
          (filter #(> (count %) 1)
                  (map (fn [x]
                         (let [{:keys [name width height]} (@tile-images x)
                               [width height] (map (partial * 0.5) [width height])
                               objs (keys (filter #(and (:active (val %)) (= x (:tile (val %)))) level))         
                               sh-objs (map #(to-int (map + [width height] %)) objs)]
                           (cons name sh-objs))) tile-nrs)))))

;; takes a loaded level list and replaces background and objects 
(defn adjust-level [level-list back]
  (let [textures (first (filter #(= 'textures (first %)) level-list))
        it (.indexOf level-list textures)
        io (.indexOf level-list (first (filter #(= 'objects (first %)) level-list)))
        new-list (assoc level-list it (list* (first textures) back (rest (rest textures))))]
   (assoc new-list io (level-to-list @levelmap))))  ;; ACHTUNG

(defn new-level-list [back]
  (let [[w h] @screen-size
        border (list '(0 0) (list w 0) (list w h) (list 0 h))]
    (vector (list 'textures (symbol back) 'sprites.png)
            (list 'borders border )
            (level-to-list @levelmap)
            '(dynamic-objects (ball (100 100))))))

(defn save-level [parent]
  (when-let [file (save-dialog parent "Levels" (into-array [ "lvl" ]) "Save Level")]
    (let [path (only-path file)
          background-path (if (background :image)
                            (get-relative-path path (background :path))
                            "background.png")
          level (if (empty? @level-list)
                  (new-level-list background-path)
                  (adjust-level @level-list background-path))]
      (println level)
      (dosync (ref-set current-path path))
      (with-open [f (clojure.java.io/writer file)]
        (doseq-to level
                  #(.write f (println-str %)))))))

(def menus
  ["New Tile Map" 
   (fn [parent] (tilemap-dialog parent))
   "Load Tile Map"
   (fn [parent] (load-tile-map parent ))
   "Save Tile Map"
   (fn [parent] (save-tile-map parent))
   "Load Level"
   (fn [parent] (load-level parent))
   "Save Level"
   (fn [parent] (save-level parent))
   "Background Image"
   (fn [parent] (background-dialog parent))
   "Screen Size"
   (fn [parent]
     (let [sizes (into-array (vec (map #(str (key %) " " (val %)) screen-sizes)))]
       (when-let [selected (JOptionPane/showInputDialog parent "Choose Device" "Screen Size" JOptionPane/INFORMATION_MESSAGE nil sizes (first sizes))]
         (dosync (ref-set screen-size ((read-string selected) screen-sizes))))))
   "World Table" (fn [_] (world-table))
   "Tile Table"  (fn [_] (tile-table))
   "Quit" (fn [_] (System/exit 0))])

(defn menu-listener [parent]
  (let [menus (apply hash-map menus)]
    (proxy [ActionListener] []
      (actionPerformed [e]
        ((menus (.getActionCommand e)) parent)))))

(defn-object undo-redo [state]
  [undos (ref [])
   redos (ref [])
   watch (fn [k r old new]
           (dosync (alter undos #(conj % old))))
   state (add-watch state :undo watch)]
  {
   :undo (fn []
           (when-let [x (peek @undos)]
             (remove-watch state :undo)
             (dosync (alter redos #(conj % @state))
                     (ref-set state x)
                     (alter undos pop))
             (add-watch state :undo watch)))
   :redo (fn []
           (when-let [x (peek @redos)]
             (dosync (alter redos pop)
                     (ref-set state x))))
   :get (fn [] @undos)})

(def undo-redo (undo-redo levelmap))

(defn key-listener []
  (proxy [KeyListener] []
      (keyPressed [e]
        (let [code (.getKeyCode e)]
          (when (.isControlDown e)
            (cond
             (= code (java.awt.event.KeyEvent/VK_Y))
             (undo-redo :redo)
             (= code (java.awt.event.KeyEvent/VK_Z))
             (undo-redo :undo)))))
      (keyReleased [e])
      (keyTyped [e])))

;; todos: -- Unterstützung mehrerer Tilemaps
;;        -- raster offset

(defn selected-tile [x y]
  (when @tile-images
    (ffirst
     (filter (fn [[[tx ty] {:keys [tile]}]]
               (let [{:keys [width height]} (@tile-images tile)]
                 (and (< tx x (+ tx width))
                      (< ty y (+ ty height)))))
             @levelmap))))

(def selected (ref nil))
(def mouse-pos (ref '()))
(def tile-offset (ref '(0 0)))

(defn mouse-scaled [e s]
  (vector (/ (.getX e) s) (/ (.getY e) s)))

(defn world-listener []
  (proxy [MouseListener MouseMotionListener] []
    (mouseClicked [e]
      (let [[x y] (mouse-scaled e @scale)
            c (map * (map quot [x y] @raster) @raster)]
        (if (= (.getButton e) (java.awt.event.MouseEvent/BUTTON2))
          (when-let [t (selected-tile x y)]
            (tile :current (:tile (@levelmap t))))
          (dosync
           (alter levelmap
                  (if (= (.getButton e) (java.awt.event.MouseEvent/BUTTON3))
                    #(dissoc % (selected-tile x y))
                    #(conj % {c {:tile (tile :current) :active true}})))))))
    (mouseEntered [e] )
    (mouseExited [e])
    (mousePressed [e]
      (let [[x y] (mouse-scaled e @scale)]
        (when-let [t (selected-tile x y)]
          (dosync (ref-set selected t)
                  (ref-set tile-offset (map - [x y] t))
                  (ref-set mouse-pos (list x y))))))
    (mouseReleased [e]
      (when-let [v (@levelmap @selected)]
        (let [[x y] (mouse-scaled e @scale)
              new-pos (map - [x y] @tile-offset)]
          (dosync
           (alter levelmap #(conj (dissoc % @selected)
                                  {(if (= (.getButton e) (java.awt.event.MouseEvent/BUTTON3))
                                     (map * (map quot new-pos @raster) @raster)
                                     new-pos) v}))
           (ref-set selected nil)))))
    (mouseDragged [e]
      (let [[x y] (mouse-scaled e @scale)]
        (dosync
         (ref-set mouse-pos (list x y)))))
    (mouseMoved [e])))

(defn tilemap-listener [image-w image-h]
  (proxy [MouseListener] []
    (mouseClicked [e]
      (let [[w h] (tile :size)
            mx (.getX e)
            my (.getY e)
            xtiles (filter (fn [{:keys [x width]}] (< x mx (+ width x))) @tile-images)
            ytiles (filter (fn [{:keys [y height]}] (< y my (+ y height))) xtiles)
            index (.indexOf @tile-images (first ytiles))]
        (when-not (neg? index)
          (tile :current index))))
    (mouseEntered [e])
    (mouseExited [e])
    (mousePressed [e])
    (mouseReleased [e])))

(defn draw-transparent [g img [x y]]
  (let [scales (float-array [1.0 1.0 1.0 0.7])
        offsets (float-array [0.0 0.0 0.0 0.0])
        op (RescaleOp. scales offsets nil)]
    (.drawImage g img op (int x) (int y))))

(defn draw-grid [g w h dx dy]
  (doseq [n (range 0 w dx)]           
    (.drawLine g n 0 n h))
  (doseq [n (range 0 h dy)]
    (.drawLine g 0 n w n)))

(defn panel [draw]
  (proxy [JPanel ActionListener] []
    (paintComponent [g]
      (proxy-super paintComponent g)
      (draw g))
    (actionPerformed [_] (.repaint this))))

(defn world-panel []
  (panel
   (fn [g]
     (let [[w h] (map * @world-size @screen-size)
           g2 (.create g)
           [rx ry] @raster]
       (.scale g2 @scale @scale)
       (when-let [back  (background :image)]
         (.drawImage g2 back nil (int 0) (int 0)))
       (.drawRect g2 0 0 w h)                          
       (draw-grid g2 w h rx ry)
       (when @tile-images
         (doseq [[[x y] {:keys [tile]}] (filter (comp :active val) @levelmap)]
           (let [img (:image (@tile-images tile))]
             (if (= [x y] @selected )
               (draw-transparent g2 img (map - @mouse-pos @tile-offset))
              (.drawImage g2 img nil (int x) (int y))))))
       (.setColor g2 (Color. 200 0 0))
       (.setStroke g2 (BasicStroke. (float 2)))
       (draw-grid g2 w h (first @screen-size) (second @screen-size))))))
                    
(defn ctile-panel []
  (panel
   (fn [g]
     (when @tile-images
       (let [image (:image (@tile-images (tile :current)))]
         (.drawImage g image 0 0 nil))))))
   
(defn tilemap-panel [image]
  (panel
   (fn [g]
     (.drawImage g image 0 0 nil)
     (doseq [{:keys  [x y width height]} @tile-images]
       (.drawRect g x y width height)))))

(defn lin-int [a b]
  (fn [t]
    (+ a (* t (- b a)))))

(defn spr-layout
  ([x y w h] (javax.swing.SpringLayout$Constraints. (Spring/constant x) (Spring/constant y) (Spring/constant w) (Spring/constant h)))
  ([x y]     (javax.swing.SpringLayout$Constraints. (Spring/constant x) (Spring/constant y))))

(defn add-component
  ([p c x y]
     (.add p c (spr-layout x y)))
  ([p c x y w h]
     (.add p c (spr-layout x y w h))))

(defn set-accelerator [ menu-item k ]
  (.setAccelerator menu-item (javax.swing.KeyStroke/getKeyStroke k (java.awt.event.InputEvent/CTRL_MASK))))

(defmacro accelerator-event [ menu-item ev  ]
  `(set-accelerator ~menu-item (. java.awt.event.KeyEvent ~ev)))

(defn spinner [init min max step listener]
  (doto (JSpinner. (SpinnerNumberModel. init min max step))
     (.addChangeListener 
      (proxy [ChangeListener] []
        (stateChanged [e]
          (let [v (.getValue (.getSource e))]
            (listener v)))))))

(defn show-tilemap [ ]
  (let [image (tile :image)
        [w h] [ (.getWidth image) (.getHeight image)]
        panel (doto (tilemap-panel image)
                (.addMouseListener (tilemap-listener w h))
                (.setPreferredSize (Dimension. w h)))]
    (.start (Timer. 60 panel))
    (doto (JFrame. "Tiles")
      (.add panel)
      (.pack)
      (.setVisible true))))

(defn World [parent]
  (let [[x y] (map * @screen-size @world-size)
        listener (world-listener)
        world (doto (world-panel)
                (.addMouseListener listener)
                (.addMouseMotionListener listener)
                (.setPreferredSize (Dimension. x y)))
        scroll-pane (doto (JScrollPane.)
                      (.setPreferredSize (Dimension. x y))
                      (.setViewportView world))]
    (add-watch scale :scale (fn [r k old new]
                              (let [[w h] (map (partial * new) @world-size @screen-size)]
                                (.setPreferredSize world (Dimension. w h))
                                (.setSize world (Dimension. w h)))))
    (add-watch world-size :world (fn [r k old [x y]]
                                   (let [[w h] (map (partial * @scale) [x y] @screen-size)]
                                     (.setPreferredSize world (Dimension. w h))
                                     (.setSize world (Dimension. w h)))))
    (.start (Timer. 60 world))
    (doto (JFrame. "World")
      (.addKeyListener (key-listener))
      (.setLocationRelativeTo parent)
      (.add scroll-pane BorderLayout/CENTER)
      (.pack)
      (.setVisible true))))

(defn menu-item [ text action acc]
  (doto (JMenuItem. text)
    (set-accelerator acc)
    (.addActionListener
     (proxy [ActionListener] []
       (actionPerformed [e]
         (action))))))
      
(defn App []                         
  (let [frame (JFrame. "Level Editor")
        spinnerx (spinner (first @raster) 1 256 1 (fn [v] (dosync (alter raster #(assoc % 0 v)))))
        spinnery (spinner (second @raster) 1 256 1 (fn [v] (dosync (alter raster #(assoc % 1 v)))))
        spinnerwx (spinner (first @world-size) 1 256 1 (fn [v] (dosync (alter world-size #(assoc % 0 v)))))
        spinnerwy (spinner (second @world-size) 1 256 1 (fn [v] (dosync (alter world-size #(assoc % 1 v)))))
        listener (menu-listener frame)
        menu (doto (JMenu. "File")
               (.setMnemonic java.awt.event.KeyEvent/VK_F))
        edit (doto (JMenu. "Edit")
               (.setMnemonic java.awt.event.KeyEvent/VK_E))
        tile (ctile-panel)
        menu-items (into [] (map #(doto (JMenuItem. %)
                                    (.addActionListener listener)) (map first (partition 2 menus)))) ;; (keys menus)))
        menu-bar (doto (JMenuBar. )
                   (.add menu)
                   (.add edit))
        interpolator1 (lin-int 1 10)
        interpolator2 (lin-int 1 (/ 1 10))
        slider (doto (JSlider. -50 50 0)
                 (.addChangeListener 
                  (proxy [ChangeListener] []
                    (stateChanged [e]
                      (let [v (.getValue (.getSource e))
                            s (if (pos? v)
                                (interpolator1 (/ v 50))
                                (interpolator2 (/ (* v -1) 50)))]
                        (dosync (ref-set scale (float s))))))))]
    (accelerator-event (first menu-items) VK_N)
    (accelerator-event (menu-items 3) VK_L)
    (accelerator-event (menu-items 4) VK_S)
    (accelerator-event (menu-items 5) VK_B)
    (accelerator-event (menu-items 7) VK_W)
    (accelerator-event (menu-items 8) VK_T)
    (accelerator-event (menu-items 9) VK_Q)
    (doseq-to menu-items .add menu)
    (.add edit (menu-item "undo" #(undo-redo :undo) java.awt.event.KeyEvent/VK_Z))
    (.add edit (menu-item "redo" #(undo-redo :redo) java.awt.event.KeyEvent/VK_Y))
    (doto frame
      (.setJMenuBar menu-bar )
      (.setLayout (SpringLayout.))
      (add-component (JLabel. (ImageIcon. "fv_10.png")) 0 0 300 200)
      (add-component (JLabel. "Grid Size") 0 185)
      (add-component (JLabel. "w") 5 215)
      (add-component spinnerx  20 215)
      (add-component (JLabel. "h") 85 215)
      (add-component spinnery 100 215)
      (add-component (JLabel. "World Size")  0 255)
      (add-component (JLabel. "x") 5 285)
      (add-component spinnerwx 20 285)
      (add-component (JLabel. "y") 85 285)
      (add-component spinnerwy 100 285)
      (add-component (JSeparator. javax.swing.SwingConstants/HORIZONTAL) 0 315 300 10)
      (add-component (JLabel. "Current Tile") 0 325)
      (add-component tile 50 360 64 64)
      (add-component (JSeparator. javax.swing.SwingConstants/HORIZONTAL) 0 445 300 10)
      (add-component (JLabel. "Zoom") 0 455)
      (add-component slider 5 490)
      (.pack)
      (.setSize (Dimension. 300 600))
      (.setVisible true))
    (World frame)
    (.start (Timer. 60 tile))))

(App)