(ns editor.core
  (:import (java.awt Graphics Graphics2D Color Dimension BorderLayout GridLayout Graphics2D BasicStroke RenderingHints)
      ;;     (javax.media.jai RenderedOp)
           (javax.swing JPanel JFrame Timer JOptionPane JMenu JMenuBar JMenuItem JFileChooser JScrollPane JTextField JLabel JSeparator JSpinner JButton SpinnerNumberModel JDialog JSlider JTable ImageIcon
                        SpringLayout Spring)
           (javax.swing.table AbstractTableModel)
           (javax.imageio ImageIO)
           (javax.swing.event ChangeListener TableModelListener)
           (java.awt.image RescaleOp BufferedImage)
           (java.awt.event ActionListener KeyListener MouseListener MouseMotionListener WindowAdapter))
  (:use macros readsvg clojure.xml))

(def screen-sizes { :desire-hd [800 480] :iconia-500 [1280 800]})

(def tile-images (ref nil))
(def levelmap (ref {}))
(def scale (ref 1.0))
(def raster (ref [32 32]))
(def screen-size (ref (screen-sizes :iconia-500)))
(def world-size (ref [1 1]))
(def current-path (ref  "."))

(declare show-tilemap row-height-update draw-grid world-frame)

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
   :tiles (fn [] @tiles)}) ;; TODO: ... 

(defn update-tile-image [ image {:keys[x y width height] :as tile}]
  (let [iw (.getWidth image)
        ih (.getHeight image)
        width (min (- iw x) width)
        height (min (- ih y) height)]
    (assoc tile :image (.getSubimage image x y width height))))
                                          
(defn make-tile-images [image w h]
  (let [size [(.getWidth image) (.getHeight image)]
        [nx ny] (map quot size [w h])
        coords (for [y (range ny) x (range nx)] (map * [w h] [x y]))]
    (vec (map (fn [[x y]]
                (hash-map :image (.getSubimage image x y w h) :name nil :x x :y y :width w  :height h)) coords))))

(defn init-tilemap [file ]
  (let [image (tile :image file)
        [w h] (tile :size)]
    (dosync (ref-set tile-images (make-tile-images image w h)))
    (show-tilemap)))

(defn only-path [file]
  (let [filename (.getName file)
        path (.getPath file)]
    (apply str (drop-last (count filename) path))))

;; to be used with update thumbs 
(defn dir-dialog [parent title]
  (let [chooser (doto (JFileChooser. (java.io.File. @current-path))
                  (.setFileSelectionMode JFileChooser/DIRECTORIES_ONLY)
                  (.setDialogTitle title))
        value (.showOpenDialog chooser parent)]
    (when (= value JFileChooser/APPROVE_OPTION)
      (.getSelectedFile chooser))))

(defn chooser-dialog [type parent desc exts title ]
  (let [filter (javax.swing.filechooser.FileNameExtensionFilter. desc (into-array exts))
        chooser (doto (JFileChooser. (java.io.File. @current-path))
                  (.setFileFilter filter)
                  (.setDialogTitle title))
        value (if (= type :save)
                (.showSaveDialog chooser parent)
                (.showOpenDialog chooser parent))]
    (when (= value JFileChooser/APPROVE_OPTION)
      (let [f (.getSelectedFile chooser)]
        (dosync (ref-set current-path (only-path f)))
        (spit "lastpath.txt" @current-path)
        f))))
  
(defn open-dialog [parent desc exts title]
  (chooser-dialog :load parent desc exts title))

(defn save-dialog [parent desc exts title]
  (chooser-dialog :save parent desc exts title))
                                  
(defn open-dialog-png [parent title]
  (open-dialog parent "PNG Images" [ "png" ]  title))

(defn background-dialog [parent]
  (when-let [file (open-dialog-png parent "Load Background Image")]
    (background :load file)))

(defn spr-layout
  ([x y w h] (javax.swing.SpringLayout$Constraints. (Spring/constant x) (Spring/constant y) (Spring/constant w) (Spring/constant h)))
  ([x y]     (javax.swing.SpringLayout$Constraints. (Spring/constant x) (Spring/constant y))))

(defn add-component
  ([p c] (.add p c))
  ([p c x y] (.add p c (spr-layout x y)))
  ([p c x y w h] (.add p c (spr-layout x y w h))))

(defmacro add-components [parent & components]
  (let [fs (map #(list* 'add-component parent (if (list? %) % (list %))) components)]
    `(do ~@fs)))

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
      (add-components ((JLabel. "Choose Tile Size")) spinnerx spinnery button)
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
            0 (dosync (alter levelmap #(conj (dissoc % k) { (list val (second k)) v})))
            1 (dosync (alter levelmap #(conj (dissoc % k) { (list (first k) val) v})))
            2 (when (< val (count @tile-images))
                (dosync (alter levelmap #(assoc % k (assoc v :tile val)))))
            3 (dosync (alter levelmap #(assoc % k (assoc v :active val))))))) 
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

(defn save-tile-map [parent]
  (when-let [file (save-dialog parent "Tile Maps" [ "tile" ] "Save Tile Map")]
    (with-open [f (clojure.java.io/writer file)]
      (.write f (println-str (tile :image-name)))
      (let [tiles (remove (comp nil? :name) @tile-images)
            func (juxt :name :x :y :width :height)]
        (doseq-to tiles
                  #(.write f (println-str (apply list (func %)))))))))
;; TODO Cocos "tile map"
;; bigblock = [CCSprite spriteWithFile: @"<image-name>" rect:CGRectMake(<x>,<y>,<x+w>,<y+w>]

(defn load-tile-map** [lines image]
  (println "load" lines image)
  (vec (for [x lines]
         (update-tile-image image (conj (zipmap [:name :x :y :width :height] (read-string x)) {:image :nil})))))
  

(defn load-tile-map* [filename]  
  (let [lines (line-seq (java.io.BufferedReader. (java.io.StringReader. (slurp filename))))
        path  (str (only-path (java.io.File. filename)) (first lines))
        image (tile :image  (java.io.File. path))]
    (spit "lasttilemap.txt" filename)
    (load-tile-map** (rest lines) image)))
;    (vec (for [x (rest lines)]
;           (update-tile-image image (conj (zipmap [:name :x :y :width :height] (read-string x)) {:image :nil}))))))

(defn load-tile-map [parent]
  (when-let [file (open-dialog parent "Tile files" [ "tile" ]  "Load Tile Map")]
    (dosync (ref-set tile-images (load-tile-map* (.getPath file))))
    (tile :current 0)
    (show-tilemap)))


(defn read-tiles-spritesheetpacker [filename]
  (let [lines (line-seq (java.io.BufferedReader. (java.io.StringReader. (slurp filename))))] 
    (map #(clojure.string/replace (str "(" % ")") "=" "") lines)))
        
(defn load-ssp-tile-map [parent]
  (when-let [file (open-dialog parent "Sprite Sheet Packer files" [ "txt" ]  "Load Sprite Sheet Packer Map")]
    (when-let [image  (open-dialog-png parent "Load Tile Sheet Image")]
      (dosync (ref-set tile-images (load-tile-map** (read-tiles-spritesheetpacker file) (tile :image image))))
      (println file image)
      (tile :current 0)
      (show-tilemap))))
;    ))

(defn load-data [filename]
  (with-open [rdr (java.io.PushbackReader. (java.io.StringReader. (str "(" (slurp filename) ")" )))]
    (binding [*read-eval* false]
      (reduce conj [] (read rdr)))))

(defn parse-object-from-list [ [name & line] ]
  (let [{:keys [width height] :as tile} (first (filter #(= (:name %) name) @tile-images))
        data (map #(map - % [(* 0.5 width)  (* 0.5 height)]) line)
        index (.indexOf @tile-images tile )
        value {:tile index :active true}]
    (zipmap data (repeat value))))

(def-object loaded-level
  [data (ref ())
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
        _ (.setTitle world-frame (loaded-level :name))
        path (only-path file) 
        bgr-image ((comp str second first ) (filter #(= 'textures (first %)) x))
        o (filter #(= 'objects (first %)) x)]
    (background :load (java.io.File. (str path bgr-image)))
    (dosync (ref-set levelmap
                     (into {} (map parse-object-from-list (rest (first o))))))))

(defn load-level [parent]
  (when-not @tile-images
    (JOptionPane/showMessageDialog parent "Load a tile map first","Info", JOptionPane/INFORMATION_MESSAGE)
    (load-tile-map parent))
  (when-let [file (open-dialog parent "Level files" [ "clj" ]  "Load Level")]
    (load-level* file)))

(defn load-level-repl [ filename ]
  (load-level* (java.io.File. filename)))

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

(defn adjust-level [level-list back]
  (let [textures (first (filter #(= 'textures (first %)) level-list))
        it (.indexOf level-list textures)
        io (.indexOf level-list (first (filter #(= 'objects (first %)) level-list)))
        new-list (assoc level-list it (list* (first textures) back (nnext textures)))] ;;(rest (rest textures))))]
   (assoc new-list io (level-to-list @levelmap))))  ;; ACHTUNG

(defn new-level-list [back]
  (let [[w h] @screen-size
        border (list '(0 0) (list w 0) (list w h) (list 0 h))]
    (vector (list 'textures (symbol back) 'sprites.png)
            (list 'borders border )
            (level-to-list @levelmap)
            '(dynamic-objects (ball (100 100))))))

(defn get-image []
  (when-let [bimg (background :image)]
    (let [back ;;(BufferedImage. 256 256 java.awt.image.BufferedImage/TYPE_INT_ARGB)
          (BufferedImage. 1280 800 java.awt.image.BufferedImage/TYPE_INT_ARGB)
          ^Graphics2D g (.createGraphics back)]
;;          back (.getScaledInstance bimg 100 100 java.awt.Image/SCALE_SMOOTH)]
      ;(.scale g 0.25 0.25)
;;      (.scale g 0.178125 0.178125)
      (.drawImage g bimg nil (int 0) (int 0))
      (doseq [[[x y] {:keys [tile]}] (filter (comp :active val) @levelmap)]
        (let [img (:image (@tile-images tile))]
          (.drawImage g img nil (int x) (int y))))
      (let [thumb-scaled (.getScaledInstance back 227 142 java.awt.Image/SCALE_SMOOTH)
            thumb (BufferedImage. 256 256 java.awt.image.BufferedImage/TYPE_INT_ARGB)
            ^Graphics2D g (.createGraphics thumb)]
        (.drawImage g thumb-scaled nil nil) ;; (int 0) (int 0))
        thumb))))

;;      (.getScaledInstance back 320 200 java.awt.Image/SCALE_SMOOTH))))


(defn write-image [image name]
  (ImageIO/write image "png" (java.io.File. name)))

(defn save-level [parent]
  (when-let [file (save-dialog parent "Levels" [ "clj" ] "Save Level")]
    (let [path (only-path file)
          background-path (if (background :image)
                            (get-relative-path path (background :path))
                            "background.png")
          level (if (empty? (loaded-level :data))
                  (new-level-list background-path)
                  (adjust-level (loaded-level :data) background-path))]
      (println level)
      (.setTitle world-frame (loaded-level :name))
      (.mkdir (java.io.File. (str (first (.split #"\." (.getPath file))))))
      (write-image (get-image) (str (first (.split #"\." (.getPath file))) "/thumb.png"))
      (with-open [f (clojure.java.io/writer file)]
        (doseq-to level
                  #(.write f (println-str %)))))))

(defn get-directories [path]
  (filter #(.isDirectory %) (.listFiles (java.io.File. path))))

(defn get-level-directories [path]  ;; memoize me ! 
  (let [dirs (get-directories path)]
    (map #(.getName %) (filter #(.startsWith (.getName %) "level") dirs))))

(defn update-all-thumbs [ path ]
  (let [dirs (get-level-directories path)]
    (println "update" (count dirs) "thumbs")
    (doseq [d dirs]
      (println "update" d)
      (load-level* (java.io.File. (str path d ".clj")))
      (write-image (get-image) (str path d "/thumb.png")))))

(defmacro key-event [ ev ]
  `(. java.awt.event.KeyEvent ~ev))

(def menus
  (array-map 
   "New Tile Map"  [(fn [] (tilemap-dialog nil)) (key-event VK_N)]
   "Load Tile Map" [(fn [] (load-tile-map nil ))]
   "Load SSP Tile Map" [(fn [] (load-ssp-tile-map nil))]
   "Save Tile Map" [(fn [] (save-tile-map nil))]
   "Load Level"    [(fn [] (load-level nil)) (key-event VK_L)]
   "Save Level"    [(fn [] (save-level nil)) (key-event VK_S)]
   "Background Image" [(fn [] (background-dialog nil)) (key-event VK_B)]
   "Screen Size"
   [(fn []
     (let [sizes (into-array (vec (map #(str (key %) " " (val %)) screen-sizes)))]
       (when-let [selected (JOptionPane/showInputDialog nil "Choose Device" "Screen Size" JOptionPane/INFORMATION_MESSAGE nil sizes (first sizes))]
         (dosync (ref-set screen-size ((read-string selected) screen-sizes))))))]
   "World Table" [(fn [] (world-table)) (key-event VK_W)]
   "Tile Table"  [(fn [] (tile-table)) (key-event VK_T)]
   "Quit" [(fn [] (System/exit 0)) (key-event VK_Q)]))

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
      (let [code (.getKeyCode e)
            menu #((first (menus %)))]
        (cond
         (= code (key-event VK_Y))
         (undo-redo :redo)
         (= code (key-event VK_Z))
         (undo-redo :undo)
         (= code (key-event VK_N))
         (menu "New Tile Map")
         (= code (key-event VK_S))
         (menu "Save Level")
         (= code (key-event VK_B))
         (menu "Background Image")
         (= code (key-event VK_L))
         (menu "Load Level"))))
        
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
                      (< ty y (+ ty height))))) @levelmap))))

(def selected (ref nil))
(def mouse-pos (ref ()))
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
                  (ref-set mouse-pos [x y])))))
    (mouseReleased [e]
      (when-let [v (@levelmap @selected)]
        (let [[x y] (mouse-scaled e @scale)
              new-pos (map - [x y] @tile-offset)]
          (dosync
           (alter levelmap #(conj (dissoc % @selected)
                                  {(if (= (.getButton e) (java.awt.event.MouseEvent/BUTTON3))
                                     new-pos
                                     (map * (map quot new-pos @raster) @raster)) v}))
           (ref-set selected nil)))))
    (mouseDragged [e]
      (let [[x y] (mouse-scaled e @scale)]
        (dosync (ref-set mouse-pos [x y]))))
    (mouseMoved [e])))

(defn tilemap-listener [image-w image-h]
  (proxy [MouseListener] []
    (mouseClicked [e]
      (let [[mx my] [(.getX e) (.getY e)]
            xtiles (filter (fn [{:keys [x width]}] (< x mx (+ width x))) @tile-images)
            ytiles (filter (fn [{:keys [y height]}] (< y my (+ y height))) xtiles)
            index (.indexOf @tile-images (first ytiles))]
        (when-not (neg? index)
          (tile :current index))))
    (mouseEntered [e])
    (mouseExited [e])
    (mousePressed [e])
    (mouseReleased [e])))

(defn draw-transparent [^Graphics2D g img [x y]]
  (let [scales (float-array [1.0 1.0 1.0 0.7])
        offsets (float-array [0.0 0.0 0.0 0.0])
        op (RescaleOp. scales offsets nil)]
    (.drawImage g img op (int x) (int y))))

(defn draw-grid [^Graphics g w h dx dy]
  (doseq [n (range 0 w dx)]           
    (.drawLine g n 0 n h))
  (doseq [n (range 0 h dy)]
    (.drawLine g 0 n w n)))

(defn panel [draw]
  (proxy [JPanel ActionListener] []
    (paintComponent [^Graphics2D g]
      (proxy-super paintComponent g)
      (draw g))
    (actionPerformed [_] (.repaint this))))

(defn world-panel []
  (panel
   (fn [^Graphics g]
     (let [[w h] (map * @world-size @screen-size)
           ^Graphics2D g2 (.create g)
           [rx ry] @raster]
       (.scale g2 @scale @scale)
       (when-let [back (background :image)]
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
   (fn [^Graphics g]
     (when @tile-images
       (let [image (:image (@tile-images (tile :current)))]
         (.drawImage g image 0 0 nil))))))
   
(defn tilemap-panel [image]
  (panel
   (fn [^Graphics g]
     (.drawImage g image 0 0 nil)
     (doseq [{:keys [x y width height]} @tile-images]
       (.drawRect g x y width height)))))

(defn lin-int [a b]
  (fn [t] (+ a (* t (- b a)))))

(defn set-accelerator [ menu-item k ]
  (when k (.setAccelerator menu-item (javax.swing.KeyStroke/getKeyStroke k (java.awt.event.InputEvent/CTRL_MASK)))))

(defn spinner [init min max step listener]
  (doto (JSpinner. (SpinnerNumberModel. init min max step))
     (.addChangeListener 
      (proxy [ChangeListener] []
        (stateChanged [e]
          (listener (.getValue (.getSource e))))))))

(defn show-tilemap [ ]
  (let [image (tile :image)
        [w h] [ (.getWidth image) (.getHeight image)]
        panel (doto (tilemap-panel image)
                (.addMouseListener (tilemap-listener w h))
                (.setPreferredSize (Dimension. w h)))]
    (.start (Timer. 60 panel))
    (doto (JFrame. "Tiles")
      (.add panel)
       (.addKeyListener (key-listener))
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

(defn jmenu [ text & acc ]
  (doto (JMenu. text) (.setMnemonic (first acc))))

(defn open-if-exists [filename]
  (when (.exists (java.io.File. filename)) (slurp filename)))

(defn App []                         
  (let [lasttilemapfile (open-if-exists "lasttilemap.txt")
        lastpath (open-if-exists "lastpath.txt")
        frame (JFrame. "Level Editor")
        spinnerx (spinner (first @raster) 1 256 1 (fn [v] (dosync (alter raster #(assoc % 0 v)))))
        spinnery (spinner (second @raster) 1 256 1 (fn [v] (dosync (alter raster #(assoc % 1 v)))))
        spinnerwx (spinner (first @world-size) 1 256 1 (fn [v] (dosync (alter world-size #(assoc % 0 v)))))
        spinnerwy (spinner (second @world-size) 1 256 1 (fn [v] (dosync (alter world-size #(assoc % 1 v)))))
        menu (jmenu "File" (key-event VK_F))
        edit (doto (jmenu "Edit" (key-event VK_E))
               (.add (menu-item "undo" #(undo-redo :undo) (key-event VK_Z)))
               (.add (menu-item "redo" #(undo-redo :redo) (key-event VK_Y))))
        tile-panel (ctile-panel)
        menu-items (into [] (map (fn [[k [action accel]]]
                                   (menu-item k action accel)) menus))
        tools (doto (jmenu "Tools" (key-event VK_T))
                (.add (menu-item "update thumbs"
                                 #(when (and @tile-images (> (count @levelmap) 0))
                                      (future (update-all-thumbs @current-path)))
                                 (key-event VK_U))))
        
        menu-bar (doto (JMenuBar.) (add-components menu edit tools))
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
    (doseq-to menu-items .add menu)
    (doto frame
      (.setJMenuBar menu-bar )
      (.setLayout (SpringLayout.))
      (add-components ((JLabel. (ImageIcon. "fv_10.png")) 0 0 300 200)
                       ((JLabel. "Grid Size") 0 185)
                       ((JLabel. "w") 5 215)  (spinnerx  20 215)
                       ((JLabel. "h") 85 215) (spinnery 100 215)
                       ((JLabel. "World Size")  0 255)
                       ((JLabel. "x") 5 285)  (spinnerwx 20 285)
                       ((JLabel. "y") 85 285) (spinnerwy 100 285)
                       ((JSeparator. javax.swing.SwingConstants/HORIZONTAL) 0 315 300 10)
                       ((JLabel. "Current Tile") 0 325)
                       (tile-panel 50 360 64 64)
                       ((JSeparator. javax.swing.SwingConstants/HORIZONTAL) 0 445 300 10)
                       ((JLabel. "Zoom") 0 455)
                       (slider 5 490))
      (.pack)
      (.setSize (Dimension. 300 600))
      (.setVisible true))
    (def world-frame  (World frame))
    (.start (Timer. 60 tile-panel))
    (when (and lastpath (.exists (java.io.File. lastpath)))
      (dosync (ref-set current-path lastpath)))

    (when (and lasttilemapfile (.exists (java.io.File. lasttilemapfile)))
      (dosync (ref-set tile-images (load-tile-map* lasttilemapfile)))
      (tile :current 0)
      (show-tilemap))))
(App)



;;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% EXPERIMENTAL **********************************
(def border (ref nil))

(defn neighs [x y]
  (remove (partial = [x y]) (for [a [(dec x) x (inc x)] b [(dec y) y (inc y)]] [a b])))

(defn transparent? [image x y]
  (zero? (bit-and 0xff000000 (.getRGB image x y))))

(defn transparent*? [image]
  (let [w (.getWidth image)
        h (.getHeight image)]
    (fn [x y]
      (or (neg? x) (neg? y) (>= x w) (>= y h)
          (transparent? image x y)))))

(defn border-pixel? [test x y]
  (and (not (test x y))
       (pos? (count (filter #(apply test %) (neighs x y))))))

(defn border-pixels [image]
  (let [w (.getWidth image)
        h (.getHeight image)]
    (filter #(apply (partial border-pixel? (transparent*? image)) %)
            (for [x (range w) y (range h)] [x y]))))
        
(defn tile-p []
  (panel
   (fn [g]
     (when @tile-images
       (let [image (:image (@tile-images (tile :current)))]
         (.drawImage g image 0 0 nil))
       (when @border
         (.setColor g (Color. 200 0 0))
         (doseq [b @border]
           (.drawRect g (first b) (second b) 1 1)))))))


(defn tile-f [ ]
  (let [image (:image (@tile-images (tile :current)))
        [w h] [ (.getWidth image) (.getHeight image)]
        panel (doto (tile-p)
                (.setPreferredSize (Dimension. w h)))]
  ;  (.start (Timer. 60 panel))
    (doto (JFrame. "Tiles")
      (.add panel)
      (.pack)
      (.setVisible true))))


(defn image-panel [ img ]
  (panel
   (fn [g]
     (.drawImage g img 0 0  nil))));; (int 0) (int 0)))))

(defn image-frame [ image ]
  (let [panel (doto (image-panel image)
                (.setPreferredSize (Dimension. 512 512)))]
    (doto (JFrame. "Image")
      (.add panel)
      (.pack)
      (.setVisible true))))


;; hidden feature ... 

;; hidden feature: writes tiles as single files (for texture packer)
(defn write-tiles-texture-packer []
  (for [{:keys [image name]} @tile-images]
    (write-image image (str name ".png"))))




(defn coords-xml [x]
  (hash-map
   :tag :array :content
   (vec (map #(hash-map :tag :string :content [(str "{" (first %) "," (second %) "}")]) (rest x)))))


(defn write-plist []
  (let [background-name (background :name)
        objects (rest ((loaded-level :data) 2))
        key-name (fn [x] (hash-map :tag :key :content [(str (first x))]))
        key-names (vec (map key-name objects))
        keys (interleave key-names (map coords-xml objects))]
    (emit-element
     {:tag :dict :content
      [{:tag :key :content ["background"]}
       {:tag :string :content [background-name]}
       {:tag :key :content ["objects"]}
       {:tag :dict :content keys}]})))

(defn write-plist-header []
  (str "<?xml version=\"1.0\" encoding=\"UTF-8\"?>" "\n"
       "<!DOCTYPE plist PUBLIC \"-//Apple//DTD PLIST 1.0//EN\" \"http://www.apple.com/DTDs/PropertyList-1.0.dtd\">" "\n"
       "<plist version=\"1.0\">" "\n"))

;; hidden feature: write level as plist xml format (cocos2d)
(defn write-plist-file []
  (let [name (loaded-level :name)
        filename (str @current-path (apply str (drop-last 4 (loaded-level :name))) ".plist")]
    (spit filename (str (write-plist-header) (clojure.string/replace (with-out-str (write-plist)) #"\n" "") "</plist>" ))))

(defn write-plists [ path ]
  (let [dirs (get-level-directories path)]
    (doseq [d dirs]
      (let [name (str path d ".clj")]
        (println "generate " name)
        (load-level* (java.io.File. (str path d ".clj")))
        (write-plist-file)))))


(defn files-with-suffix [path suffix]
   (filter #(.endsWith % suffix) (map #(.getName %) (.listFiles (java.io.File. path)))))

(defn get-svgs [path]
  (files-with-suffix path ".svg"))

(defn polygons-to-plist [path ]
  (let [polygon-names (get-svgs path)
        read-svg (read-svg path)
        polygons (for [p polygon-names] (:polygon (read-svg p)))
        to-png (fn [x] (str (apply str (drop-last 3 x)) "png"))
        key-name (fn [x] (hash-map :tag :key :content [(to-png x)]))
        names (vec (map key-name polygon-names))
        keys (interleave names (map coords-xml polygons))]
    (emit-element
     {:tag :dict :content (vec keys)})))
        

(defn write-polygons-plist [path]
  (let [filename "polygons.plist"]
    (spit filename (str (write-plist-header) (clojure.string/replace (with-out-str (polygons-to-plist path)) #"\n" "") "</plist>")))) 