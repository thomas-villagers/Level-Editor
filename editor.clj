(ns editor
  (:import (java.awt Color Dimension BorderLayout GridLayout Graphics2D BasicStroke)
           (javax.swing JPanel JFrame Timer JOptionPane JMenu JMenuBar JMenuItem JFileChooser JScrollPane JTextField JLabel JSeparator JSpinner JButton SpinnerNumberModel JDialog JSlider JTable ImageIcon
                        SpringLayout Spring)
           (javax.swing.table AbstractTableModel)
           (javax.imageio ImageIO)
           (javax.swing.event ChangeListener TableModelListener)
           (java.awt.event ActionListener KeyListener MouseListener MouseWheelListener  WindowAdapter))
  (:use macros))

(def screen-sizes { :desire-hd [800 480] :iconia-500 [1280 800]})

(def tile-images (ref nil))
(def levelmap (ref {}))
(def scale (ref 1.0))
(def raster (ref [32 32]))
(def background-image (ref nil))
(def screen-size (ref (screen-sizes :iconia-500)))
(def world-size (ref [1 1]))
(def current-path (ref  "."))

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
            ([file] (dosync (ref-set image (ImageIO/read file))
                            (ref-set image-name  file))
               @image))
   :tiles (fn [] @tiles) ;; TODO: ... 
   :set (fn [k v]
          (let [r (k {:size size :current current :image image :tiles tiles })]
            (dosync (ref-set r v))))})

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
    (mouseEntered [e] )
    (mouseExited [e])
    (mousePressed [e])
    (mouseReleased [e])))

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

(defn draw-grid [g w h dx dy]
  (doseq [n (range 0 w dx)]           
    (.drawLine g n 0 n h))
  (doseq [n (range 0 h dy)]
    (.drawLine g 0 n w n)))

(defn tilemap-panel [ image ]                                      
  (proxy [JPanel ActionListener] []
    (paintComponent [g]
      (proxy-super paintComponent g)
      (.drawImage g image 0 0 nil)
      (doseq [{:keys  [x y width height]} @tile-images]
        (.drawRect g x y width height)))
    (actionPerformed [e]
      (.repaint this))))

(defn show-tilemap [ ]
  (let [image (tile :image)
        [w h] [ (.getWidth image) (.getHeight image)]
        panel (doto (tilemap-panel image)
                (.addMouseListener (tilemap-listener w h))
                (.setPreferredSize (Dimension. w h)))
        timer (Timer. 60 panel)]
    (.start timer)
    (doto (JFrame. "Tiles")
      (.add panel)
      (.pack)
      (.setVisible true))))

(defn init-tilemap [file ]
  (let [image (tile :image file)
        [w h] (tile :size)]
    (dosync (ref-set tile-images (make-tile-images image w h)))
    (show-tilemap)))

(defn open-dialog [parent desc exts title]
  (let [filter (javax.swing.filechooser.FileNameExtensionFilter. desc exts)
        chooser (doto
                    (JFileChooser. (java.io.File. @current-path))
                  (.setFileFilter filter)
                  (.setDialogTitle title))
        value    (.showOpenDialog chooser parent)]
    (when (= value JFileChooser/APPROVE_OPTION)
      (.getSelectedFile chooser))))

(defn open-dialog-png [parent title]
  (open-dialog parent "PNG Images" (into-array [ "png" ]) title))

(defn save-dialog [parent]
  (let [filter (javax.swing.filechooser.FileNameExtensionFilter. "Tile Maps" (into-array [ "tile" ]))
        chooser (doto
                    (JFileChooser. (java.io.File. "."))
                  (.setFileFilter filter))
        value  (.showSaveDialog chooser parent)]
    (when (= value JFileChooser/APPROVE_OPTION)
      (.getSelectedFile chooser))))
                                  
(defn background-dialog [parent]
  (when-let [file (open-dialog-png parent "Load Background Image")]
    (dosync (ref-set background-image (ImageIO/read file)))))

(defn tilemap-dialog [parent]                
  (let [dialog (JDialog.) ;; parent)
        [w h] (tile :size)
        spinnerx (JSpinner. (SpinnerNumberModel. w 1 256 1))
        spinnery (JSpinner. (SpinnerNumberModel. h 1 256 1))
        button   (doto (JButton. "OK")
                   (.addActionListener 
                    (proxy [ActionListener] []
                      (actionPerformed [e] 
                        (let [x (.getValue spinnerx)
                              y (.getValue spinnery)]
                          (tile :set :size [x y])
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
        (let [[k v] ((vec @levelmap) row)
              r [(first k) (second k) (:tile v) (:active v)]]
          (r col)))
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
        (let [[k v] ((vec @levelmap) 0)
              r [(first k) (second k) (:tile v) (:active v)]]
          (class (r col)))))))

(defn watch-table [ model reference tag ]
  (add-watch reference tag (fn [r k old new] (.fireTableDataChanged model))))

(defn world-table []
  (let [frame (JFrame. "World Table")
        model (world-table-model)
        table (JTable. model)]
     (watch-table model levelmap :world-table)
     (doto frame
      (.add (JScrollPane. table))
      (.pack)
      (.setVisible true))))

(defn row-height-update* [table]
  (fn [row val]
    (.setRowHeight table row val)))

(declare row-height-update)

(defn tile-table-model []          
  (let [names ["Tile" "Nr" "Name" "x" "y" "width" "height" "Active"]
        data [(ImageIcon.) 0 "" 0 0 0 0 true]]
    (proxy [AbstractTableModel] []
      (getColumnCount [] (count names))
      (getRowCount [] (count @tile-images))
      (getColumnName [col]
        (names col))
      (getValueAt [row col]
        (let [r (@tile-images row)]
          (case col
            0  (ImageIcon. (:image r))
            1 row
            2 (:name r)
            3 (:x r)
            4 (:y r)
            5 (:width r)
            6 (:height r)
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

(defn tile-table []
  (let [frame (JFrame. "Tile Table")
        model (tile-table-model)
        table (JTable. model)]
    (def row-height-update (row-height-update* table))
    (watch-table model (tile :get-ref :current) :tile-table)
    (watch-table model tile-images  :tile-table2)
    (.setRowHeight table (+ 2 (second (tile :size))))
    (doto frame
      (.add (JScrollPane. table))
      (.pack)
      (.setVisible true))))

(defn save-tile-map [parent]
  (when-let [filename (save-dialog parent)]
    (with-open [f (clojure.java.io/writer filename)]
      (do
        (println (tile :image-name))
        (.write f (println-str (tile :image-name)))
        (let [tiles (remove (comp nil? :name) @tile-images)
              func (juxt :name :x :y :width :height)]
          (doseq-to tiles
                    #(.write f (println-str (apply list (func %))))))))))

(defn load-tile-map* [filename]  
  (let [lines   (line-seq (java.io.BufferedReader. (java.io.StringReader. (slurp filename))))
        image (tile :image  (java.io.File. (first lines)))]
    (vec (for [x (rest lines)]
           (update-tile-image image (conj (zipmap [:name :x :y :width :height] (read-string x)) {:image :nil}))))))

(defn load-tile-map [parent]
  (when-let [file (open-dialog parent "Tile files" (into-array [ "tile" ]) "Load Tile Map")]
    (dosync (ref-set tile-images (load-tile-map* (.getName file))))
    (tile :current 0)
    (show-tilemap)))

(defn load-data [filename]
  (with-open [rdr (java.io.PushbackReader. (java.io.StringReader. (str "(" (slurp filename) ")" )))]
    (binding [*read-eval* false]
      (reduce conj [] (read rdr)))))

(defn parse-object [ line ]
  (let [name (first line)
        {:keys [width height] :as tile} (first (filter #(= (:name %) name) @tile-images))
        data (map #(map - % [(* 0.5 width)  (* 0.5 height)]) (rest line))
;;        data (map #(list (- (first %) (* 0.5 width)) (- (second %) (* 0.5 height)))  (rest line))    ;; map #(map + [w/2 h/2] %) (rest line) ) !!! 
        index (.indexOf @tile-images tile )
        value {:tile index :active true}]
    (zipmap data (repeat value))))

(defn load-level* [file]
  (let [filename (.getName file)
        path (.getPath file)
        x (load-data path)
        path (apply str (drop-last (count filename) path))
        bgr-image ((comp str second first ) (filter #(= 'textures (first %)) x))
        o (filter #(= 'objects (first %)) x)]
    (dosync (ref-set background-image (ImageIO/read (java.io.File. (str path bgr-image))))
            (ref-set current-path path)
            (ref-set levelmap
                     (into {} (map parse-object (rest (first o))))))
    o))

(defn load-level [parent]
  (when-not @tile-images
    (JOptionPane/showMessageDialog parent "Load a tile map first","Info", JOptionPane/INFORMATION_MESSAGE)
    (load-tile-map parent))
  (when-let [file (open-dialog parent "Level files" (into-array [ "clj" ]) "Load Level")]
    (load-level* file)))

;; SAVE LEVEL:
;; -- if level loaded (<<-- filename !)
;;    -- adjust objects & background in level
;; -- else
;;    -- make new level
;; 1. step: levelmap to list !

(defn level-to-list [level]
  (let [tile-nrs (distinct (map (comp :tile val) level))]
    (cons 'objects
          (map (fn [x]
                 (let [{:keys [name width height]} (@tile-images x)
                       [width height] (map (partial * 0.5) [width height])
                       objs (keys (filter #(= x (:tile (val %))) level))            ;; todo: filter active !! 
                       sh-objs (map #(map + [width height] %) objs)]
                   (cons name sh-objs))) tile-nrs))))


(def menus
  {"New Tile Map"
   (fn [parent]
     (tilemap-dialog parent))
   "Load Tile Map"
   (fn [parent]
     (load-tile-map parent ))
   "Save Tile Map"
   (fn [parent]
     (save-tile-map parent))
   "Load Level"
   (fn [parent]
     (load-level parent))
   "Background Image"
   (fn [parent]
     (background-dialog parent))
   "Screen Size"
   (fn [parent]
     (let [sizes (into-array (vec (map #(str (key %) " " (val %)) screen-sizes)))
           selected  (JOptionPane/showInputDialog parent "Choose Device" "Screen Size" JOptionPane/INFORMATION_MESSAGE nil sizes (first sizes))]
       (when selected
         (dosync (ref-set screen-size ((read-string selected) screen-sizes))))))
   "World Table"      (fn [_] (world-table))
   "Tile Table"  (fn [_] (tile-table))})

(defn menu-listener [parent]
  (proxy [ActionListener] []
    (actionPerformed [e]
      ((menus (.getActionCommand e)) parent))))

;; todos: -- drag Tiles in World
;;        -- Unterstützung mehrerer Tilemaps
;;        -- raster offset
;;        -- speichere Level

(defn world-listener []
  (proxy [MouseListener] []
    (mouseClicked [e]
      (let [x (/ (.getX e) @scale)
            y (/ (.getY e) @scale)
            c (map quot [x y] @raster)
            c (map * c @raster)]
        (dosync
         (alter levelmap
                (if (= (.getButton e) (java.awt.event.MouseEvent/BUTTON3))
                  #(dissoc % c)
                  #(conj % {c {:tile (tile :current) :active true}}))))))
    (mouseEntered [e] )
    (mouseExited [e])
    (mousePressed [e])
    (mouseReleased [e])))

(defn world-panel []
  (proxy [JPanel ActionListener] [] 
    (paintComponent [g]
      (proxy-super paintComponent g)
      (let [[w h] (map * @world-size @screen-size)
            g2 (.create g)
            [rx ry] @raster]
      (.scale g2 @scale @scale)
      (when @background-image
        (.drawImage g2 @background-image nil (int 0) (int 0)))
      (.drawRect g2 0 0 w h)                          
      (draw-grid g2 w h rx ry)
      (when @tile-images
        (doseq [n (filter (comp :active val) @levelmap)]
          (let [img (:image (@tile-images (:tile (val n))))
                [x y] (key n)]
            (.drawImage g2 img nil (int x) (int y)))))
      (.setColor g2 (Color. 200 0 0))
      (.setStroke g2 (BasicStroke. (float 2)))
      (draw-grid g2 w h (first @screen-size) (second @screen-size))))
    (actionPerformed [e]
      (.repaint this))))

(defn ctile-panel []
  (proxy [JPanel ActionListener] []
    (paintComponent [g]
      (proxy-super paintComponent g)
      (when @tile-images
        (.drawImage g (:image (@tile-images (tile :current))) 0 0 nil)))
    (getPreferredSize []
      (let [[x y] (tile :size)]
      (Dimension. x y)))
    (actionPerformed [e]
      (.repaint this))))

(defn lin-int [a b]
  (fn [t]
    (+ a (* t (- b a)))))

(defn World [parent]
  (let [frame (JFrame. "World")
        [x y] (map * @screen-size @world-size)
        world (world-panel)
        timer (Timer. 60 world)
        scroll-pane (JScrollPane.)]
     (add-watch world-size :world (fn [r k old [x y]]
                                   (let [[w h] (map * [x y] @screen-size)]
                                     (.setPreferredSize world (Dimension. w h))
                                     (.setSize world (Dimension. w h)))))
    (doto world
      (.addMouseListener (world-listener))
      (.setPreferredSize (Dimension. x y)))
    (doto scroll-pane
      (.setPreferredSize (Dimension. x y))
      (.setViewportView world))
    (doto frame
      (.setLocationRelativeTo parent)
      (.add scroll-pane BorderLayout/CENTER)
      (.pack)
      (.setVisible true))
    (.start timer)))

(defn spr-layout
  ([x y w h] (javax.swing.SpringLayout$Constraints. (Spring/constant x) (Spring/constant y) (Spring/constant w) (Spring/constant h)))
  ([x y]     (javax.swing.SpringLayout$Constraints. (Spring/constant x) (Spring/constant y))))

(defn add-component
  ([p c x y]
     (.add p c (spr-layout x y)))
  ([p c x y w h]
     (.add p c (spr-layout x y w h))))

(defn add-spinner-listener [spinner func]
  (.addChangeListener spinner
                      (proxy [ChangeListener] []
                        (stateChanged [e]
                          (let [v (.getValue (.getSource e))]
                            (func v))))))

(defn App []                         
  (let [frame (JFrame. "Level Editor")
        spinnerx (JSpinner. (SpinnerNumberModel. (first @raster) 1 256 1))
        spinnery (JSpinner. (SpinnerNumberModel. (second @raster) 1 256 1))
        spinnerwx (JSpinner. (SpinnerNumberModel. (first @world-size) 1 256 1))
        spinnerwy (JSpinner. (SpinnerNumberModel. (second @world-size) 1 256 1))
        tile (ctile-panel)
        menu-bar (JMenuBar. )
        listener (menu-listener frame)
        menu (JMenu. "File")
        menu-items (into [] (map #(JMenuItem. %) (keys menus)))
        slider (JSlider. -50 50 0)
        interpolator1 (lin-int 1 10)
        interpolator2 (lin-int 1 (/ 1 10))
        timer (Timer. 60 tile)]
    (doseq-to menu-items  #(.addActionListener % listener))
    (doseq-to menu-items .add menu)
    (doto menu-bar
      (.add menu))
    (add-spinner-listener spinnerx (fn [v] (dosync (alter raster #(assoc % 0 v)))))
    (add-spinner-listener spinnery (fn [v] (dosync (alter raster #(assoc % 1 v)))))
    (add-spinner-listener spinnerwx (fn [v] (dosync (alter world-size #(assoc % 0 v)))))
    (add-spinner-listener spinnerwy (fn [v] (dosync (alter world-size #(assoc % 1 v)))))
    (.addChangeListener slider
                        (proxy [ChangeListener] []
                          (stateChanged [e]
                            (let [v (.getValue (.getSource e))
                                  s (if (pos? v)
                                        (interpolator1 (/ v 50))
                                        (interpolator2 (/ (* v -1) 50)))]
                              (dosync (ref-set scale (float s)))))))
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
    (.start timer)))


