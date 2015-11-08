(import '(javax.swing JFrame JLabel JTextField JButton JPanel)
        '(java.awt.event ActionListener)
	'(java.awt.geom Ellipse2D)
        '(java.awt GridBagLayout GridBagConstraints Color Font RenderingHints)
        '(java.io File IOException)
	QuadTree
        '(javax.imageio ImageIO))
(import '(java.util Date))

(defn current-time []
  (. (new Date) (toString)))

(set! *warn-on-reflection* true)

(defn class-methods [x]
 (let [c (if (class? x) x (class x))]
  (distinct (sort (seq
                   (map #(.getName %1)
                    (.getMethods c)))))))
 


(defrecord Point [x y])
(defrecord MyColor [r g b a])
(defrecord Circle [center radius])
(defrecord BBox [xmin ymin xmax ymax])

(defrecord Pattern [branchperiod angleperiod nextpattern rparam aparam anglerange])
(defrecord Plug [circle angle level pattern])

(def width  1000)
(def height 1000)


(defn bbox-x [bbox width X]
     (* (/ (- X (-> bbox :xmin)) (- (-> bbox :xmax) (-> bbox :xmin))) width)
)

(defn bbox-y [bbox height Y]
     (* (/ (- Y (-> bbox :ymin)) (- (-> bbox :ymax) (-> bbox :ymin))) height)
)

(defn bbox-width [bbox width xsize]
     (* (/ xsize (- (-> bbox :xmax) (-> bbox :xmin))) width)
)

(defn bbox-height [bbox height ysize]
     (* (/ ysize (- (-> bbox :ymax) (-> bbox :ymin))) height)
)

(defn rendercircle [g2d bbox circle color]
  (. g2d setColor color)
  (. g2d fillOval (bbox-x bbox width  (- (-> circle :center :x) (-> circle :radius)))
                  (bbox-y bbox height (- (-> circle :center :y) (-> circle :radius)))
                  (bbox-width bbox width (* (-> circle :radius) 2))
                  (bbox-height bbox height (* (-> circle :radius) 2)))
  (let [jcircle (new java.awt.geom.Ellipse2D$Double (bbox-x bbox width  (- (-> circle :center :x) (-> circle :radius)))
                  (bbox-y bbox height (- (-> circle :center :y) (-> circle :radius)))
                  (bbox-width bbox width (* (-> circle :radius) 2))
                  (bbox-height bbox height (* (-> circle :radius) 2)))]
    (. g2d fill jcircle)))

(defn bbox-merge [bbox1 bbox2]
  (let [xmin (min (-> bbox1 :xmin)  (-> bbox2 :xmin))
	ymin (min (-> bbox1 :ymin)  (-> bbox2 :ymin))
	xmax (max (-> bbox1 :xmax)  (-> bbox2 :xmax))
	ymax (max (-> bbox1 :ymax)  (-> bbox2 :ymax))
	width  (- xmax xmin)
	height (- ymax ymin)
	dist (if (> width height) width height)
	xcenter (/ (+ xmin xmax) 2.0)
	ycenter (/ (+ ymin ymax) 2.0)
	nxmin (- xcenter (/ dist 2.0))
	nymin (- ycenter (/ dist 2.0))
	nxmax (+ xcenter (/ dist 2.0))
	nymax (+ ycenter (/ dist 2.0))]
    (BBox. nxmin nymin nxmax nymax)
  )
)

(defn bbox-circle [circle]
  (BBox. (- (-> circle :center :x) (-> circle :radius))
	 (- (-> circle :center :y) (-> circle :radius))
	 (+ (-> circle :center :x) (-> circle :radius))
	 (+ (-> circle :center :y) (-> circle :radius)))
)

(defn bboxitems  [item1 item2]
  (let [merge (bbox-merge (bbox-circle (first item1)) (bbox-circle (first item2)))]
    merge
  )  
)

(defn items-bbox [items]
  ; (reduce bbox-merge (map (fn [x] (bbox-circle (first x))) items))
  (reduce bbox-merge (map bbox-circle (map first items)))
)

(defn bbox-add-margin [bbox marginfactor]
  (BBox. (- (-> bbox :xmin) (* (- (-> bbox :xmax) (-> bbox :xmin)) marginfactor))
	 (- (-> bbox :ymin) (* (- (-> bbox :ymax) (-> bbox :ymin)) marginfactor))
	 (+ (-> bbox :xmax) (* (- (-> bbox :xmax) (-> bbox :xmin)) marginfactor))
	 (+ (-> bbox :ymax) (* (- (-> bbox :ymax) (-> bbox :ymin)) marginfactor)))
)

(defn render [g2d items ubbox]
  ; first compute bbox
  (let [bbox (if (= ubbox nil) (bbox-add-margin (items-bbox items) 0.1) ubbox)
	backcolor (new java.awt.Color 90 90 120 255)]
    (println bbox)
    (. g2d setColor backcolor)
    (. g2d fillRect 0 0 width height)
  ; then render them
    (loop [itemlist items]
      (when (seq itemlist)
	(let [[circle color] (first itemlist)]
	  ; (println "render circle" circle)
	  (rendercircle g2d bbox circle color)
	  (recur (rest itemlist)))))))
 
(defn save_image [image filename]
  (let [file (File. filename)]
      (ImageIO/write image "PNG" file)
  )
)



(defn rand-color []
  (new java.awt.Color (rand-int 256) (rand-int 256) (rand-int 256) 255))

(defn color-white []
  (new java.awt.Color 255 255 255 255))


(defn display_image [image]
    (doto (javax.swing.JFrame.)
      (.add (proxy [javax.swing.JPanel] []
	      (paint [g] (.drawImage g image 0 0 this))))
      (.setSize (java.awt.Dimension. width height))
      (.show)))

(defn point-dist [p1 p2]
  (let [x (- (-> p1 :x) (-> p2 :x))
	y (- (-> p1 :y) (-> p2 :y))
	dist2 (+ (* x x) (* y y))]
    (Math/sqrt dist2)
   )
)

(defn circle-intersect [c1 c2]
  (let [p1 (-> c1 :center)
	p2 (-> c2 :center)
	x (- (-> p1 :x) (-> p2 :x))
	y (- (-> p1 :y) (-> p2 :y))
	dist2 (+ (* x x) (* y y))
	sumrad (+ (-> c1 :radius) (-> c2 :radius))]
    (< (- dist2 (* sumrad sumrad)) (* sumrad -0.00001)))
)

(defn circle-tangent [circle angle radius]
  (let [dist (+ (-> circle :radius) radius)
	newx (+ (-> circle :center :x) (* (Math/cos angle) dist))
	newy (+ (-> circle :center :y) (* (Math/sin angle) dist))]
    (Circle. (Point. newx newy) radius)
  )
)

(defn circle-colliding? [quadtree circle]
  (let [bbox (bbox-circle circle)
	subcircles (.getIntersected quadtree (-> bbox :xmin) (-> bbox :ymin) (-> bbox :xmax) (-> bbox :ymax))
	result (first (drop-while false? (map #(circle-intersect circle %) subcircles)))]
    ; (println "intersect count" (count subcircles))
    (when (not result)
      (.add quadtree (-> bbox :xmin) (-> bbox :ymin) (-> bbox :xmax) (-> bbox :ymax) circle)
    )
    ; (println "colliding result" result "circle" circle)
    result
  )
)

(defn ncircles [quadtree ncircles]
    (for [i (range ncircles)
	  :let [circle (Circle. (Point. (rand) (rand)) (* 0.03 (rand)))]
	  :when (not (circle-colliding? quadtree circle))]
      (list circle (color-white)))
    )

(defn nextadjcircles [quadtree ncircles]
  (let [circle (Circle. (Point. 0.0 0.0) 1.0)
	result [(list circle (rand-color))]]
    (circle-colliding? quadtree circle)
    (loop [result result
	   niter 0]
      (if (< (count result) ncircles)
	(let [nextcircle (circle-tangent (first (rand-nth result)) (* (rand) 6.28) (- 1.0 (* niter 0.0000005)))]
	  (when (= (rem (count result) 1000) 0)
	    (println "count result" (count result)))
	  (recur (if (not (circle-colliding? quadtree nextcircle)) (conj result (list nextcircle (rand-color))) result)
		 (inc niter)))
	  result))))

(defn drange [start stop nitems]
  (if (= nitems 0)
    [start]
    (if (= nitems 1)
      [start]
      (let [incr (/ (- stop start) nitems)]
	(map #(+ start (* incr %)) (range 0 nitems))))))

(defn nextplugs [patternmap nextplug]
  (let [nextcircle  (-> nextplug :circle)
	nextangle   (-> nextplug :angle)
	nextlevel   (-> nextplug :level)
	nextpattern (-> nextplug :pattern)
	next2pattern (-> (get patternmap nextpattern) :nextpattern)
	branchperiod (-> (get patternmap nextpattern) :branchperiod)
	angleperiod  (-> (get patternmap nextpattern) :angleperiod)
	[minangle maxangle] (-> (get patternmap nextpattern) :anglerange)
	nextincrangles (drange (* minangle 3.14156) (* maxangle 3.14156) angleperiod)
	newpattern (if (== nextlevel 0) next2pattern nextpattern) 
	normalplug (Plug. nextcircle nextangle nextlevel newpattern)
	result (if (= (rem nextlevel branchperiod) 0)
		 (reverse (conj (map #(Plug. nextcircle (+ nextangle %) nextlevel next2pattern) (rest nextincrangles)) normalplug))
		 [normalplug])]
    result
    )
  )
	
(defn firstplug [plugs]
  [(first plugs) (rest plugs)]
)

(defn lastplug [plugs]
  [(last plugs) (drop-last plugs)]
)

(defn randplug [plugs]
  (let [index (int (* (rand) (count plugs)))
	item (nth plugs index)
	[firstpart lastpart] (split-at index plugs)
	restlist (concat firstpart (rest lastpart))]
    ; (println "plugs" plugs)
    ; (println "index" index "randplug" item "restlist" restlist)
    [item restlist]
  )
)


(defn nextparams [lastcircle lastangle rparam aparam]
  [(* (-> lastcircle :radius) rparam) (+ lastangle aparam)]
)


(defn nextreccircles [quadtree ncircles]
  (let [patternmap {:pattern1 (Pattern. 3 7 :pattern2 0.95 0.05  [-1.0 1.0])
		    :pattern2 (Pattern. 10 4 :pattern1 0.95 0.05  [-1.0 1.0])}
	circle (Circle. (Point. 0.0 0.0) 1.0)
	initplug (Plug. circle 0.0 0 :pattern2)
	result [[circle (color-white)]]]
    (circle-colliding? quadtree circle)
    (loop [result result
	   plugs (nextplugs patternmap initplug)
	   niter 0]
      (if (and (< (count result) ncircles) (> (count plugs) 0))
	(let [[nextplug restplugs] (firstplug plugs)
	      nextcircle (-> nextplug :circle)
	      nextangle  (-> nextplug :angle)
	      nextlevel  (-> nextplug :level)
	      rparam     (-> (get patternmap (-> nextplug :pattern)) :rparam)
	      aparam     (-> (get patternmap (-> nextplug :pattern)) :aparam)
	      [newradius newangle] (nextparams nextcircle nextangle rparam aparam)
	      newcircle  (circle-tangent nextcircle newangle newradius)
	      isnextOK    (and (> (-> newcircle :radius) 0.001) (not (circle-colliding? quadtree newcircle)))
	      newresult  (if isnextOK [newcircle (color-white)] [])
	      newplugs   (if isnextOK (nextplugs patternmap (Plug. newcircle newangle (+ nextlevel 1) (-> nextplug :pattern))) [])]
	  (when (= (rem niter 5000) 0)
	    (println "count result" (count result) "niter" niter "time" (current-time) "nplugs" (count plugs)))
	  (recur (if (= (count newresult) 2) (conj result newresult) result)
		 (concat (reverse newplugs) restplugs)
		 (inc niter)))
	result))))



(defn drawimage []
  (let [image (java.awt.image.BufferedImage. width height java.awt.image.BufferedImage/TYPE_INT_RGB)
	g2d (.createGraphics image)
	quadtree (QuadTree.)
	items (nextreccircles quadtree 5000000)]
    (. g2d setRenderingHint (. RenderingHints KEY_ANTIALIASING) (. RenderingHints VALUE_ANTIALIAS_ON))
    (println "items size" (count items))
    (render g2d items nil); (BBox. -5.0 -70.0 50.0 -15.0))
    (save_image image "d:/DEV/CVG/output/cjvg.ppm")
    ; (display_image image)
  )
)

; (dotimes [i 10] (time (drawimage)))
(drawimage)
; -- API END --