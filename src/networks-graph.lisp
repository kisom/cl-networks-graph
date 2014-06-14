#|
  This file is a part of networks-graph project.
|#

(in-package :cl-user)
(defpackage networks-graph
  (:use :cl)
  (:export
   :node
   :node-p
   :node-label
   :node-edges
   :neighbour-p
   :valid-network-p
   :path-p
   :path-length
   :simple-path-p
   :cycle-p
   :link-directed
   :link-undirected
   :get-node
   :get-node-map
   :followers
   :following
   :sort-by-followers
   :directed-network-p))
(in-package :networks-graph)

;;; hacks and glory await

(defstruct node
  "NODE represents a node in the graph. It has a LABEL, which should
   be a SYMBOL, and edges, which is a list of SYMBOLs."
  (label nil :type symbol) (edges nil :type list))

(defun sort-edges (edges)
  (sort edges
        (lambda (x y)
          (string-lessp (string x)
                        (string y)))))

(defun node (label edges)
  "node is a shortcut to creating a new node. LABEL should be a
   SYMBOL, and EDGES should be a LIST of SYMBOLs."
  (if (and (listp edges) (every #'symbolp edges) (symbolp label))
      (make-node :label label :edges (sort-edges edges))
      (error "Malformed node.")))

(defun neighbour-p (from to)
  "Returns true if FROM has an edge connecting it directly with TO."
  (member (node-label to) (node-edges from)))

(defun valid-edges-p (net node)
  (not
   (some #'null
         (mapcar (lambda (lbl)
                   (member lbl net :key #'node-label))
                 (node-edges node)))))

(defun valid-network-p (net)
  "A valid network is a list of nodes such that every node contained
   in all edge lists are defined in the network"
  (and (every #'node-p net)
       (every (lambda (node) (valid-edges-p net node)) net)
       (eql (length net)
            (length (remove-duplicates net :key #'node-label)))))

(defun check-path (path seen duplicates)
  (cond ((not (listp path)) (values nil nil))
        ((endp path) (values nil nil))
        ((endp (rest path)) (values t duplicates))
        ((not (node-p (first path))) (values nil nil))
        (:else
         (if (neighbour-p (first path) (second path))
             (let ((duplicates (or duplicates
                                   (member (first path) seen))))
               (check-path (rest path)
                           (union seen (list (first path)))
                           duplicates))
             (values nil nil)))))

(defun path-p (path)
  "Returns true if a valid path is present in PATH. This implies that
   there is an edge from each node to the next in the list."
  (multiple-value-bind (path-p duplicates)
      (check-path path nil nil)
    (declare (ignore duplicates))
    path-p))

(defun path-length (path)
  "Returns the number of edges in the path."
  (- (length path) 1))

(defun simple-path-p (path)
  "Returns true if the path contains no duplicate elements."
  (multiple-value-bind (path-p duplicates)
      (check-path path nil nil)
    (and path-p (not duplicates))))

(defun cycle-p (path)
  "Return true if the path is a cycle."
  (and (path-p path)
       (eql (node-label (first path))
            (node-label (first (last path))))))

(defun link-directed (from to)
  "Return a new copy of FROM whose edges contain TO."
  (if (neighbour-p from to)
      from
     (node (node-label from)
            (sort-edges
             (cons (node-label to)
                   (node-edges from))))))

(defun link-undirected (a b)
  "Returns a pair of values with new copies of both nodes with edges
   containing each other."
  (values
   (link-directed a b)
   (link-directed b a)))

(defun get-node (net lbl)
  "Look up the node associated with LBL in NET."
  (find lbl net :key #'node-label))

(defun get-node-map (net)
  "Returns a lambda form (i.e. suitable for MAPCAR) that takes one
   argument, a node, and calls (GET-NODE NET NODE)."
  (lambda (lbl)
    (get-node net lbl)))

(defun followers (net node)
  "Return a list of nodes following NODE in NET, i.e. those nodes with
   an edge into NODE."
  (let ((node (if (node-p node)
                  node
                  (get-node net node))))
    (remove-if-not (lambda (other)
                     (neighbour-p other node))
                   net)))

(defun following (net node)
  "Return a list of edges out of NODE."
  (let ((node (if (node-p node)
                  node
                  (get-node net node))))
    (length (node-edges node))))

(defun sort-by-followers (net)
  "Sort NET in descending order by the number of followers each node
   has. Each element in this list is a plist with :FOLLOWERS and :NODE
   indicators."
  (mapcar (lambda (node)
            (list :followers (length (followers net node))
                       :node node))
          (sort net
                (lambda (a b)
                  (> (length (followers net a))
                     (length (followers net b)))))))

(defun directed-node-p (net node)
  (not
   (every (lambda (lbl)
            (neighbour-p (get-node net lbl) node))
          (node-edges node))))

(defun directed-network-p (net)
  "Return T if not every edge in every node in NET has a link back to
   the node."
  (some #'identity
   (mapcar (lambda (node)
             (directed-node-p net node))
           net)))
