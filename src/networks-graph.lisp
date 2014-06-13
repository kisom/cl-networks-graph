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

(defstruct node (label nil :type symbol) (edges nil 0:type list))

(defun sort-edges (edges)
  (sort edges
        (lambda (x y)
          (string-lessp (string x)
                        (string y)))))

(defun node (label edges)
  "node is a shortcut to creating a "
  (if (and (listp edges) (every #'symbolp edges) (symbolp label))
      (make-node :label label :edges (sort-edges edges))
      (error "Malformed node.")))

(defun neighbour-p (from to)
  (member (node-label to) (node-edges from)))

(defun valid-edges-p (net node)
  (not
   (some #'null
         (mapcar (lambda (lbl)
                   (member lbl net :key #'node-label))
                 (node-edges node)))))

(defun valid-network-p (net)
  (and (every #'node-p net)
       (every (lambda (node) (valid-edges-p net node)) net)
       (eql (length net)
            (length (remove-duplicates net :key #'node-label)))))

(defun path-p (path)
  (and (listp path)
       (cond ((endp path) nil)
             ((endp (rest path)) t)
             ((not (every #'node-p path)) nil)
             (:else
              (if (neighbour-p (first path) (second path))
                  (path-p (rest path))
                  nil)))))

(defun path-length (path)
  (- (length path) 1))

(defun simple-path-p (path)
  (and (path-p path)
       (eql (length path)
            (length (remove-duplicates path :key #'node-label)))))

(defun cycle-p (path)
  (and (path-p path)
       (eql (node-label (first path))
            (node-label (first (last path))))))

(defun sort-edges (edges)
  (sort edges
        (lambda (x y)
          (string-lessp (string x)
                        (string y)))))

(defun link-directed (from to)
  (if (neighbour-p from to)
      from
     (node (node-label from)
            (sort-edges
             (cons (node-label to)
                   (node-edges from))))))

(defun link-undirected (a b)
  (values
   (link-directed a b)
   (link-directed b a)))

(defun get-node (net lbl)
  (find lbl net :key #'node-label))

(defun get-node-map (net)
  (lambda (lbl)
    (get-node net lbl)))

(defun followers (net node)
  (let ((node (if (node-p node)
                  node
                  (get-node net node))))
    (length
     (remove-if-not (lambda (other)
                      (neighbour-p other node))
                    net))))

(defun following (net node)
  (let ((node (if (node-p node)
                  node
                  (get-node net node))))
    (length (node-edges node))))

(defstruct rank followers node)

(defun sort-by-followers (net)
  (mapcar (lambda (node)
            (make-rank :followers (followers net node)
                       :node node)
            )
          (sort net
                (lambda (a b)
                  (> (followers net a)
                     (followers net b))))))

(defun directed-node-p (net node)
  (not
   (every (lambda (lbl)
            (neighbour-p (get-node net lbl) node))
          (node-edges node))))

(defun directed-network-p (net)
  (some #'identity
   (mapcar (lambda (node)
             (directed-node-p net node))
           net)))
