#|
  This file is a part of networks-graph project.
|#

(in-package :cl-user)
(defpackage networks-graph-test
  (:use :cl
        :networks-graph))
(in-package :networks-graph-test)

;;; netcheck allows the definition of a set of nodes and network,
;;; which are enclosed in a let* so they can refer to each other, and
;;; a list of properties to be applied to the network.
(defmacro netcheck (nodes &rest body)
  `(let* ,nodes
     (unless (and ,@body)
       (error "Network failure."))))

(netcheck
 ((node-1 (networks-graph:node 'node-1 '(node-2 node-3)))
  (node-2 (networks-graph:node 'node-2 '(node-1 node-3)))
  (node-3 (networks-graph:node 'node-3 '(node-4)))
  (net    (list node-1 node-2 node-3)))
 (path-p net)
 (simple-path-p net)
 (not (valid-network-p net))
 (eql 2 (path-length net)))

(netcheck
 ((node-3 (networks-graph:node 'node-3 '(node-4)))
  (node-2 (networks-graph:node 'node-1 '(node-2 node-3)))
  (node-1 (networks-graph:node 'node-2 '(node-1 node-3)))
  (net    (list node-3 node-2 node-1)))
 (not (path-p net)))

(netcheck
 ((node-1 (networks-graph:node 'node-1 '(node-2 node-3)))
  (node-2 (networks-graph:node 'node-2 '(node-1 node-3)))
  (node-3 (networks-graph:node 'node-3 '(node-4)))
  (node-4 (networks-graph:node 'node-4 '(node-3 node-1)))
  (net    (list node-1 node-2 node-3 node-4)))
 (simple-path-p net)
 (not (cycle-p net))
 (directed-network-p net)
 (neighbour-p (link-directed node-3 node-1) node-1))

(netcheck
 ((node-1 (networks-graph:node 'node-1 '(node-2 node-3)))
  (node-2 (networks-graph:node 'node-2 '(node-1 node-3)))
  (node-3 (networks-graph:node 'node-3 '(node-4)))
  (node-4 (networks-graph:node 'node-4 '(node-3 node-1)))
  (net    (list node-1 node-2 node-1 node-3 node-4)))
 (not (simple-path-p net)))

(netcheck
 ((node-1 (networks-graph:node 'node-1 '(node-2 node-3)))
  (node-2 (networks-graph:node 'node-2 '(node-1 node-3)))
  (node-3 (networks-graph:node 'node-3 '(node-1 node-2)))
  (net    (list node-1 node-2 node-3)))
 (simple-path-p net)
 (not (cycle-p net)))

(netcheck
 ((node-1 (networks-graph:node 'node-1 '(node-2 node-3)))
  (node-2 (networks-graph:node 'node-2 '(node-1 node-3)))
  (node-3 (networks-graph:node 'node-3 '(node-1 node-2)))
  (net    (list node-1 node-2 node-3 node-1)))
 (cycle-p net))

(netcheck
 ((node-1 (networks-graph:node 'node-1 '()))
  (node-2 (networks-graph:node 'node-2 '())))
 (multiple-value-bind (node-1 node-2) (link-undirected node-1 node-2)
   (and (neighbour-p node-1 node-2)
        (neighbour-p node-2 node-1)))
 (not (neighbour-p node-2 (link-directed node-1 node-2))))

(netcheck
 ((SRI  (node 'sri  '(utah stan ucla ucsb)))
  (UCSB (node 'ucsb '(sri  ucla)))
  (UCLA (node 'ucla '(ucsb stan  sri rand)))
  (STAN (node 'stan '(sri  ucla)))
  (UTAH (node 'utah '(sri  sdc  mit)))
  (SDC  (node 'sdc  '(utah rand)))
  (RAND (node 'rand '(ucla bbn sdc)))
  (MIT  (node 'mit  '(utah bbn  linc)))
  (BBN  (node 'bbn  '(rand mit  harv)))
  (HARV (node 'harv '(bbn  carn)))
  (CARN (node 'carn '(harv case)))
  (CASE (node 'case '(carn linc)))
  (LINC (node 'linc '(case mit)))
  (ARPA-NET (list SRI
                  UCSB
                  UCLA
                  STAN
                  UTAH
                  SDC
                  RAND
                  MIT
                  BBN
                  HARV
                  CARN
                  CASE
                  LINC)))
 (equal MIT (get-node ARPA-NET 'mit))
 (equal HARV (funcall (get-node-map ARPA-NET) 'harv))
 (let* ((by-followers (sort-by-followers ARPA-NET))
        (most (apply #'max (mapcar (lambda (node)
                                     (slot-value node 'followers))
                                   by-followers)))
        (by-followers (remove-if-not (lambda (node)
                                       (= (slot-value node 'followers) most))
                                     by-followers))
        (hubs (mapcar (lambda (node)
                        (slot-value node 'node))
                      by-followers)))
   (and (member SRI hubs)
        (member UCLA hubs))
   (not (directed-network-p ARPA-NET))))

(format t "~%~%networks-graph-test: passed.~%")
