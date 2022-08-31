;;; Copyright © 2022 Ricardo Wurmus
;;;
;;; This program is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(define-module (minigrad graph)
  #:use-module (ice-9 match)
  #:use-module (ice-9 vlist)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-9 gnu)
  #:export (graph?
            graph-null
            graph-cons
            contexts->graph
            context
            link

            partition-context
            delete-context
            match-context

            delete-node

            ufold
            gmap
            nodes
            graph-reverse
            undir

            predecessors
            successors
            neighbors
            degree

            context-successors

            ;; Algorithms
            depth-first-order
            spanning-forest
            topo-sort))

;;; Commentary:
;;;
;;; This module implements an inductive graph representation, and
;;; purely functional graph algorithms.  See "Inductive Graphs and
;;; Functional Graph Algorithms" by Martin Erwig for details.
;;; https://web.engr.oregonstate.edu/~erwig/papers/abstracts.html#JFP01
;;;
;;; Nodes can be of any type, but each node must be unique.
;;;
;;; Code:


;; A link pointing from the current node to some other node.  This is
;; used in an adjacency list.
(define-record-type <link>
  (link label target)
  link?
  (label link-label)    ;string
  (target link-target)) ;node of any type

(define-record-type <context>
  (context node label in out)
  context?
  (node context-node)   ;node of any type
  (label context-label) ;string
  (in context-in)       ;list of <link> for incoming links
  (out context-out))    ;list of <link> for outgoing links

(define-record-type <graph>
  (graph-cons* head tail nodes)  ;private constructor
  graph?
  (head graph-head)  ;<context>
  (tail graph-tail)  ;<graph> or graph-null

  ;; This field is not strictly necessary.  It contains references to
  ;; all previous nodes to make validation of node uniqueness more
  ;; performant.  Different applications may want to use a different
  ;; data structure here.
  ;;
  ;; Each entry in the vhashq points to the node's context.
  (nodes graph-nodes))  ;vhashq of the node type

(set-record-type-printer! <graph>
                          (lambda (rec port)
                            (format port "#<graph ~a nodes ~{~a~^ ~}>"
                                    (vhash-fold (lambda (key value acc)
                                                  (1+ acc))
                                                0 (graph-nodes rec))
                                    (vhash-fold (lambda (key value acc)
                                                  (cons key acc))
                                                '() (graph-nodes rec)))))

;; Identity token for the empty graph
(define graph-null '#{ %graph-null}#)
(define (graph-null? g)
  "Return #TRUE if the graph G is the empty graph."
  (eq? graph-null g))

;; Validating variant of <graph> constructor.
(define (graph-cons context graph)
  "Return a new <graph> by extending GRAPH with CONTEXT.  GRAPH may be
either GRAPH-NULL or a value of type <graph>."
  (match context
    (($ <context> node label in out)
     (match graph
       ((? graph-null?)
        (graph-cons* context graph
                     (vhash-consq (context-node context)
                                  context vlist-null)))
       (($ <graph> head tail nodes)
        (match context
          (($ <context> node label in out)
           (when (vhash-assq node nodes)
             (error (format #false "Node ~a (~a) already exists in graph.~%" label node)))
           (for-each
            (match-lambda
              (($ <link> label target)
               (when (eq? node target)
                 (error (format #false "Node ~a (~a) cannot be adjacent to itself.~%" label node)))
               (unless (vhash-assq target nodes)
                 (error (format #false "Node ~a (~a) may not reference unknown node ~a.~%" label node target)))))
            (append in out))
           (graph-cons* context graph
                        (vhash-consq node context nodes)))))
       (_ (error "type error: second argument must be <graph> or graph-null."))))
    (_ (error
        (format #false
                "type error: first argument must be <context>, got: ~a.~%"
                context)))))

(define (contexts->graph contexts)
  (fold graph-cons graph-null contexts))


(define (partition-context pivot g)
  "Find the context PIVOT in <graph> G and return two values: a list
of contexts before the pivot and the remaining graph after it."
  (define (inner before rest-g)
    (match rest-g
      ((? graph-null?)
       (values before rest-g))
      (($ <graph> (? context? head) tail nodes)
       (if (eq? head pivot)
           (values before tail)
           (inner (cons head before) tail)))))
  (inner '() g))

(define (delete-context ctx g)
  "Return a new <graph> that is just like G except that the context CTX and
any reference to its node has been removed."
  (call-with-values (lambda () (partition-context ctx g))
    (lambda (before rest-graph)
      ;; Delete context node in all adjacency lists in "before", and
      ;; graph-cons them onto the unmodified rest-graph.  This works
      ;; because rest-graph cannot possibly contain a reference to the
      ;; node in context.
      (let* ((node-to-delete (context-node ctx))
             (drop-node
              (lambda (links)
                (filter-map (match-lambda
                              ((and ($ <link> label target) link)
                               (if (eq? target node-to-delete)
                                   #false link)))
                            links))))
        (fold (match-lambda*
                ((($ <context> node label in out) acc)
                 (graph-cons (context node label
                                      (drop-node in)
                                      (drop-node out))
                             acc)))
              rest-graph
              before)))))

(define (match-context node g)
  "Find the context associated with NODE in <graph> G.  Return a pair
consisting of that context and the remaining graph without that
context."
  (cond
   ((graph-null? g) #false)
   (else
    (match (vhash-assq node (graph-nodes g))
      ((_ . (? context? c))
       (cons c (delete-context c g)))
      (_ #false)))))

(define (delete-node node g)
  (match (match-context node g)
    ((context . rest) rest)
    (_ g)))

(define (ufold f init g)
  "Fold the binary procedure F over contexts in the graph G.
The initial value for the fold is INIT.  The u stands for `unordered'
and emphasizes that the order of encountered nodes is not important."
  (match g
    ((? graph-null?) init)
    (($ <graph> (? context? head) tail)
     (f head (ufold f init tail)))))

(define (gmap f g)
  "Map the procedure F over all contexts in the graph G."
  (ufold (lambda (context acc)
           (graph-cons (f context) acc))
         graph-null g))

(define (nodes g)
  "Return a list of nodes from the graph G."
  (ufold (lambda (context acc)
           (cons (context-node context) acc))
         '() g))

(define (graph-reverse g)
  "Return a new graph by reversing the direction of all edges in G."
  (let ((swap (match-lambda
                (($ <context> node label in out)
                 (context node label out in)))))
    (gmap swap g)))

(define (undir g)
  "Return a new undirected graph from the possibly directed graph G."
  (let ((double (match-lambda
                  (($ <context> node label in out)
                   (let ((in+out (lset-union equal? in out)))
                     (context node label in+out in+out))))))
    (gmap double g)))

(define (predecessors node g)
  "Return a list of nodes in <graph> G that are predecessors of NODE."
  (match (match-context node g)
    ((($ <context> _ _ in out) . rest)
     (map link-target in))
    (_ '())))

(define (successors node g)
  "Return a list of nodes in <graph> G that are successors of NODE."
  (match (match-context node g)
    ((($ <context> _ _ in out) . rest)
     (map link-target out))
    (_ '())))

(define (neighbors node g)
  "Return a list of nodes in <graph> G that are neighbors of NODE."
  (match (match-context node g)
    ((($ <context> _ _ in out) . rest)
     (map link-target (append in out)))
    (_ '())))

(define (degree node g)
  "Return the degree of NODE in <graph> G."
  (match (match-context node g)
    ((($ <context> _ _ in out) . rest)
     (+ (length in) (length out)))
    (_ '())))

(define (context-successors context)
  "Return the list of nodes that can be reached from the given
CONTEXT."
  (match context
    (($ <context> node label in out)
     (map link-target out))))

(define (depth-first-order nodes-to-visit graph)
  "Return a list of nodes in depth-first order.  Visit all nodes in
NODES-TO-VISIT from GRAPH."
  (if (graph-null? graph) '()
      (match nodes-to-visit
        ((node . rest-nodes)
         (match (match-context node graph)
           ((context . rest-graph)
            (cons node
                  (depth-first-order (append (context-successors context)
                                             rest-nodes)
                                     rest-graph)))
           (_
            (depth-first-order rest-nodes graph))))
        (() '()))))

;; Postorder traversal of multi-way trees.  A multi-way tree is a pair
;; consisting of a node value and a list of trees associated with it.

;; TODO: This is inefficient O(n²) because we repeatedly use append
;; here, so we need to traverse the list multiple times.  We should
;; use an O(1) queue to reduce computational complexity.
(define postorder
  (match-lambda
    ((node . trees)
     (append (append-map postorder trees)
             (list node)))))

(define (spanning-forest nodes graph)
  "Return the list of depth-first spanning trees of GRAPH."
  (define (df ns g)
    (match ns
      (() (cons '() g))
      ((first-node . rest-nodes)
       (match (match-context first-node g)
         ((ctx . rest-graph)
          (match-let* (((forest . g1)
                        (df (context-successors ctx) rest-graph))
                       ((forest' . g2)
                        (df rest-nodes g1))
                       (tree (cons first-node forest)))
            (cons (cons tree forest') g2)))
         (_ (df rest-nodes g))))))
  (match (df nodes graph)
    ((forest . remaining-graph) forest)))

(define (topo-sort graph)
  (reverse
   (append-map postorder
               (spanning-forest (nodes graph) graph))))
