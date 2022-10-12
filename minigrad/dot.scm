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

(define-module (minigrad dot)
  #:use-module (minigrad graph)
  #:use-module (minigrad value)
  #:use-module (minigrad operation)
  #:use-module (minigrad libgv)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:export (draw))

(define* (draw graph #:optional (port (mkstemp "/tmp/minigrad-XXXXXX")))
  "Use Graphviz to draw the GRAPH as an SVG to FILENAME."
  (define g (digraph "whatever"))
  (setv g "rankdir" "LR")
  (let ((node-contexts (map (cut find-context <> graph)
                            (topo-sort graph)))
        (edges (ufold (match-lambda*
                        ((($ <context> source label in out) acc)
                         (append (map (match-lambda
                                        (($ <link> edge-label target)
                                         (list source target edge-label)))
                                      out)
                                 (map (match-lambda
                                        (($ <link> edge-label target)
                                         (list target source edge-label)))
                                      in)
                                 acc)))
                      '() graph)))
    ;; Add a node for each value.
    (for-each (match-lambda
                (($ <context> value label in out)
                 (let* ((uid (object->string value))
                        (g:node (node g uid)))
                   (setv g:node "shape" "record")
                   (setv g:node "label"
                         (if (value? value)
                             (format #false "{ ~a | data ~,4,,f | grad ~,4,,f }"
                                     (or label "")
                                     (value-data value)
                                     (value-grad value))
                             label))

                   ;; If this value is the result of an operation, add an op
                   ;; node and connect it.
                   (when (value? value)
                     (let ((op (value-operation value)))
                       (when op
                         (let ((g:op-node (node g (string-append uid (operation-name op)))))
                           (setv g:op-node "label" (operation-name op))
                           (edge g:op-node g:node))))))))
              node-contexts)

    ;; Connect all edges by name
    (for-each (match-lambda
                ((source target edge-label)
                 (edge g
                       (object->string source)
                       (string-append
                        (object->string target)
                        (or (and (value? target)
                                 (and=> (value-operation target)
                                        operation-name)) "")))))
              edges))

  (layout g "dot")

  (let ((filename (port-filename port)))
    (close port)
    (render g "svg" filename)

    ;; Let me see this in Emacs Geiser
    (format #false "#<Image: ~a>" filename)))
