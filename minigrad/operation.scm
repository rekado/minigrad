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

(define-module (minigrad operation)
  #:use-module (minigrad graph)
  #:use-module (minigrad value)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:export (apply-operation
            add
            multiply))

(define-record-type <operation>
  (operation name proc backward)
  operation?
  (name operation-name)   ;string
  (proc operation-proc)   ;procedure
  (backward operation-backward)) ;procedure

(define add
  (operation "+" +
             (lambda (out a b)
               (values (+ (value-grad a) (value-grad out))
                       (+ (value-grad b) (value-grad out))))))

(define multiply
  (operation "*" *
             (lambda (out a b)
               (values (+ (value-grad a) (* (value-data b)
                                            (value-grad out)))
                       (+ (value-grad b) (* (value-data a)
                                            (value-grad out)))))))

(define (apply-operation graph op a b)
  "This is a monadic procedure that takes an existing GRAPH, builds
new contexts for the input nodes A and B if necessary, and adds a new
context for the result of applying the operation OP to A and B.  It
returns a new graph."
  (let* ((result-node
          (value ((operation-proc op)
                  (value-data a)
                  (value-data b))))
         (result-ctx
          (context result-node
                   (operation-name op)
                   (list)               ;incoming links
                   (list                ;outgoing links
                    (link (format #false "~a~a" (operation-name op) a) a)
                    (link (format #false "~a~a" (operation-name op) b) b))))
         ;; If a or b don't exist in the graph: add contexts for them
         (contexts
          (filter-map (lambda (node)
                        (or (find-context node graph)
                            (context node (format #false "~a" node) (list) (list))))
                      (list a b))))
    (graph-cons result-ctx (contexts->graph contexts graph))))
