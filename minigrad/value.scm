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

(define-module (minigrad value)
  #:use-module (srfi srfi-9)
  #:export (value
            value-data
            value-grad))

(define-record-type <value>
  (make-value data grad)
  value?
  (data value-data)
  (grad value-grad))

(define* (value data #:optional (grad 0))
  (make-value data grad))
