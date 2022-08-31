(define-module (minigrad libgv)
  #:export (digraph node layout render setv))

(load-extension "libgv_guile" "SWIG_init")
