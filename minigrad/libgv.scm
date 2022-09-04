(define-module (minigrad libgv)
  #:export (digraph node edge layout render setv))

(load-extension "libgv_guile" "SWIG_init")
