#lang info

;; pkg info

(define version "1.0")
(define collection "base64")
(define deps '("base" "net-lib" "rackunit-lib" "base64-lib"))
(define build-deps '("racket-doc" "scribble-lib" "net-doc"))
(define pkg-authors '(ryanc))

;; collection info

(define name "base64")
(define scribblings '(("base64.scrbl" ())))
