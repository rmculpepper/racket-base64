#lang info

;; pkg info

(define version "1.1")
(define collection "base64")
(define blurb "Base64 encoding and decoding, similar to net/base64 but with more options (documentation only)")
(define deps '("base" "net-lib" "rackunit-lib" "base64-lib"))
(define build-deps '("racket-doc" "scribble-lib" "net-doc"))
(define pkg-authors '(ryanc))
(define license 'Apache-2.0)

;; collection info

(define name "base64")
(define scribblings '(("base64.scrbl" ())))
