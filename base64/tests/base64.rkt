#lang racket/base
(require rackunit
         base64
         (prefix-in r: net/base64))

(define (test expect f . args)
  (unless (equal? expect (apply f args))
    (error "fail")))

(check-equal? (base64-encode #"") #"")
(check-equal? (base64-encode #"" #:line 60 #:line-sep #"\r\n") #"")

(define (check-standard plain encoded)
  (check-equal? (base64-encode plain #:pad? #t) encoded)
  (check-equal? (base64-decode encoded) plain))

;; From RFC 4648 Section 10:
(check-standard #"" #"")
(check-standard #"f" #"Zg==")
(check-standard #"fo" #"Zm8=")
(check-standard #"foo" #"Zm9v")
(check-standard #"foob" #"Zm9vYg==")
(check-standard #"fooba" #"Zm9vYmE=")
(check-standard #"foobar" #"Zm9vYmFy")

;; More tests

(define (check-strict plain encoded)
  (check-equal? (base64-encode plain) encoded)
  (check-equal? (base64-decode encoded #:mode 'strict) plain))
(check-strict #"" #"")
(check-strict #"f" #"Zg")
(check-strict #"fo" #"Zm8")
(check-strict #"foo" #"Zm9v")
(check-strict #"foob" #"Zm9vYg")
(check-strict #"fooba" #"Zm9vYmE")
(check-strict #"foobar" #"Zm9vYmFy")

(define (check-lined plain encoded)
  (check-equal? (base64-encode plain #:line 4 #:line-sep #"\n" #:pad? #t) encoded)
  (check-equal? (base64-decode encoded) plain))
(check-lined #"" #"")
(check-lined #"f" #"Zg==\n")
(check-lined #"fo" #"Zm8=\n")
(check-lined #"foo" #"Zm9v\n")
(check-lined #"foob" #"Zm9v\nYg==\n")
(check-lined #"fooba" #"Zm9v\nYmE=\n")
(check-lined #"foobar" #"Zm9v\nYmFy\n")

;; Random testing for agreement with net/base64
(for ([i (in-range 10)])
  (define LEN (+ 320 (random 8)))
  (define bs (make-bytes LEN))
  (for ([k (in-range LEN)]) (bytes-set! bs k (random 256)))

  (define r-enc (r:base64-encode bs #"\r\n"))
  (define enc (base64-encode bs #:line 72 #:line-sep #"\r\n" #:pad? #t))
  (check-equal? enc r-enc)

  (check-equal? (r:base64-decode enc) bs)
  (check-equal? (base64-decode r-enc) bs))
