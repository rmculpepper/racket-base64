;; Copyright 2020-2022 Ryan Culpepper
;; SPDX-License-Identifier: Apache-2.0

#lang racket/base
(require racket/match
         rackunit
         base64
         (prefix-in r: net/base64))

(check-equal? (base64-encode #"") #"")
(check-equal? (base64-encode #"" #:line 60 #:line-end #"\r\n") #"")

;; ----------------------------------------
;; From RFC 4648 Section 10

(define (check-standard plain encoded)
  (check-equal? (base64-encode plain #:pad? #t) encoded)
  (check-equal? (base64-encode (open-input-bytes plain) #:pad? #t) encoded)
  (check-equal? (base64-decode encoded) plain)
  (check-equal? (base64-decode (open-input-bytes encoded)) plain))

(check-standard #"" #"")
(check-standard #"f" #"Zg==")
(check-standard #"fo" #"Zm8=")
(check-standard #"foo" #"Zm9v")
(check-standard #"foob" #"Zm9vYg==")
(check-standard #"fooba" #"Zm9vYmE=")
(check-standard #"foobar" #"Zm9vYmFy")

;; No-pad, no-line version
(define (check-strict plain encoded)
  (check-equal? (base64-encode plain) encoded)
  (check-equal? (base64-encode (open-input-bytes plain)) encoded)
  (check-equal? (base64-decode encoded #:mode 'strict) plain)
  (check-equal? (base64-decode (open-input-bytes encoded) #:mode 'strict) plain))
(check-strict #"" #"")
(check-strict #"f" #"Zg")
(check-strict #"fo" #"Zm8")
(check-strict #"foo" #"Zm9v")
(check-strict #"foob" #"Zm9vYg")
(check-strict #"fooba" #"Zm9vYmE")
(check-strict #"foobar" #"Zm9vYmFy")

;; Short line + padding
(define (check-lined plain encoded)
  (check-equal? (base64-encode plain #:line 4 #:line-end #"\n" #:pad? #t) encoded)
  (check-equal? (base64-encode (open-input-bytes plain) #:line 4 #:line-end #"\n" #:pad? #t) encoded)
  (check-equal? (base64-decode encoded) plain)
  (check-equal? (base64-decode (open-input-bytes encoded)) plain))
(check-lined #"" #"")
(check-lined #"f" #"Zg==\n")
(check-lined #"fo" #"Zm8=\n")
(check-lined #"foo" #"Zm9v\n")
(check-lined #"foob" #"Zm9v\nYg==\n")
(check-lined #"fooba" #"Zm9v\nYmE=\n")
(check-lined #"foobar" #"Zm9v\nYmFy\n")

;; ----------------------------------------
;; Random testing

(define (random-bytes len)
  (define bs (make-bytes len))
  (for ([k (in-range len)]) (bytes-set! bs k (random 256)))
  bs)

;; Agreement with net/base64
(define (random-test/len LEN)
  (define bs (random-bytes LEN))
  (define r-enc (r:base64-encode bs #"\r\n"))
  (define enc (base64-encode bs #:line 72 #:line-end #"\r\n" #:pad? #t))
  (check-equal? enc r-enc)
  (check-equal? (r:base64-decode enc) bs)
  (check-equal? (base64-decode r-enc) bs))
(for ([LEN (in-range 20)])
  (random-test/len LEN))
(for ([i (in-range 100)])
  (define LEN (+ 320 (random 20)))
  (random-test/len LEN))

(define (add-whitespace4 buf)
  (define (make-whitespace len)
    (make-string len #\space))
  (define out (open-output-bytes))
  (for ([b (in-bytes buf)] [k (in-naturals)])
    (when (zero? (remainder k 4))
      (write-string (make-whitespace (random 4)) out))
    (write-byte b out))
  (get-output-bytes out))

;; Roundtrip + rountrip after whitespace insertion
(for ([i (in-range 100)])
  (define LEN (+ 200 (random 20)))
  (define bs (random-bytes LEN))
  (define enc (base64-encode bs))
  ;; Check rountrip
  (check-equal? (base64-decode enc) bs)
  (check-equal? (base64-decode (open-input-bytes enc)) bs)
  ;; Check insertion of whitespace (at mod 4 positions) preserves meaning
  (for ([j (in-range 5)])
    (check-equal? (base64-decode (add-whitespace4 enc)) bs)
    (check-equal? (base64-decode (open-input-bytes (add-whitespace4 enc))) bs)))

(define (add-whitespace1 buf)
  (define (make-whitespace len)
    (make-string len #\space))
  (define out (open-output-bytes))
  (for ([b (in-bytes buf)] [k (in-naturals)])
    (write-string (make-whitespace (random 2)) out)
    (write-byte b out))
  (get-output-bytes out))

(test-case "Roundtrip + rountrip after whitespace insertion"
  (for ([i (in-range 100)])
    (define LEN (+ 200 (random 20)))
    (define bs (random-bytes LEN))
    (define enc (base64-encode bs))
    ;; Check rountrip
    (check-equal? (base64-decode enc) bs)
    (check-equal? (base64-decode (open-input-bytes enc)) bs)
    ;; Check insertion of whitespace (at any position) preserves meaning
    (for ([j (in-range 5)])
      (check-equal? (base64-decode (add-whitespace1 enc) #:mode 'padding) bs)
      (check-equal? (base64-decode (open-input-bytes (add-whitespace1 enc)) #:mode 'padding) bs))))

;; ----------------------------------------
;; Error tests

(check-exn (lambda (e)
             (match e
               [(exn:fail:read (regexp #rx"bad base64 encoding") _
                               (list (srcloc 'string 1 4 5 #f)))
                #t]
               [_ #f]))
           (lambda () (base64-decode "ABCD$")))

(check-exn (lambda (e)
             (match e
               [(exn:fail:read (regexp #rx"bad base64 encoding") _
                               (list (srcloc 'bytes 2 4 6 #f)))
                #t]
               [_ #f]))
           (lambda () (base64-decode #"\nABCD$")))

(check-exn #rx"invalid segment length"
           (lambda () (base64-decode "ABCDE")))
