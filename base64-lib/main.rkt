;; Copyright 2018-2020 Ryan Culpepper

#lang racket/base
(require racket/contract/base)

;; References:
;; - https://tools.ietf.org/html/rfc4648
;; - https://en.wikipedia.org/wiki/Base64

(provide (contract-out
          #:unprotected-submodule unchecked
          [base64-encode
           (->* [input/c]
                [#:endcodes endcodes/c
                 #:line line/c
                 #:line-sep line-sep/c
                 #:pad? boolean?]
                bytes?)]
          [base64-encode-stream
           (->* [input/c output-port?]
                [#:endcodes endcodes/c
                 #:line line/c
                 #:line-sep line-sep/c
                 #:pad? boolean?]
                void?)]
          [base64-decode
           (->* [input/c]
                [#:endcodes endcodes/c
                 #:mode (or/c 'strict 'whitespace)]
                bytes?)]
          [base64-decode-stream
           (->* [input/c output-port?]
                [#:endcodes endcodes/c
                 #:mode (or/c 'strict 'whitespace)]
                void?)]))

(define input/c (or/c bytes? string? input-port?))
(define endcodes/c (or/c '#f 'url (and/c bytes? #rx#"^..$")))
(define (line/c v) (or/c #f (and (exact-positive-integer? v) (zero? (remainder v 4)))))
(define line-sep/c (and/c bytes? #px#"^[[:space:]]*$"))

;; Note: Nat[6-bit] = [0, 63], etc

;; ------------------------------------------------------------
;; Encode/decode between Nat[6-bit] and printable character

(define b64-prefix #"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789")
(define std-endcodes #"+/")
(define url-endcodes #"-_") ;; see https://tools.ietf.org/html/rfc4648#section-5
(define alt-endcodes #"./") ;; used by passlib

;; encode1 : Bytes[2] Nat[6-bit] -> Byte
(define (encode1 endcodes k)
  (cond [(< k 62) (bytes-ref b64-prefix k)]
        [(< k 64) (bytes-ref endcodes (- k 62))]))

;; decode1 : Bytes[2] Byte -> Nat[6-bit]
(define (decode1 endcodes n #:who [who 'decode1])
  (cond [(<= (char->integer #\A) n (char->integer #\Z))
         (+ 0 (- n (char->integer #\A)))]
        [(<= (char->integer #\a) n (char->integer #\z))
         (+ 26 (- n (char->integer #\a)))]
        [(<= (char->integer #\0) n (char->integer #\9))
         (+ 52 (- n (char->integer #\0)))]
        [(= n (bytes-ref endcodes 0)) 62]
        [(= n (bytes-ref endcodes 1)) 63]
        [else (error who "bad base64(~a) code: ~e" endcodes n)]))

;; ------------------------------------------------------------

;; encode : Bytes Bytes[2] Boolean -> Bytes
(define (encode src endcodes pad?)
  (define (code k) (encode1 endcodes k))
  (define srclen (bytes-length src))
  (define outlen
    (+ (* 4 (quotient srclen 3))
       (if pad?
           (case (remainder srclen 3) [(0) 0] [else 4])
           (case (remainder srclen 3) [(0) 0] [(1) 2] [(2) 3]))))
  (define out (make-bytes outlen))
  (for ([srci (in-range 0 srclen 3)]
        [outi (in-range 0 outlen 4)])
    (define n (read-triplet src srci srclen))
    (write-quad out outi outlen n code))
  (when pad?
    (define padlen (case (remainder srclen 3) [(0) 0] [(1) 2] [(2) 1]))
    (for ([i (in-range (- outlen padlen) outlen)])
      (bytes-set! out i (char->integer #\=))))
  out)

;; read-triplet : Bytes Nat Nat -> Nat[6-bit]
(define (read-triplet src srci srclen)
  (define (get srci) (if (< srci srclen) (bytes-ref src srci) 0))
  (+ (arithmetic-shift (get (+ srci 0)) 16)
     (arithmetic-shift (get (+ srci 1)) 8)
     (get (+ srci 2))))

;; write-quad : Bytes Nat Nat Nat[24-bit] (Nat[6-bit] -> Byte) -> Void
(define (write-quad out outi outlen n code)
  (define (put outi v) (when (< outi outlen) (bytes-set! out outi v)))
  (put (+ outi 0) (code (bitwise-bit-field n 18 24)))
  (put (+ outi 1) (code (bitwise-bit-field n 12 18)))
  (put (+ outi 2) (code (bitwise-bit-field n 6  12)))
  (put (+ outi 3) (code (bitwise-bit-field n 0  6))))

;; ------------------------------------------------------------

;; decode : Bytes Bytes[2] -> Bytes
(define (decode src endcodes #:who who)
  (define (dc k) (decode1 endcodes k #:who who))
  (define srclen (bytes-length src))
  (define outlen
    (+ (* 3 (quotient srclen 4))
       (case (remainder srclen 4) [(0) 0] [(2) 1] [(3) 2])))
  (define out (make-bytes outlen))
  ;; Decode main part (full quads)
  (for ([srci (in-range 0 srclen 4)]
        [outi (in-range 0 outlen 3)])
    (define n (read-quad src srci srclen dc))
    (write-triplet out outi outlen n))
  out)

;; read-quad : Bytes Nat Nat (Byte -> Nat[6-bit]) -> Nat[24-bit]
(define (read-quad src srci srclen dc)
  (define (get srci) (if (< srci srclen) (dc (bytes-ref src srci)) 0))
  (+ (arithmetic-shift (get (+ srci 0)) 18)
     (arithmetic-shift (get (+ srci 1)) 12)
     (arithmetic-shift (get (+ srci 2))  6)
     (get (+ srci 3))))

;; write-triplet : Bytes Nat Nat Nat[24-bit] -> Void
(define (write-triplet out outi outlen n)
  (define (put outi v) (when (< outi outlen) (bytes-set! out outi v)))
  (put (+ outi 0) (bitwise-bit-field n 16 24))
  (put (+ outi 1) (bitwise-bit-field n 8  16))
  (put (+ outi 2) (bitwise-bit-field n 0  8)))

;; ----------------------------------------

;; coerce-endcodes : Symbol (U Bytes[2] #f 'url) -> Bytes[2]
(define (coerce-endcodes who v)
  (cond [(eq? v #f) std-endcodes]
        [(eq? v 'url) url-endcodes]
        [(and (bytes? v) (= 2 (bytes-length v))) v]
        [else (error who "endcodes is not a byte string of length 2\n  endcodes: ~e" v)]))

;; Returns a regexp that recognizes contiguous base64-encoded characters.
(define (get-content-rx endcodes)
  (cond [(equal? endcodes std-endcodes)
         #rx#"[A-Za-z0-9+/]+"]
        [(equal? endcodes alt-endcodes)
         #rx#"[A-Za-z0-9./]+"]
        [(equal? endcodes url-endcodes)
         #rx#"[A-Za-z0-9_-]+$"]
        [else
         (byte-regexp (format "(?:[A-Za-z0-9]|~a|~a)+"
                              (regexp-quote (bytes (bytes-ref endcodes 0)))
                              (regexp-quote (bytes (bytes-ref endcodes 1)))))]))

;; ============================================================

;; base64-encode : (U String Bytes InputPort) -> Bytes
(define (base64-encode src
                       #:endcodes [endcodes0 #f]
                       #:line [line #f] #:line-sep [line-sep #"\r\n"]
                       #:pad? [pad? #f] #:who [who 'base64-encode])
  (define endcodes (coerce-endcodes who endcodes0))
  (cond [(bytes? src)
         (base64-encode-bytes who src endcodes line line-sep pad?)]
        [(string? src)
         (base64-encode-bytes who (string->bytes/utf-8 src) endcodes line line-sep pad?)]
        [(input-port? src)
         (define out (open-output-bytes))
         (base64-encode-port who src out endcodes line line-sep pad?)
         (get-output-bytes out)]
        [else (raise-argument-error who "(or/c bytes? string? input-port?)" src)]))

(define (base64-encode-stream in out
                              #:endcodes [endcodes0 #f]
                              #:line [line #f] #:line-sep [line-sep #"\r\n"]
                              #:pad? [pad? #f] #:who [who 'base64-encode-stream])
  (define endcodes (coerce-endcodes who endcodes0))
  (base64-encode-port who in out endcodes line line-sep pad?))

(define (base64-encode-bytes who src endcodes line line-sep pad?)
  (define (do-enc bs) (encode bs endcodes pad?))
  (cond [line
         (define in-line (* 3 (quotient line 4)))
         (define out (open-output-bytes))
         (define srclen (bytes-length src))
         (for ([start (in-range 0 srclen in-line)])
           (define line-src (subbytes src start (min srclen (+ start in-line))))
           (write-bytes (do-enc line-src) out)
           (write-bytes line-sep out))
         (get-output-bytes out)]
        [else (do-enc src)]))

(define (base64-encode-port who src out endcodes line line-sep pad?)
  (define CHUNK 400)
  (define (do-enc bs) (encode bs endcodes pad?))
  (let loop ()
    (define line-src (read-bytes CHUNK src))
    (when (bytes? line-src)
      (write-bytes (do-enc line-src) out)
      (write-bytes line-sep out)
      (loop))))

;; ------------------------------------------------------------

;; base64-decode : (U String Bytes) -> Bytes
(define (base64-decode src
                       #:endcodes [endcodes0 #f]
                       #:mode [mode 'whitespace]
                       #:who [who 'base64-decode])
  (define endcodes (coerce-endcodes who endcodes0))
  (define content-rx (get-content-rx endcodes))
  (cond [(bytes? src)
         (base64-decode-bytes who src endcodes content-rx mode)]
        [(string? src)
         (base64-decode-bytes who (string->bytes/utf-8 src) endcodes content-rx mode)]
        [(input-port? src)
         (define out (open-output-bytes))
         (base64-decode-port who src endcodes content-rx mode)
         (get-output-bytes out)]))

(define (base64-decode-stream in out
                              #:endcodes [endcodes0 #f]
                              #:mode [mode 'whitespace]
                              #:who [who 'base64-decode-stream])
  (define endcodes (coerce-endcodes who endcodes0))
  (define content-rx (get-content-rx endcodes))
  (base64-decode-port who in out endcodes content-rx mode))

(define (base64-decode-bytes who src endcodes content-rx mode)
  (define (do-dec bs) (decode bs endcodes #:who who))
  (define content-ranges (regexp-match-positions* content-rx src))
  (case mode
    [(strict)
     (cond [(zero? (bytes-length src)) #""]
           [(equal? content-ranges (list (cons 0 (bytes-length src))))
            (do-dec src)]
           [else (error who "bad base64 encoding (strict mode)\n  input: ~e" src)])]
    [else
     (case mode
       [(whitespace)
        (for/fold ([start 0]) ([r (in-list content-ranges)])
          (unless (regexp-match? #px#"^[=[:space:]]*$" src start (car r))
            (error who "bad base64 encoding\n  input: ~e" src))
          (cdr r))]
       [else (void)])
     (define out (open-output-bytes))
     (for ([r (in-list content-ranges)])
       (write-bytes (do-dec (subbytes src (car r) (cdr r))) out))
     (get-output-bytes out)]))

(define (base64-decode-port who src out endcodes content-rx mode)
  (define CHUNK 300)
  (let loop ()
    (define chunk (read-bytes CHUNK src))
    (when (bytes? chunk)
      (write-bytes (base64-decode-bytes who chunk endcodes content-rx mode) out)
      (loop))))
