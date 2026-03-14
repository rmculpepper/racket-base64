#lang racket/base

(require (prefix-in old: (submod "old-main.rkt" unchecked)))
(require (prefix-in new: (submod "main.rkt" unchecked)))


(require racket/random)

(let ()
  (define bs (crypto-random-bytes 24))
  (write (old:base64-encode bs))
  (newline)
  (write (new:base64-encode bs))
  (newline))


(define bs (crypto-random-bytes (* 1024 1024 20)))

(when #t
  (displayln "original base64-encode")
  (collect-garbage)
  (time (old:base64-encode bs))
  (time (old:base64-encode bs))
  (time (old:base64-encode bs))
  (time (old:base64-encode bs))
  (void)
  )

(when #t
  (displayln "new base64-encode")
  (collect-garbage)
  (time (new:base64-encode bs))
  (time (new:base64-encode bs))
  (time (new:base64-encode bs))
  (time (new:base64-encode bs))
  (void)
  )


