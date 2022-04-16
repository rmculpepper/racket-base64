#lang scribble/manual
@(require (for-label racket/base racket/contract base64)
          scribble/example)

@(define the-eval (make-base-eval))
@(the-eval '(require base64))

@(module net-links racket/base
   (require scribble/manual (for-label net/base64))
   (provide (all-defined-out))
   (define (net-base64-encode) @racket[base64-encode])
   (define (net-base64-decode) @racket[base64-decode])
   (define (net-base64-encode-stream) @racket[base64-encode-stream])
   (define (net-base64-decode-stream) @racket[base64-decode-stream]))
@(require (submod "." net-links))

@title[#:version "1.1"]{Base64 Encoding and Decoding}
@author[@author+email["Ryan Culpepper" "ryanc@racket-lang.org"]]

This library provides support for Base64 encoding and decoding,
similar to @racketmodname[net/base64] but with more options.

@defmodule[base64]

@defproc[(base64-encode [src (or/c bytes? string? input-port?)]
                        [#:endcodes endcodes (or/c #f 'url bytes?) #f]
                        [#:line line (or/c #f exact-positive-integer?) #f]
                        [#:line-end line-end bytes? #"\r\n"]
                        [#:pad? pad? boolean? (and line #t)])
         bytes?]{

Encodes the bytes of @racket[src] in Base64 (see @cite["RFC4648"]), returning a
byte string.

If @racket[endcodes] is @racket[#f], the standard characters for the last two
codes are used: @racket[#"+/"]. If @racket[endcodes] is @racket['url], the
alternate endcodes for URLs and filenames are used: @racket[#"-_"] (see
@cite["RFC4648"], section 5). Otherwise, @racket[endcodes] must be a byte
string containing two bytes, and these are used as the last two codes for the
encoding.

If @racket[line] is a number, it must be a positive multiple of 4. In that case,
the output is broken into segments whose length is @racket[line] bytes (except
possibly the final segment), and @racket[line-end] is added to the end of each
segment.

If @racket[pad?] is @racket[#t], then the output is padded, using @litchar{=}
characters, to a multiple of 4 bytes (disregarding line separators). If
@racket[pad?] is @racket[#f], the padding is omitted.

@examples[#:eval the-eval
(base64-encode #"apple")
(base64-encode #"apple" #:pad? #t)
(base64-encode #"apple" #:line 8)
(base64-encode "orange")
(base64-encode "orange" #:pad? #t)
(base64-encode "orange" #:line 8)
(base64-encode "apples or oranges")
(base64-encode "apples or oranges" #:line 8)
]}

@defproc[(base64-decode [src (or/c bytes? string? input-port?)]
                        [#:endcodes endcodes (or/c #f 'url bytes?) #f]
                        [#:mode mode (or/c 'strict 'whitespace 'padding) 'whitespace])
         bytes?]{

Decodes the bytes of @racket[src] as Base64, returning a byte string.
The @racket[endcodes] are interpreted the same as for @racket[base64-encode].
The @racket[mode] argument determines how padding and whitespace are handled:
@itemlist[

@item{If @racket[mode] is @racket['strict], then the entire input must consist
of Base64-encoded data, @emph{without padding} and without whitespace or other
extra characters.}

@item{If @racket[mode] is @racket['whitespace], then the input is divided into
segments separated by whitespace and padding @litchar{=} characters; each
segment is decoded independently, and the results are concatenated. This
correctly handles the concatenation of padded encodings, but it is not
equivalent to deleting padding and whitespace characters and then decoding what
remains as a single segment.}

@item{If @racket[mode] is @racket['padding], then the input is divided into
segments separated by padding @litchar{=} characters, whitespace within each
segment is discarded, each segment is decoded independently, and the results are
concatenated.}

]

If @racket[src] contains any content that is not valid
Base64-encodings, padding, or whitespace, then @racket[exn:fail:read] is raised,
with the location of the first illegal byte. If the length of a segment (not
including whitespace or padding) is 1 (mod 4), then @racket[exn:fail:read] is
raised, with the location of the segment's end. If the length of a segment is 2
or 3 (mod 4), then the unused bits in the final byte are ignored --- that is, no
error is raised if the unused bits are not all zero.

@examples[#:eval the-eval
(base64-decode "YXBwbGU")
(base64-decode "YXBw = \n=bGU")
;(code:line (base64-decode "YX Bw bG U") (code:comment "different!"))
(code:line (base64-decode "YXB wbGU") (code:comment "different!"))
(code:line (base64-decode "YXB wbGU" #:mode 'padding))
]

@history[#:changed "1.1" @elem{Added the @racket['padding] mode.}]}

@defproc[(base64-encode-stream [src (or/c bytes? string? input-port?)]
                               [dest output-port?]
                               [#:endcodes endcodes (or/c #f 'url bytes?) #f]
                               [#:line line (or/c #f exact-positive-integer?) #f]
                               [#:line-end line-end bytes? #"\r\n"]
                               [#:pad? pad? boolean? (and line #t)])
         void?]{

Like @racket[base64-encode], but writes the output to @racket[dest].
}

@defproc[(base64-decode-stream [src (or/c bytes? string? input-port?)]
                               [#:endcodes endcodes (or/c #f 'url bytes?) #f]
                               [#:mode mode (or/c 'strict 'whitespace 'padding) 'whitespace])
         bytes?]{

Like @racket[base64-decode], but writes the output to @racket[dest].

@history[#:changed "1.1" @elem{Added the @racket['padding] mode.}]}

@bibliography[
#:tag "base64-bibliography"

@bib-entry[#:key "RFC4648"
           #:title "RFC 4648: The Base16, Base32, and Base64 Data Encodings"
           #:url "https://tools.ietf.org/html/rfc4648"]

]

@(close-eval the-eval)
