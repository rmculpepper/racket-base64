#lang scribble/manual
@(require (for-label racket/base racket/contract base64))

@(module net-links racket/base
   (require scribble/manual (for-label net/base64))
   (provide (all-defined-out))
   (define (net-base64-encode) @racket[base64-encode])
   (define (net-base64-decode) @racket[base64-decode])
   (define (net-base64-encode-stream) @racket[base64-encode-stream])
   (define (net-base64-decode-stream) @racket[base64-decode-stream]))
@(require (submod "." net-links))

@title[#:version "1.0"]{Base64 Encoding and Decoding}
@author[@author+email["Ryan Culpepper" "ryanc@racket-lang.org"]]

This library provides support for Base64 encoding and decoding,
similar to @racketmodname[net/base64] but with more options.

@defmodule[base64]

@defproc[(base64-encode [src (or/c bytes? string? input-port?)]
                        [#:endcodes endcodes (or/c #f 'url bytes?) #f]
                        [#:line line (or/c #f exact-positive-integer?) #f]
                        [#:line-sep line-sep bytes? #"\r\n"]
                        [#:pad? pad? boolean? #f])
         bytes?]{

Encodes the bytes of @racket[src] in Base64 (see @cite["RFC 4648"]), returning a
byte string.

If @racket[endcodes] is @racket[#f], the standard characters for the last two
codes are used: @racket[#"+/"]. If @racket[endcodes] is @racket['url], the
alternate endcodes for URLs and filenames are used: @racket[#"-_"] (see
@cite["RFC 4648"], section 5). Otherwise, @racket[endcodes] must be a byte
string containing two bytes, and these are used as the last two codes for the
encoding.

If @racket[line] is a number, it must be a positive multiple of 4. In that case,
the output is broken into segments whose length is @racket[line] bytes (except
possibly the final segment), and @racket[line-sep] is added to the end of each
segment.

If @racket[pad?] is @racket[#t], then the output is padded to a multiple of 4
bytes, using @litchar{=} characters. If @racket[pad?] is @racket[#f], the
padding is omitted.
}

@defproc[(base64-decode [src (or/c bytes? string? input-port?)]
                        [#:endcodes endcodes (or/c #f 'url bytes?) #f]
                        [#:mode mode (or/c 'strict 'whitespace) 'whitespace])
         bytes?]{

Decodes the bytes of @racket[src] as Base64, returning a byte string.

The @racket[endcodes] are interpreted the same as for @racket[base64-encode].

If @racket[mode] is @racket['strict], then the entire input must consist of
Base64-encoded data, @emph{without padding} and without whitespace or other
extra characters. If @racket[mode] is @racket['whitespace], then the input is
divided into segments separated by whitespace and padding @litchar{=}
characters; each segment is decoded independently, and the results are
concatenated. (Note: This is not equivalent to deleting padding and whitespace
characters and then decoding what remains as a single segment.)
}

@defproc[(base64-encode-stream [src (or/c bytes? string? input-port?)]
                               [dest output-port?]
                               [#:endcodes endcodes (or/c #f 'url bytes?) #f]
                               [#:line line (or/c #f exact-positive-integer?) #f]
                               [#:line-sep line-sep bytes? #"\r\n"]
                               [#:pad? pad? boolean? #f])
         void?]{

Like @racket[base64-encode], but writes the output to @racket[dest].
}

@defproc[(base64-decode-stream [src (or/c bytes? string? input-port?)]
                               [#:endcodes endcodes (or/c #f 'url bytes?) #f]
                               [#:mode mode (or/c 'strict 'whitespace) 'whitespace])
         bytes?]{

Like @racket[base64-decode], but writes the output to @racket[dest].
}

@bibliography[
#:tag "base64-bibliography"

@bib-entry[#:key "RFC 4648"
           #:title "RFC 4648: The Base16, Base32, and Base64 Data Encodings"
           #:url "https://tools.ietf.org/html/rfc4648"]

]
