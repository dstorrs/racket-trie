#lang info

(define collection "trie")
(define deps '("base" "handy" "struct-plus-plus"))
(define build-deps '("scribble-lib" "racket-doc" "rackunit-lib" "sandbox-lib"))
(define scribblings '(("scribblings/trie.scrbl" ())))
(define pkg-desc "Implements the trie data structure")
(define version "0.1")
(define pkg-authors '("David K. Storrs"))
