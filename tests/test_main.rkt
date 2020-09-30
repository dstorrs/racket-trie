#lang racket

(require handy)
(require handy/test-more
         handy/hash
         "../main.rkt"
         ;         "../private/nodes.rkt"
         )

(module+ main
  (define splitter (compose (curry map ~a) string->list))
  (define h mutable-hash) ; convenient alias
  (define c mcons)
  (define bp build-path)


  (test-suite
   "make-trie-root and trie?"

   (is (make-trie-root)
       (make-hash)
       "(make-trie-root) worked")
   (is-type (make-trie-root) trie? "root is of type 'trie?'"))


  (test-suite
   "trie-add-item!"

   (define root (make-trie-root))
   (is (trie-add-item! root (splitter "buy"))
       (h "b"
          (c #f
             (h "u"
                (c #f
                   (h "y"
                      (c #t
                         (h)))))))
       "trie-add-item! 'buy' worked")

   (is (trie-add-item! root (splitter "bi"))
       (h "b"
          (c #f
             (h "u"
                (c #f
                   (h "y"
                      (c #t
                         (h))))
                "i" (c #t (h)))))
       "trie-add-item! 'bi' worked; trie now has 'bi' and 'buy'"))

  (test-suite
   "adding non-string items"

   (define root (make-trie-root))
   (trie-add-item! root (explode-path "/home/dstorrs/writing/patchwork-realms"))
   (trie-add-item! root (explode-path "/"))
   (trie-add-item! root (explode-path "/home/dstorrs/writing/two-year-emperor"))
   (trie-add-item! root (explode-path "/home/dstorrs/taxes/2018.pdf"))
   (trie-add-item! root (explode-path "/home/dstorrs/todo.txt"))
   (trie-add-item! root (explode-path "/home/bob/grocery-list.txt"))
   (trie-add-item! root (explode-path "/bin/bash"))

   (is root
       (h (bp "/")
          (c #t
             (h (bp "bin")
                (c #f
                   (h (bp "bash")
                      (c #t (h))))
                (bp "home")
                (c #f
                   (h (bp "bob")
                      (c #f
                         (h (bp "grocery-list.txt") (c #t (h))))
                      (bp "dstorrs")
                      (c #f
                         (h (bp "todo.txt") (c #t (h))
                            (bp "taxes")    (c #f (h (bp "2018.pdf")         (c #t (h))))
                            (bp "writing")  (c #f (h (bp "patchwork-realms") (c #t (h))
                                                     (bp "two-year-emperor") (c #t (h))))))))
                )))
       "successfully inserted paths")
   )

  (test-suite
   "trie-contains? and trie-contains?/update!"

   (define root (make-trie-root))
   (define thing (trie-add-item! root (splitter "bike")))
   (ok (eq? root thing) "root is returned after mutation via trie-add-item!")

   (is-false (trie-contains? thing (splitter "bik"))
             "trie-contains? correctly returns false when a string is not marked terminal")
   (ok (trie-contains?/update! thing (splitter "bik"))
       "trie-contains?/update! thing 'bik' returns true...")
   (is thing
       (h "b"
          (c #f
             (h "i"
                (c #f
                   (h "k"
                      (c #t
                         (h "e"
                            (c #t (h)))))))))
       "...and correctly updated the 'terminal?' value for bik"))

  (test-suite
   "trie-unroll"
   (define root (make-trie-root))
   (trie-add-item! root (explode-path "/home/dstorrs/writing/patchwork-realms"))
   (trie-add-item! root (explode-path "/"))
   (trie-add-item! root (explode-path "/home/dstorrs/writing/two-year-emperor"))
   (trie-add-item! root (explode-path "/home/dstorrs/taxes/2018.pdf"))
   (trie-add-item! root (explode-path "/home/dstorrs/todo.txt"))
   (trie-add-item! root (explode-path "/home/bob/grocery-list.txt"))
   (trie-add-item! root (explode-path "/bin/bash"))

   (is (trie-unroll root
                    #:pre  path->string
                    #:combine  (compose1 path->string cleanse-path (curryr string-join "/"))
                    #:sort (curryr sort string<?))
       (list "/"
             "/bin/bash"
             "/home/bob/grocery-list.txt"
             "/home/dstorrs/taxes/2018.pdf"
             "/home/dstorrs/todo.txt"
             "/home/dstorrs/writing/patchwork-realms"
             "/home/dstorrs/writing/two-year-emperor")
       "successfully unrolled trie"))
  )
