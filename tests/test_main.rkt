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
          (trie-node #f '()
             (h "u"
                (trie-node #f '()
                   (h "y"
                      (trie-node #t '()
                         (h)))))))
       "trie-add-item! 'buy' worked")

   (is (trie-add-item! root (splitter "bi"))
       (h "b"
          (trie-node #f '()
             (h "u"
                (trie-node #f '()
                   (h "y"
                      (trie-node #t '()
                         (h))))
                "i" (trie-node #t '() (h)))))
       "trie-add-item! 'bi' worked; trie now has 'bi' and 'buy'"))

  (test-suite
   "checking emptiness, clearing, getting elements"

   (define root (make-trie-root))
   (ok (trie-is-empty? root) "on creation, root node is empty")
   (trie-add-item! root (splitter "buy"))
   (is-false (trie-is-empty? root) "after adding an item, root node is not empty"))

  (test-suite
   "checking emptiness, clearing, getting elements"

   (define root (make-trie-root))
   (trie-add-item! root (splitter "buy"))
   (is (trie-get-elements root) '("b") "got expected elements from trie containing 'buy'")
   (trie-add-item! root (splitter "foo"))
   (is (sort (trie-get-elements root) string<?)
       '("b" "f")
       "got expected elements for trie containing 'buy' and 'foo'"))



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
          (trie-node #t '()
             (h (bp "bin")
                (trie-node #f '()
                   (h (bp "bash")
                      (trie-node #t '() (h))))
                (bp "home")
                (trie-node #f '()
                   (h (bp "bob")
                      (trie-node #f '()
                         (h (bp "grocery-list.txt") (trie-node #t '() (h))))
                      (bp "dstorrs")
                      (trie-node #f '()
                         (h (bp "todo.txt") (trie-node #t '() (h))
                            (bp "taxes")    (trie-node #f '() (h (bp "2018.pdf")         (trie-node #t '() (h))))
                            (bp "writing")  (trie-node #f '() (h (bp "patchwork-realms") (trie-node #t '() (h))
                                                     (bp "two-year-emperor") (trie-node #t '() (h))))))))
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
          (trie-node #f '()
             (h "i"
                (trie-node #f '()
                   (h "k"
                      (trie-node #t '()
                         (h "e"
                            (trie-node #t '() (h)))))))))
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
       "successfully unrolled trie")

   (ok (match (trie-unroll root)
         [(list-no-order
           (app (curry map path->string) '("/"))
           (app (curry map path->string) '("/" "bin" "bash"))
           (app (curry map path->string) '("/" "home" "bob" "grocery-list.txt"))
           (app (curry map path->string) '("/" "home" "dstorrs" "taxes" "2018.pdf"))
           (app (curry map path->string) '("/" "home" "dstorrs" "todo.txt"))
           (app (curry map path->string) '("/" "home" "dstorrs" "writing" "patchwork-realms"))
           (app (curry map path->string) '("/" "home" "dstorrs" "writing" "two-year-emperor")))
          #t]
         [_ #f])
       "successfully unrolled trie with no args"))
  )
