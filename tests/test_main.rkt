#lang racket

(require handy/hash
         handy/list-utils
         handy/test-more
         "../main.rkt"
         )

(module+ main
  (define splitter (compose (curry map ~a) string->list))
  (define h mutable-hash) ; convenient alias
  (define c mcons)
  (define bp build-path)


  (test-suite
   "trie-node functionality"

   (ok (parameter? trie-node-default-data) "trie-node-default-data is a parameter")

   (is (trie-node-default-data)
       (void)
       "got expected initial value for trie-node-default-data")
   (is (trie-node.data (trie-node++ #:terminal? #t))
       (void)
       "trie-node defaults data as expected when not parameterized")

   (parameterize ([trie-node-default-data 'x])
     (is (trie-node.data (trie-node++ #:terminal? #t))
         'x
         "trie-node defaults data as expected when parameterized")))

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
          (trie-node #f (void)
                     (h "u"
                        (trie-node #f (void)
                                   (h "y"
                                      (trie-node #t (void)
                                                 (h)))))))
       "trie-add-item! 'buy' worked")
   (is (trie-add-item! root (splitter "bi"))
       (h "b"
          (trie-node #f (void)
                     (h "u"
                        (trie-node #f (void)
                                   (h "y"
                                      (trie-node #t (void)
                                                 (h))))
                        "i" (trie-node #t (void) (h)))))
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
          (trie-node #t (void)
                     (h (bp "bin")
                        (trie-node #f (void)
                                   (h (bp "bash")
                                      (trie-node #t (void) (h))))
                        (bp "home")
                        (trie-node #f (void)
                                   (h (bp "bob")
                                      (trie-node #f (void)
                                                 (h (bp "grocery-list.txt") (trie-node #t (void) (h))))
                                      (bp "dstorrs")
                                      (trie-node #f (void)
                                                 (h (bp "todo.txt") (trie-node #t (void) (h))
                                                    (bp "taxes")    (trie-node #f (void) (h (bp "2018.pdf")         (trie-node #t (void) (h))))
                                                    (bp "writing")  (trie-node #f (void) (h (bp "patchwork-realms") (trie-node #t (void) (h))
                                                                                            (bp "two-year-emperor") (trie-node #t (void) (h))))))))
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
          (trie-node #f (void)
                     (h "i"
                        (trie-node #f (void)
                                   (h "k"
                                      (trie-node #t (void)
                                                 (h "e"
                                                    (trie-node #t (void) (h)))))))))
       "...and correctly updated the 'terminal?' value for bik"))

  (test-suite
   "trie-unroll"
   (define root (make-trie-root))
   (trie-add-item! root (explode-path "/home/dstorrs/writing/patchwork-realms"))
   (trie-add-item! root (explode-path "/"))
   (trie-add-item! root (explode-path "/home/dstorrs/writing/two-year-emperor"))
   (trie-add-item! root (explode-path "/home/dstorrs/taxes/2018.pdf"))
   (trie-add-item! root (explode-path "/home/bob/grocery-list.txt"))
   (trie-add-item! root (explode-path "/home/dstorrs/todo.txt"))
   (trie-add-item! root (explode-path "/bin/bash"))

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
       "successfully unrolled trie with no args and no data")

   (is (trie-unroll root
                    #:pre     path->string
                    #:combine (compose1 path->string cleanse-path (curryr string-join "/"))
                    #:sort    (curryr sort string<?)
                    #:post    (curry map (curry string-append "X"))
                    )
       (list
        "X/"
        "X/bin/bash"
        "X/home/bob/grocery-list.txt"
        "X/home/dstorrs/taxes/2018.pdf"
        "X/home/dstorrs/todo.txt"
        "X/home/dstorrs/writing/patchwork-realms"
        "X/home/dstorrs/writing/two-year-emperor")
       "successfully unrolled trie with with args but no data")
   )

  (test-suite
   "get subtrie"
   (define root (make-trie-root))
   (trie-add-item! root (explode-path "/home/dstorrs/writing/patchwork-realms"))

   (is (trie-get-subtrie root (explode-path "/home/dstorrs/writing"))
       (h (bp "patchwork-realms")
          (trie-node++ #:terminal? #t))
       "successfully got subtrie")

   (throws
    (thunk
     (trie-get-subtrie root (explode-path "/no/such/key")))
    #px"subtrie not found"
    "correctly fails if there is no such key"
    ))

  (test-suite
   "trie-add-item+data"
   (define (get-fresh)
     (define root (make-trie-root))
     (trie-add-item! root (list "/" "home" "dstorrs" "writing")))

   (is (get-fresh)
       (h "/"
          (trie-node++
           #:kids (h "home"
                     (trie-node++
                      #:kids (h "dstorrs"
                                (trie-node++
                                 #:kids (h "writing"
                                           (trie-node++
                                            #:terminal? #t))))))))
       "get-fresh works as expected")

   (is (trie-add-item+data! (get-fresh)
                            #:combine 'replace
                            (list
                             (cons "/"
                                   (trie-node++ #:terminal? #t
                                                #:data (hash 'dir #t)))))
       (h "/"
          (trie-node++
           #:terminal? #t
           #:data (hash 'dir #t)
           #:kids (h "home"
                     (trie-node++
                      #:kids (h "dstorrs"
                                (trie-node++
                                 #:kids (h "writing"
                                           (trie-node++
                                            #:terminal? #t))))))))
       "trie-add-item+data #:combine 'replace with a shorter version of an existing item will keep the longer item but update the properties of the shorter version")

   (is (trie-add-item+data! (get-fresh)
                            #:combine 'keep
                            (list
                             (cons "/"
                                   (trie-node++ #:terminal? #t
                                                #:data (hash 'dir #t)))))
       (h "/"
          (trie-node++
           #:terminal? #t
           #:data (hash 'dir #t)
           #:kids (h "home"
                     (trie-node++
                      #:kids (h "dstorrs"
                                (trie-node++
                                 #:kids (h "writing"
                                           (trie-node++
                                            #:terminal? #t))))))))
       "trie-add-item+data #:combine 'keep with a shorter version of an existing item will keep the longer item but update the properties of the shorter version")
   
   (is (trie-add-item+data! (get-fresh)
                            #:combine 'keep
                            (list "/"
                                  "home"
                                  (cons "dstorrs"
                                        (trie-node++ #:terminal? #t
                                                     #:data 'user-home))
                                  "writing"
                                  (cons "two-year-emperor"
                                        (trie-node++ #:terminal? #t
                                                     #:data (hash 'type 'novel)))))
       (h "/"
          (trie-node++
           #:kids (h "home"
                     (trie-node++
                      #:kids (h "dstorrs"
                                (trie-node++
                                 #:terminal? #t
                                 #:data 'user-home
                                 #:kids (h "writing"
                                           (trie-node++
                                            #:terminal? #t
                                            #:kids (h "two-year-emperor"
                                                      (trie-node++
                                                       #:terminal? #t
                                                       #:data (hash 'type 'novel)
                                                       #:kids (h)))))))))))
       "trie-add-item+data! worked with mode 'keep")

   (is (trie-add-item+data! (get-fresh)
                            #:combine 'replace
                            (list "/"
                                  "home"
                                  (cons "dstorrs"
                                        (trie-node++ #:terminal? #t
                                                     #:data 'user-home))
                                  "writing"
                                  (cons "two-year-emperor"
                                        (trie-node++ #:terminal? #t
                                                     #:data (hash 'type 'novel)))))
       (h "/"
          (trie-node++
           #:kids (h "home"
                     (trie-node++
                      #:kids (h "dstorrs"
                                (trie-node++
                                 #:terminal? #t
                                 #:data 'user-home
                                 #:kids (h "writing"
                                           (trie-node++
                                            #:terminal? #t
                                            #:kids (h "two-year-emperor"
                                                      (trie-node++
                                                       #:terminal? #t
                                                       #:data (hash 'type 'novel)
                                                       #:kids (h)))))))))))
       "trie-add-item+data! worked with mode 'replace.  This overwrote the terminal? for 'writing'")

   (is (trie-add-item+data! (get-fresh)
                            #:combine 'meld/current
                            (list "/"
                                  "home"
                                  (cons "dstorrs"
                                        (trie-node++ #:terminal? #t
                                                     #:data 'user-home))
                                  (cons "writing"
                                        (trie-node++ #:terminal? #t))
                                  (cons "two-year-emperor"
                                        (trie-node++ #:terminal? #t
                                                     #:data (hash 'type 'novel)))))
       (h "/"
          (trie-node++
           #:kids (h "home"
                     (trie-node++
                      #:kids (h "dstorrs"
                                (trie-node++
                                 #:terminal? #t
                                 #:data 'user-home
                                 #:kids (h "writing"
                                           (trie-node++
                                            #:terminal? #t
                                            #:kids (h "two-year-emperor"
                                                      (trie-node++
                                                       #:terminal? #t
                                                       #:data (hash 'type 'novel)
                                                       #:kids (h)))))))))))
       "trie-add-item+data! worked with mode 'meld/current")

   (is (trie-add-item+data! (trie-add-item+data! (get-fresh)
                                                 #:combine 'replace
                                                 (list
                                                  (cons "/"
                                                        (trie-node++ #:terminal? #t
                                                                     #:data (hash 'dir #t)))
                                                  (cons "home"
                                                        (trie-node++ #:terminal? #t
                                                                     #:data 'home))))
                            #:combine 'meld/new
                            (list (cons "/" (trie-node++ #:data (hash 'root #t)))
                                  (cons "home" (trie-node++  #:data 'users-home))
                                  (cons "dstorrs"
                                        (trie-node++ #:terminal? #t
                                                     #:data 'dks-home))
                                  (cons "writing"
                                        (trie-node++ #:terminal? #t))
                                  (cons "two-year-emperor"
                                        (trie-node++ #:terminal? #t
                                                     #:data (hash 'type 'novel)))))
       (h "/"
          (trie-node++
           #:terminal? #t
           #:data (hash 'dir #t 'root #t)
           #:kids (h "home"
                     (trie-node++
                      #:terminal? #t
                      #:data '(users-home home)
                      #:kids (h "dstorrs"
                                (trie-node++
                                 #:terminal? #t
                                 #:data 'dks-home
                                 #:kids (h "writing"
                                           (trie-node++
                                            #:terminal? #t
                                            #:kids (h "two-year-emperor"
                                                      (trie-node++
                                                       #:terminal? #t
                                                       #:data (hash 'type 'novel)
                                                       #:kids (h)))))))))))
       "trie-add-item+data! worked with mode 'meld/new")
   )

  (test-suite
   "trie-unroll-item+data"

   (parameterize ([trie-node-default-data 'def])
     (define the-trie
       (trie-add-item+data! (make-trie-root)
                            #:combine 'keep
                            (list "/"
                                  (cons "home"
                                        (trie-node++ #:data 'all-users))
                                  (cons "dstorrs"
                                        (trie-node++ #:terminal? #t
                                                     #:data (hash 'type 'dir
                                                                  'subtype 'user-dir)))
                                  "writing"
                                  (cons "two-year-emperor"
                                        (trie-node++ #:terminal? #t
                                                     #:data (cons "hash" "deadbeef"))))))
     (ok (match (trie-unroll the-trie)
           [(list-no-order
             (list "/"
                   "home"
                   "dstorrs"
                   "writing"
                   "two-year-emperor")
             (list "/"
                   "home"
                   "dstorrs"))
            'ok]
           [else #f])
         "using trie-unroll correctly ignores data")

     (define empty-kids? (and/c hash? hash-empty?))
     (define subhash (hash 'type 'dir 'subtype 'user-dir))
     (define correct-subhash? (curry equal? subhash))
     (ok (match (trie-unroll+data the-trie)
           [(list-no-order
             (list (cons "/"
                         (trie-node #f 'def (? empty-kids?)))
                   (cons "home"
                         (trie-node #f 'all-users  (? empty-kids?)))
                   (cons "dstorrs"
                         (trie-node #t (? correct-subhash?) (? empty-kids?))))
             (list (cons "/"
                         (trie-node #f 'def (? empty-kids?)))
                   (cons "home"
                         (trie-node #f 'all-users (? empty-kids?)))
                   (cons "dstorrs"
                         (trie-node #t (? correct-subhash?) (? empty-kids?)))
                   (cons "writing"
                         (trie-node #f 'def (? empty-kids?)))
                   (cons "two-year-emperor"
                         (trie-node #t (cons "hash" "deadbeef") (? empty-kids?)))))
            'success!
            ]
           [_ #f])
         "using trie-unroll+data with no args correctly returns the data")

     (trie-add-item! the-trie (map path->string (explode-path "/foo/bar")))
     (trie-add-item! the-trie (map path->string (explode-path "/jaz/bar")))
     (trie-add-item! the-trie (map path->string (explode-path "/a/b/c")))
     (is  (trie-unroll+data the-trie
                            #:combine (λ (lst)
                                        (define parts (map car lst))
                                        (define fullpath
                                          (path->string
                                           (cleanse-path
                                            (string-join parts "/"))))
                                        (cons fullpath lst))
                            #:sort (curryr sort  string<? #:key car))
          (list
           (list "/a/b/c"
                 (cons "/"   (trie-node #f 'def (h)))
                 (cons "a"   (trie-node #f 'def (h)))
                 (cons "b"   (trie-node #f 'def (h)))
                 (cons "c"   (trie-node #t 'def (h))))
           (list "/foo/bar"
                 (cons "/"   (trie-node #f 'def (h)))
                 (cons "foo" (trie-node #f 'def (h)))
                 (cons "bar" (trie-node #t 'def (h))))
           (list "/home/dstorrs"
                 (cons "/"
                       (trie-node #f 'def (h)))
                 (cons "home"
                       (trie-node #f 'all-users  (h)))
                 (cons "dstorrs"
                       (trie-node #t subhash (h))))
           (list "/home/dstorrs/writing/two-year-emperor"
                 (cons "/"
                       (trie-node #f 'def (h)))
                 (cons "home"
                       (trie-node #f 'all-users (h)))
                 (cons "dstorrs"
                       (trie-node #t subhash (h)))
                 (cons "writing"
                       (trie-node #f 'def (h)))
                 (cons "two-year-emperor"
                       (trie-node #t (cons "hash" "deadbeef") (h))))
           (list "/jaz/bar"
                 (cons "/"   (trie-node #f 'def (h)))
                 (cons "jaz" (trie-node #f 'def (h)))
                 (cons "bar" (trie-node #t 'def (h)))))
          "using trie-unroll+data with args correctly returns the data"))

   (let ([root (make-trie-root)])
     (trie-add-item+data! root
                          #:combine 'replace
                          (list "/"
                                "home"
                                (cons "dstorrs"
                                      (trie-node++ #:terminal? #t
                                                   #:data (hash 'user "David K. Storrs")))))
     (trie-add-item+data! root
                          #:combine hash-meld
                          (list "/"
                                "home"
                                "dstorrs"
                                "writing"
                                (cons "baby-blues"
                                      (trie-node++ #:terminal? #t
                                                   #:data (hash 'type 'novella)))))))

  )

(done-testing)
