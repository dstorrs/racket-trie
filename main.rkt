#lang racket

(require handy struct-plus-plus)
(provide make-trie-root
         trie?
         trie-is-empty?
         clear-trie
         trie-get-elements
         trie-add-item!
         trie-contains?
         trie-contains?/update!
         trie-unroll

         (struct-out trie-node)
         trie-node++
         trie-node.terminal?
         trie-node.data
         trie-node.kids
         )

;;  A trie is a tree where each node has a value, a flag saying whether
;;  it's a terminal node, and zero or more children.  The normal case
;;  for a trie is compact storage of strings, where each node contains
;;  the next substring in the entry.  For example:
;;
;;  Build a trie containing "buy", "buyer", and "hi"
;;
;;      (define root (make-trie-root))
;;      (trie-add-item! root '("b" "u" "y" "e" "r"))
;;      (trie-add-item! root '("b" "u" "y"))
;;      (trie-add-item! root '("h" "i"))
;;
;;  Gives us this result:
;;
;;      root
;;       -> "b"      #f
;;        -> "u"     #f
;;         -> "y"    #t   ; this is a terminal node, marking the word 'buy'
;;          -> "e"   #f
;;           -> "r"  #t   ; this is a terminal node, marking the word 'buyer'
;;       -> "h"      #f
;;        -> "i"     #t   ; this is a terminal node, marking the word 'hi'
;;
;;
;;  Note that 'buy' was added after 'buyer' had already created the
;;  intermediate nodes.  When 'buy' was added all that actually
;;  happened was that the 'terminal?' flag was flipped to #t.
;;
;;  This module will support having whatever values you want, not just
;;  strings.  For example:
;;
;;      (define root (make-trie-root))
;;      (trie-add-item! root (explode-path "/home/dstorrs/writing/patchwork-realms")])
;;      (trie-add-item! root (explode-path "/home/dstorrs/writing/two-year-emperor")])
;;      (trie-add-item! root (explode-path "/home/dstorrs/taxes/2018.pdf")])
;;      (trie-add-item! root (explode-path "/home/bob/notes/grocery-list.txt"))
;;
;;  Gives us this:
;;
;;    root
;;     -> #<path:/>
;;       -> #<path:home>
;;         -> #<path:bob>
;;           -> #<path:notes>
;;             -> #<path:grocery-list.txt>
;;         -> #<path:dstorrs>
;;          -> #<path:taxes>
;;            -> #<path:2018.pdf>
;;          -> #<path:writing>
;;            -> #<path:patchwork-realms>
;;            -> #<path:two-year-emperor>

;;----------------------------------------------------------------------
;;   PRIVATE
;;----------------------------------------------------------------------
;;
;; These implementation details might change in the future.  For now,
;; a trie containing the words "bi" and "buy" looks like this:
;;
;;  IMPORTANT!  ALL HASHES IN THE FOLLOWING ARE MUTABLE!
;;
;;  (hash "b" (mcons #f
;;                  (hash "i" (mcons #t (make-hash))
;;                        "u" (mcons #f
;;                                   (hash "y" (mcons #t (make-hash))))
;;
;;  The (mcons terminal? kids) is a 'trie-node?'
;;----------------------------------------------------------------------

; Don't use hash/c because it could be really slow for a large trie.
(define trie? (and/c hash? (not/c immutable?)))

(struct++ trie-node ([terminal? boolean?]
                     [(data '()) any/c]
                     [(kids (make-trie-root)) trie?])
          (#:omit-reflection)
          #:prefab
          #:mutable)

(define/contract (trie-node-has-key? node key)
  (-> trie-node? any/c any/c)
  (hash-has-key? (trie-node.kids node) key))

(define trie-has-key? hash-has-key?)

(define/contract (make-trie-root)
  (-> trie?)
  (make-hash))

(define/contract (trie-is-empty? t)
  (-> trie? boolean?)
  (hash-empty? t))

(define/contract (clear-trie t)
  (-> trie? trie?)
  (hash-clear! t))

(define/contract (trie-get-elements t)
  (-> trie? list?)
  (hash-keys t))

(define/contract (trie-get-node t elem)
  (-> trie? any/c trie-node?)
  (hash-ref t elem))


(define/contract (trie-add-item! root elements)
  (-> trie? (listof any/c) trie?)

  (let add-next-val ([lst      elements]
                     [the-trie root])
    (match lst
      ['()     root]
      ;
      [(list key)
       #:when (trie-has-key? the-trie key)
       (define entry (trie-get-node the-trie key))
       (set-trie-node-terminal?! entry #t)
       root]
      ;
      [(list key others ...)
       #:when (trie-has-key? the-trie key)
       (add-next-val others (trie-node.kids (trie-get-node the-trie key)))]
      ;
      [(list key others ...)
       (define kids (make-hash))
       (hash-set! the-trie key (trie-node++ #:terminal? (null? others) #:kids kids))
       (add-next-val others kids)])))


(define/contract (trie-contains?/update! root keys #:update? [update? #t])
  (->* (trie? (listof any/c))
       (#:update? boolean?)
       boolean?)

  ; Check if a sequence of items is in the trie and is marked as a
  ; terminal.  If 'update?' is #t then it will set the 'terminal?'
  ; value to #t before doing the check.  (i.e. it combines checking
  ; for the string and inserting the string)

  (let loop ([current root]
             [keys    keys])
    (match keys
      ['()                   #t] ; a trie always contains the null entry
      [(list key)
       #:when (trie-has-key? current key)
       (define node (trie-get-node current key))
       (when update?
         (set-trie-node-terminal?! node #t))
       (trie-node.terminal? node)]
      [(list key)            #f]
      [(list key others ...)
       #:when (trie-has-key? current key)
       (loop (trie-node.kids (trie-get-node current key)) others)]
      [_ #f])))
(define trie-contains? (curryr trie-contains?/update! #:update? #f))


;;----------------------------------------------------------------------

(define/contract (trie-unroll the-trie
                              #:pre     [pre       identity]
                              #:combine [combine   identity]
                              #:sort    [sort-func identity]
                              #:post    [post      identity])
  (->* (trie?)
       (#:pre  (-> any/c any/c) #:combine (-> list? any/c)
        #:sort (-> list? list?) #:post procedure?)
       any)
  #;
  (h (bp "/")
     (c #t
        (h (bp "home")
           (c #f
              (h (bp "dstorrs")
                 (c #f
                    (h (bp "todo.txt") (c #t (h))
                       (bp "taxes")    (c #f (h (bp "2018.pdf")         (c #t (h))))
                       (bp "writing")  (c #f (h (bp "patchwork-realms") (c #t (h))
                                                (bp "two-year-emperor") (c #t (h)))))))))))
  (define (unroll the-trie result current)
    (match the-trie
      [(hash-table)

       result]
      [(hash-table (element (mcons is-terminal?
                                   (trie-node _ kids _)))) ; @@ Update if implementation changes
       (define next (cons (pre element) current))
       (unroll kids
               (if is-terminal? (set-add result (combine (reverse next))) result)
               next)]
      [_
       ; There are multiple elements in the trie
       (apply set-union
              (cons result
                    (for/list ([element (in-list (trie-get-elements the-trie))])
                      (define next         (cons (pre element) current))
                      (define node         (trie-get-node the-trie element))
                      (define is-terminal? (trie-node.terminal?    node))
                      (define kids         (trie-node.kids         node))
                      (unroll kids
                              (if is-terminal?
                                  (set-add result (combine (reverse next)))
                                  result)
                              next))))]))
  (post (sort-func (set->list (unroll the-trie (set) '())))))
