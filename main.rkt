#lang racket/base

(require racket/contract/base
         racket/contract/region
         racket/function
         racket/list
         racket/match
         racket/set
         handy/hash
         struct-plus-plus)

(provide make-trie-root
         trie?
         trie-is-empty?
         clear-trie
         trie-get-elements
         trie-get-subtrie
         trie-add-item!
         trie-add-item+data!
         trie-contains?
         trie-contains?/update!
         trie-unroll
         trie-unroll+data

         (struct-out trie-node)
         trie-node++
         trie-node.terminal?
         trie-node.data
         trie-node.kids

         trie-node-default-data
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
(define trie-has-key? hash-has-key?)

(define trie-node-default-data (make-parameter (void)))

(struct++ trie-node ([(terminal? #f)                  boolean?]
                     [(data (trie-node-default-data)) any/c]
                     [(kids (make-trie-root))         trie?])
          (#:omit-reflection)
          #:prefab
          #:mutable)

;;----------------------------------------------------------------------

(define/contract (trie-node-has-key? node key)
  (-> trie-node? any/c boolean?)
  (hash-has-key? (trie-node.kids node) key))

(define/contract (update-trie-node-terminal?! the-trie the-key new-val)
  (-> trie? any/c boolean? trie?)
  (define the-node (trie-get-node the-trie the-key))
  (set-trie-node-terminal?! (hash-ref the-trie the-key) new-val)
  the-trie)

;;----------------------------------------------------------------------

(define/contract (set-trie-node! the-trie key new-node)
  (-> trie? any/c trie-node? trie?)
  (hash-set! the-trie key new-node)
  the-trie)

;;----------------------------------------------------------------------

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

;;----------------------------------------------------------------------

(define/contract (trie-get-node t key)
  (-> trie? any/c trie-node?)
  (hash-ref t key))

(define/contract (trie-get-subtrie root all-keys)
  (-> trie? list? trie?)

  (let loop ([current root]
             [keys    all-keys])
    (match keys
      [(list key others ...)
       #:when (trie-has-key? current key)
       (define subtrie (trie-node.kids (trie-get-node current key)))
       (if (null? others)
           subtrie
           (loop subtrie others))]
      [_
       (raise-arguments-error 'trie-get-subtrie
                              "subtrie not found at key specification"
                              "all keys"    keys)])))

;;----------------------------------------------------------------------

(define/contract (trie-add-item! root elements)
  (-> trie? (listof any/c) trie?)
  (let add-next-val ([lst      elements]
                     [the-trie root])
    (match lst
      ['()     root]
      ;
      [(list key)
       #:when (trie-has-key? the-trie key)
       (update-trie-node-terminal?! the-trie key #t)
       root]
      ;
      [(list key others ...)
       #:when (trie-has-key? the-trie key)
       (add-next-val others (trie-node.kids (trie-get-node the-trie key)))]
      ;
      [(list key others ...)
       (define kids (make-trie-root))
       (set-trie-node! the-trie key (trie-node++ #:terminal? (null? others) #:kids kids))
       (add-next-val others kids)])))

;;----------------------------------------------------------------------

;;  Add an item and some associated data to an existing trie.  The
;;  complicated bit is when a new node must be combined with an
;;  existing one.
;;
;;    *) terminal?:  In all cases, the final node is marked terminal if either the
;;    old or new nodes are marked terminal.
;;
;;    *) kids:  In all cases, 'kids' will be updated to contain all elements
;;    of the new item
;;
;;    *) data:  Depends on the value of the #:combine parameter.  Options include:
;;
;;      (-> trie-node? trie-node? trie-node?) : The current and new
;;      trie-node values are passed to the function and the resulting
;;      trie-node is used.
;;
;;      'keep : The existing node's data is preserved, the new node's
;;      data is ignored.
;;
;;      'replace : The new node's data is preserved, the existing
;;      node's data is discarded.
;;
;;      'meld/current : The two data values are combined with current
;;      data having precedence.  See below for details.
;;
;;      'meld/new : The two data values are combined with new data
;;      having precedence.  See below for details.
;;
;;
;;   In the case of 'meld/*, data is combined as follows:
;;
;;     If the values are the same then the result will be that value.
;;
;;     If both values are hashes they will be combined with (hash-meld)
;;     from the 'handy' module.  Entries from the hash that has
;;     precedence will overwrite matching values from the other hash.
;;
;;     If exactly one of the values is unset (i.e., if it is equal? to
;;     the current value of the trie-node-default-data parameter) then
;;     the other will be used.
;;
;;     If both of the values are unset (i.e., if they are equal? to
;;     the current value of the trie-node-default-data parameter) then
;;     the result will be the default value.
;;
;;     In all other cases, the result will be a list containing the
;;     two values, where the first element in the list is the data
;;     that has precedence.

(define/contract (trie-add-item+data! root elements #:combine [combine-method 'meld/new])
  (->* (trie? (listof any/c))
       (#:combine (or/c 'keep 'replace 'meld/current 'meld/new
                        (-> trie-node? trie-node? trie-node?)))
       trie?)

  (match (last elements)
    [(cons _ (and (trie-node #f _ _) node))
     (raise-arguments-error 'trie-add-item+data!
                            "If final element is a trie-node it must have #:terminal? #t"
                            "final element" node)]
    [_ 'ok])

  (define default (trie-node-default-data))
  (define (meld arg-with-precedence arg2)
    (match (list  arg-with-precedence arg2)
      [(list a b)  #:when (equal? a b)       a]
      [(list a b)  #:when (equal? a default) b]
      [(list a b)  #:when (equal? b default) a]
      [(list (? hash? a) (? hash? b))
       (hash-meld arg2  arg-with-precedence)]
      [(list a b) (list arg-with-precedence arg2)]))

  (define is-default? (curry equal? default))
  (define (new-term? . args) (ormap trie-node.terminal? args))
  (define (meld-data . args) (apply meld (map trie-node.data args)))
  (define (meld-kids . args) (apply meld (map trie-node.kids args)))
  
  (define combiner
    (match combine-method
      [(? procedure?)   combine-method]
      ['keep            (λ (current new)
                          (trie-node++ #:terminal? (new-term? current new)
                                       #:data      (if (is-default? (trie-node.data current))
                                                       (trie-node.data new)
                                                       (trie-node.data current))
                                       #:kids      (meld-kids current new)))]
      ['replace         (λ (current new)
                          (trie-node++ #:terminal? (new-term? current new)
                                       #:data      (if (is-default? (trie-node.data new))
                                                       (trie-node.data current)
                                                       (trie-node.data new))
                                       #:kids      (meld-kids new current)))]
      ['meld/current    (λ (current new)
                          (trie-node++ #:terminal? (new-term? current new)
                                       #:data      (meld-data current new)
                                       #:kids      (meld-kids current new)
                                       ))]
      ['meld/new        (λ (current new)
                          (trie-node++ #:terminal? (new-term? current new)
                                       #:data      (meld-data new current)
                                       #:kids      (meld-kids new current)))]))

  (let add-next-val ([the-trie root]
                     [lst      elements])
    (match lst
      ['() root]
      [(list (cons key (? trie-node? node)) others ...)
       #:when (not (trie-has-key? the-trie key))
       (set-trie-node! the-trie key node)
       (add-next-val (trie-node.kids node) others)]
      ;
      [(list (cons key (? trie-node? node)) others ...)
       (define new-node (combiner (trie-get-node the-trie key) node))
       (set-trie-node! the-trie key new-node)
       (add-next-val (trie-node.kids new-node) others)]
      ;
      [(list key others ...)
       #:when (not (trie-has-key? the-trie key))
       (define kids (make-trie-root))
       (set-trie-node! the-trie
                       key
                       (trie-node++ #:terminal? (null? others)
                                    #:kids kids))
       (add-next-val kids others)]
      [(list key others ...)
       #:when (trie-has-key? the-trie key) ; this is unneeded but for self-documentation
       (define node (trie-get-node the-trie key))
       (set-trie-node-terminal?! node (or (trie-node.terminal? node) (null? others)))
       (add-next-val (trie-node.kids node) others)])))


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

  ; 'keys' is the list of keys that leads to a particular element
  (define (unroll the-trie result keys)
    (match the-trie
      [(hash-table)
       result]
      [(hash-table (key (list is-terminal? data kids))) ; @@ Update if implementation changes
       (define next (cons (if (unsupplied-arg? data)
                              (pre key)
                              (cons (pre key) data))
                          keys))
       (unroll kids
               (if is-terminal? (set-add result (combine (reverse next))) result)
               next)]
      [_
       ; There are multiple elements in the trie
       (apply set-union
              (cons result
                    (for/list ([key (in-list (trie-get-elements the-trie))])
                      (define next         (cons (pre key) keys))
                      (define node         (trie-get-node the-trie key))
                      (define is-terminal? (trie-node.terminal?    node))
                      (define kids         (trie-node.kids     node))
                      (unroll kids
                              (if is-terminal?
                                  (set-add result (combine (reverse next)))
                                  result)
                              next))))]))
  (post (sort-func (set->list (unroll the-trie (set) '())))))

;;----------------------------------------------------------------------

(define/contract (trie-unroll+data the-trie
                                   #:pre     [pre       identity]
                                   #:combine [combine   identity]
                                   #:sort    [sort-func identity]
                                   #:post    [post      identity])
  (->* (trie?)
       (#:pre  (-> any/c any/c) #:combine (-> list? any/c)
        #:sort (-> list? list?) #:post procedure?)
       any)

  ; 'elements' is the list of (cons key data) that leads to a particular element
  (define (unroll the-trie result elements)
    (match the-trie
      [(hash-table)  result]
      [(hash-table (key (trie-node is-terminal? data kids)))
       (define element (cons key  (trie-node++ #:terminal? is-terminal?
                                               #:data      data)))
       (define new-elements (cons element elements))
       (unroll kids
               (if is-terminal?
                   (set-add result (combine (reverse new-elements)))
                   result)
               new-elements)]
      [_
       ; There are multiple elements in the trie
       (apply set-union
              (for/list ([(key node) (in-hash the-trie)])
                (unroll (set-trie-node! (make-trie-root) key node) result elements)))]))
  (post (sort-func (set->list (unroll the-trie (set) '())))))
