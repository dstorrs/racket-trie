#lang scribble/manual
@(require (for-label racket trie)
          racket/base
          racket/sandbox
          scribble/example)

@title{trie}
@author{David K. Storrs}

@defmodule[trie]

@section{Introduction}

Tries are a data structure for efficiently storing collections of potentially-overlapping pieces of information.  For example, instead of storing the strings `bi', `bike', `bicycle', and `bicycles', it would be better to store `bi', `ke', `cycle', `s' with appropriate markers for how to rebuild the words.

NOTE:  Each entry in a trie is either @italic{terminal} or @italic{non-terminal}.  This matters because it means that a trie can in fact store all elements of an entry yet if the final element is not marked terminal then the trie will answer `no' when asked if it contains the entry.  

Tries are represented as hashes but they should be treated as an opaque struct for purposes of manipulation.


@section{Synopsis}

@(define eval
   (call-with-trusted-sandbox-configuration
    (lambda ()
      (parameterize ([sandbox-output 'string]
                     [sandbox-error-output 'string]
                     [sandbox-memory-limit 50])
        (make-evaluator 'racket)))))

@examples[
 #:eval eval
 #:label #f

(require trie)

  (define root (make-trie-root))
  (trie? root)
  (void (trie-add-item! root (explode-path "/home/dstorrs/writing/patchwork-realms")))
  (void (trie-add-item! root (explode-path "/")))
  (void (trie-add-item! root (explode-path "/home/dstorrs/writing/two-year-emperor")))
  (void (trie-add-item! root (explode-path "/home/dstorrs/taxes/2018.pdf")))
  (void (trie-add-item! root (explode-path "/home/dstorrs/todo.txt")))
  (void (trie-add-item! root (explode-path "/home/bob/grocery-list.txt")))
  (void (trie-add-item! root (explode-path "/bin/bash")))

  (trie-contains? root (explode-path "/"))
  (trie-contains? root (list (build-path "/") (build-path "home") (build-path "dstorrs") (build-path "todo.txt")))
  (trie-contains? root (explode-path "/tmp/test.txt"))

  @#reader scribble/comment-reader
  ;;  The trie contains the node "/" with child "home" with child
  @#reader scribble/comment-reader
  ;;  "dstorrs" with child "todo.txt".  The "dstorrs" node is not terminal
  @#reader scribble/comment-reader
  ;;  because we never added "/home/dstorrs" and therefore '("/", "home", "dstorrs") 
  @#reader scribble/comment-reader
  ;;  is not considered to be in the trie.
  (trie-contains? root (explode-path "/home/dstorrs"))

  @#reader scribble/comment-reader
  ;; We can say "if this thing is there but not terminal, update it to be terminal".
  @#reader scribble/comment-reader
  ;; This is NOT the same as `if it is not there, add it'.
  (trie-contains?/update! root (explode-path "/home/dstorrs"))

  @#reader scribble/comment-reader
  ;; After the previous command "/home/dstorrs" is considered to be in the trie.
  (trie-contains? root (explode-path "/home/dstorrs"))
  (trie-get-elements root)

  (define result
    (trie-unroll root
                 #:pre  path->string
                 #:combine  (compose1 path->string cleanse-path (curryr string-join "/"))
                 #:sort (curryr sort string<?)
                 #:post (λ (l)
    		        (displayln "Trie contains the following paths: ")
	                (pretty-print l))))
  @#reader scribble/comment-reader
  ;; The final result is whatever came back from the `post' function.
  (displayln (~a "Result of the unroll was: " (~v result)))

  @#reader scribble/comment-reader
  ;; If you don't specify keyword args then you get the data back as a list of lists in no particular order.
  (pretty-print (trie-unroll root))
]

@section{API}

@defproc[(trie? [arg any/c]) boolean?]{Returns @racket[#t] if the argument is a trie.  Return @racket[#f] otherwise.}

@defproc[(make-trie-root) trie?]{Create an empty trie.}

@defproc[(trie-is-empty? [arg trie?]) boolean?]{Returns @racket[#t] if @italic{arg} has no items; returns @racket[#f] otherwise.}

@defproc[(clear-trie [arg trie?]) trie?]{Removes all items from the trie, returns an empty trie.}

@defproc[(trie-get-elements [arg trie?]) (listof any/c)]{Returns the top-level elements of the supplied trie.}

@defproc[(trie-add-item! [arg trie?][item (listof any/c)]) trie?]{Adds one item to the trie where the elements of the list are considered to be elements of the item.  The final node is marked as terminal regardless of whether it already existed.}

@defproc[(trie-contains? [arg trie?][item (listof any/c)]) boolean?]{Returns @racket[#t] if the specified item is in the trie and the final element is marked as terminal.  Returns @racket[#f] otherwise.}

@defproc[(trie-contains?/update! [arg trie?][item (listof any/c)][#:update? update? boolean? #t]) boolean?]{Returns @racket[#t] if the specified item is in the trie.  Returns @racket[#f] otherwise.  If @racket[update?] is @racket[#t] AND all elements of @racket[arg] are in the trie AND the final element of @racket[arg] is non-terminal then it will be mutated to be terminal and the function will return @racket[#t].}

@defproc[(trie-unroll [arg trie?]
                      [#:pre     pre       (-> any/c any/c) identity]
                      [#:combine combine   (-> list? any/c) identity]
                      [#:sort    sort-func (-> list? list?) identity]
                      [#:post    post      procedure? identity])
                      any]{
Processes all items in the trie.  By default it simply returns them as a list of items where each item is a list containing the elements of the item. (e.g. @racket['("/" "home")] to represent the string @racket["/home"])  The list of items is in no particular order.

Arguments:

@itemlist[
@item{@racket[pre] will be called on each element as the item is assembled.}
@item{@racket[combine] will be used to transform the item after it is assembled.}
@item{@racket[sort-func] will sort the list returned from mapping @racket[combine] across all the items.}
@item{@racket[post] will transform the list returned by @racket[sort-func].}
]
}

@examples[
 #:eval eval
 #:label #f

  (require trie)

  (define root (trie-add-item! (make-trie-root) (explode-path "/home/dstorrs/writing/patchwork-realms")))
  (void (trie-add-item! root (explode-path "/")))
  (void (trie-add-item! root (explode-path "/home/dstorrs/writing/two-year-emperor")))
  (void (trie-add-item! root (explode-path "/home/dstorrs/taxes/2018.pdf")))
  (void (trie-add-item! root (explode-path "/home/dstorrs/todo.txt")))
  (void (trie-add-item! root (explode-path "/home/bob/grocery-list.txt")))
  (void (trie-add-item! root (explode-path "/bin/bash")))

  @#reader scribble/comment-reader
  ;; By default trie-unroll will give us a list of items where each item is a list and the items are in no particular order
  (pretty-print (trie-unroll root))

  @#reader scribble/comment-reader
  ;; We can preprocess the items into something more useful
  (define result
    (trie-unroll root
                 #:pre  path->string
                 #:combine  (compose1 path->string cleanse-path (curryr string-join "/"))
                 #:sort (curryr sort string<?)
                 #:post (λ (l)
    		        (displayln "Trie contains the following paths: ")
	                (pretty-print l))))
			
  @#reader scribble/comment-reader
  ;; The final result is whatever came back from the `post' function.
  (displayln (~a "Result of the unroll was: " (~v result)))
]
