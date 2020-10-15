#lang scribble/manual

@(require (for-label racket trie)
	  racket
          racket/sandbox
          scribble/example)

@title{trie}
@author{David K. Storrs}

@defmodule[trie]

@section{Introduction}


Tries are a data structure for efficiently storing collections of potentially-overlapping pieces of information.  For example, instead of storing the strings `bi', `bike', `bicycle', and `bicycles', it would be better to store `bi', `ke', `cycle', `s' with appropriate markers for how to rebuild the words.

A trie is represented as a hash but should be treated as an opaque struct for purposes of manipulation.


@section{Definitions}

An @emph{item} is one piece of data stored in the trie.  Items are represented as lists.  For example, @racket['("/" "home" "dstorrs")].  It is fine to have an item with length 1.

An @emph{element} is one component of an item.  In the item example above, @racket["/"] is an element, @racket["home"] is an element, and @racket["dstorrs"] is an element.

Each element is either @italic{terminal} or @italic{non-terminal}.  Terminal elements represent the end of an item.  This matters because it means that a trie might in fact store all elements of an entry yet if the final element is not marked terminal then the trie will answer `no' when asked if it contains the entry.  For example, if the trie contains the item @racket['("/" "home" "dstorrs" "writing")] it still does not contain the item @racket['("/" "home" "dstorrs")] unless that item has been added separately.

A @emph{trie-node} is the information associated with an element in the trie.  Trie-nodes track three pieces of information:  whether their element is terminal, whatever user-supplied data might be attached, and a trie containing the children of this node.  In the @racket['("/" "home" "dstorrs")] example, @racket["dstorrs"] is a child of @racket["home"] is a child of @racket["/"].  The name of the field that tracks children is `kids' because it's shorter.

The data field of a trie-node is considered @emph{not set} if it is @racket[equal?] to the current value of the @racket[trie-node-default-data] parameter.

Trie-nodes are structs created with the @racketmodname[struct-plus-plus] module.  They therefore have keyword constructors with default values and contracts that validate the arguments, as well as dotted accessors (e.g. @racket[trie-node.data]) in addition to the standard ones (e.g. @racket[trie-node-data]).  All of the normal struct function exist, but you are encouraged to work with the new versions.

When using @racket[trie-add-item+data!] it will sometimes be the case that the item specifies information for a node that already exists.  In this case the existing node is called @racketid[current] and the one that is being added is @racketid[current].

Whenever it is necessary to combine two nodes, either @racketid[current] or @racketid[new] will have @emph{priority}, meaning that its values will win in the case of conflict.  An example from general Racket code is that when doing @racket[hash-union] the hash that comes last in the list has priority because its fields will override those of earlier hashes.

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
  (trie-contains? root (explode-path "/home/dstorrs/todo.txt"))
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

@section{Tries and Data}

Often, you want to store your dataset compactly but decorate parts of it with arbitrary data.  For example, you might want to store the fact that "/home/dstorrs" is the user directory for David K. Storrs, that  "/home/dstorrs/writing/two-year-emperor" is a novel and "/home/dstorrs/writing/baby-blues" is a novella.  Additionally, you might want to add several overlapping items at the same time. @racket[trie-add-item+data!] allows this.

@examples[
 #:eval eval
 #:label #f

     (require trie)

     (define the-trie (make-trie-root))
     (trie-add-item+data! the-trie
			  (list "/"
                            (cons "home"
			    	  (trie-node++ #:data 'all-users))
                            (cons "dstorrs"
                                  (trie-node++ #:terminal? #t
                                               #:data (hash 'user "David K. Storrs")))
                            "writing"
                            (cons "two-year-emperor"
                                  (trie-node++ #:terminal? #t
                                               #:data (cons 'type 'novel)))))
     (trie-add-item+data! the-trie
     			  #:combine 'replace
                          (list "/"
                                (cons "home"
                                      (trie-node++ #:data 'all-users))
                                (cons "dstorrs"
                                      (trie-node++ #:terminal? #t
                                                   #:data (hash 'user "David K. Storrs")))
                                "writing"
                                (cons "two-year-emperor"
                                      (trie-node++ #:terminal? #t
                                                   #:data (cons 'type 'novel)))))
]

@section{API}

@defparam[trie-node-default-data default any/c
          #:value (void)]{A parameter that defines what the default value for data entries in a trie should be.  This value will be used if no value is specified.}

@defstruct*[trie-node ([terminal? boolean?] [data any/c][kids trie?])]{A struct for describing the values of a trie.  Each element of an item is associated with one element of one item.}

@defproc[(trie-node++ [#:terminal? terminal? boolean? #f][#:data data any/c (trie-node-default-data)][#:kids kids trie? (make-trie-root)]) trie-node?]{Create a trie-node struct using keyword constructor with contract-checking and default values.}

@defproc*[([(trie-node.terminal? [node trie-node?]) boolean?]
           [(trie-node.data [node trie-node?]) any/c]
           [(trie-node.kids [node trie-node?]) trie?])]{Dotted accessors for a trie-node struct.}

@defproc[(trie? [arg any/c]) boolean?]{Returns @racket[#t] if the argument is a trie.  Return @racket[#f] otherwise.}

@defproc[(make-trie-root) trie?]{Create an empty trie.}

@defproc[(trie-is-empty? [arg trie?]) boolean?]{Returns @racket[#t] if @italic{arg} has no items; returns @racket[#f] otherwise.}

@defproc[(clear-trie! [arg trie?]) trie?]{Mutationally removes all items from the trie, returns the empty trie.}

@defproc[(trie-get-elements [arg trie?]) (listof any/c)]{Returns the top-level elements of the supplied trie.}


@defproc[(trie-add-item! [arg trie?][item (listof any/c)]) trie?]{Adds one item to the trie.  The final element is marked as terminal.}

@defproc[(trie-contains? [arg trie?][item (listof any/c)]) boolean?]{Returns @racket[#t] if the specified item is in the trie and the final element is marked as terminal.  Returns @racket[#f] otherwise.}

@defproc[(trie-contains?/update! [arg trie?][item (listof any/c)][#:update? update? boolean? #t]) boolean?]{Returns @racket[#t] if the specified item is in the trie.  If @racket[update?] is @racket[#t] AND all elements of @racket[arg] are in the trie AND the final element of @racket[arg] is non-terminal then it will be mutated to be terminal and the function will return @racket[#t].  Returns @racket[#f] otherwise.}

@defproc[(trie-add-item+data! [arg trie?][the-item (listof any/c)][#:combine combine-method (or/c 'keep 'replace 'meld/current 'meld/new (-> trie-node? trie-node? trie-node?)) 'meld/new]) trie?]{Adds one item to the trie, optionally including data with the elements.  In the simple case this is equivalent to @racket[trie-add-item!].  The behavior is only different when a value in the list is a cons pair of the form `(cons any/c trie-node?)'.  In this case the car will be interpreted as the element of the trie and the cdr will be information intended for the value of that entry.

If the last value in arg specifies a trie-node then that trie-node must have its terminal? value set to @racket[#t] or an error will be thrown.

The important part is to understand what happens when a trie-node is given for an element that already exists in the trie--an example would be:

  @#reader scribble/comment-reader

   (racketblock

    (define root (make-trie-root))

    ;;  Add an item but do not put any data with the elements.  This is effectively
    ;;  the same as using 'trie-add-item!'
    (trie-add-item+data! root '("/" "home" "dstorrs"))

    ;;  The trie contains:  '(("/" "home" "dstorrs"))
    
    ;; Add another item that overlaps with the previous one but does not specify extra information
    (trie-add-item+data! root '("/" "home" "dstorrs" "writing"))

    ;;  The trie contains:  '(("/" "home" "dstorrs")
    ;;                        ("/" "home" "dstorrs" "writing"))
    
    ;; Add a third item that overlaps and has associated data. We want to annotate
    ;; the fact that "/home" is a directory, not a file.
    (trie-add-item+data! root (list "/" (cons "home" (trie-node++ #:terminal? #t #:data 'dir))))
    
    ;;  The trie contains:  '(("/" "home" "dstorrs")
    ;;                        ("/" "home" "dstorrs" "writing")
    ;;                        ("/" "home"))

    ;; In that last call we provided a trie-node of data that we want
    ;; added to the "home" element.  That element already exists and
    ;; has a trie-node.  Therefore, we must merge current and new.
   )


When merging nodes:

@itemlist[
@item{If the @racket[combine-method] argument is a procedure then the @racketid[current] and @racketid[new] nodes will be passed to it and the resulting trie-node will be used.}
]

If the @racket[combine-method] is not a procedure, then we first determine priority based on the value of  @racket[combine-method]:

@itemlist[
@item{@racket['keep] : @racketid[current] has priority.}
@item{@racket['meld/current] : @racketid[current] has priority.}
@item{@racket['replace] :  @racketid[new] has priority.}
@item{@racket['meld/new] : @racketid[new] has priority.}
]

After making this determination we update the fields of the existing node as follows:

@itemlist[
@item{@racket[terminal?]:
  @itemlist[
    @item{Set to @racket[#t] if either of @racketid[current] or @racketid[new] is marked as terminal.  This ensures that we don't accidentally remove an item from the trie by setting its final element to be non-terminal.}
]
}
@item{@racket[kids]:
  @itemlist[
    @item{Take the union of the kids for the two nodes.  The node with priority overrides conflicting elements in the other.  WARNING:  This can cause items to be deleted from the trie if they are overwritten by a new entry.  In general, it's better to leave the kids field blank when using @racket[trie-add-item+data!].}
  ]
}
@item{@racket[data]:
  @itemlist[
    @item{Under the @racket['keep] or @racket['replace] methods, use the value from the node that has priority.}
    @item{Under the @racket['meld/current] or @racket['meld/new] methods, combine the items as follows:
      @itemlist[
        @item{If the data fields in both nodes are the same, do nothing.}
        @item{If neither node has its data field set, do nothing. (This is a special case of the prior rule.)}
        @item{If exactly one of the nodes has its data field set, use that one.}
        @item{If both values are hashes, take their union, allowing the node that has priority to overwrite conflicting entries from the other node.}
        @item{Otherwise, return a list containing both values where the first element is from the node that has priority.}
      ]
    }
  ]
}
]

One edge case:  `The data field is not set' simply means that its value is @racket[equal?] to the current value of the @racket[trie-node-default-data] parameter.  If the value of @racket[trie-node-default-data] changed after the trie-node was created then the status of the node as un/set may be misidentified.  (The parameter's value might have changed either because it was assigned to or because you entered/left a @racket[parameterize] block.)
}

@defproc[(trie-unroll+data [the-trie trie?]
                           [#:pre     pre       (-> (cons/c any/c trie-node?) any/c) identity]
                           [#:combine combine   (-> list? any/c) identity]
                           [#:sort    sort-func (-> list? list?) identity]
                           [#:post    post      procedure? identity])
			   any]{Processes a trie, taking into account the data for each element.  By default it will return a list of all items in the trie, where each item is represented as a list of cons pairs where the car is the element and the cdr is a trie-node containing the @racket[terminal?] and @racket[data] values for that element (but not the @racket[kids] field).  The items will be in no particular order.

The @racket[pre] function is called on each element as it is processed.  It is passed a cons pair containing the element and a newly-created trie node that includes the @racket[data] and @racket[terminal?] values for that node.

The @racket[combine] function is called on each item once it has been assembled.

The @racket[sort-func] function is called on the list of items.

The @racket[post] function is called on the sorted list of items.

The final result of @racket[trie-unroll+data] is whatever comes back from the @racket[post] function.
}

@examples[
 #:eval eval
 #:label #f

    (require trie)
  
    (define root (make-trie-root))
  
    (trie-add-item+data! root (list (build-path "/")
    			      	    (build-path "home")
				    (cons (build-path "dstorrs")
				    	  (trie-node++ #:terminal? #t
					  	       #:data (hash 'personal-name
						       	      	    "David K. Storrs")))))
    (trie-add-item+data! root (explode-path "/home/dstorrs/writing"))
    (trie-add-item+data! root (explode-path "/home"))

    (pretty-print (trie-unroll+data root))

    (pretty-print (trie-unroll+data root #:pre car))

    (pretty-print (trie-unroll+data root #:pre (compose1 path->string car)))

    (pretty-print (trie-unroll+data root
    		  		    #:pre (λ (e) (cons (path->string (car e)) (cdr e)))
				    #:combine (λ (the-item)
       		  		                (cons (path->string
						        (cleanse-path
							  (string-join (map car the-item)
							               "/")))
						      (filter-not (curry equal? (trie-node-default-data))
						        (map (compose1 trie-node.data cdr)
							  the-item))))
				    #:sort (curryr sort string<? #:key car)
				    #:post (λ (items)
				    	     (define (write-to-database x) 'TODO)
					     (write-to-database items)
					     items)))]

Obviously, it's perfectly possible to use the basic form of @racket[trie-unroll+data] and do all of the processing separately after the fact.  The advantage to doing it through the keywords is that everything is part of the same S-expression and therefore both closer to the data and less likely to get forgotten if one piece of the code is refactored elsewhere.
