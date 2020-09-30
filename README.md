trie
===========
The trie data structure, written in Racket.

Typically tries are used for storing text strings one character at a time, e.g. the following stores the words "bike" and "bicyle":

```
  "b"                <- root node
    "i"
      "k"
        "e"
      "c"
        "y"
    "c"
      "l"
        "e"
```

This library accepts data of any type and requires you to divide it yourself.  For example, the following trie stores "bi", "bike", and "bicycles" but not "bicycle".

```
  "bi"       #t  ; terminal node
    "ke"     #t  ; terminal node
    "cycle"  #f  ; non-terminal node
      "s"    #t  ; terminal node
```

Alternatively, here's a trie that stores paths:

```
   "/"                                #t
     "home"                           #f
       "dstorrs"                      #f
         "personal"                   #f
           "novels"                   #t
             "two-year-emperor"       #f
               "chapter-1.html"       #t
               "chapter-2.html"       #t
               "chapter-3.html"       #t
               "chapter-4.html"       #t
         "business"                   #f
           "invoice.txt"              #t
           "taxes"                    #f
             "2018.pdf"               #t
             "2019.pdf"               #t
             "the-year-from-heck.pdf" #t
```

That represents the following paths:

```
   "/"
   "/home/dstorrs/personal/novels"
   "/home/dstorrs/personal/novels/two-year-emperor/chapter-1.html"
   "/home/dstorrs/personal/novels/two-year-emperor/chapter-2.html"
   "/home/dstorrs/personal/novels/two-year-emperor/chapter-3.html"
   "/home/dstorrs/personal/novels/two-year-emperor/chapter-4.html"
   "/home/dstorrs/business/invoice.txt"
   "/home/dstorrs/business/taxes/2018.pdf"
   "/home/dstorrs/business/taxes/2019.pdf"
   "/home/dstorrs/business/taxes/the-year-from-heck.pdf"
```

Elements of an item do not need to be the same type.  For example, instead of repeating 'chapter-N.html' over and over you could use a special entry such as '("chapter-" (1 N) ".html") and write application-level code to reassemble it.  The  trie library provides necessary hooks to simplify that.

See the Racket documentation for details on the API.