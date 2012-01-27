#lang scribble/manual
@(require planet/scribble
          racket/sandbox
          scribble/eval
          (for-label "main.rkt"))


@(define my-evaluator
   (call-with-trusted-sandbox-configuration 
    (lambda ()
      (parameterize ([sandbox-output 'string]
                     [sandbox-error-output 'string])
        (make-evaluator 'racket
                        #:requires
                        (list))))))

@title{Suffix trees with Ukkonen's algorithm}

@author+email["Danny Yoo" "dyoo@hashcollision.org"]

This is an implementation of suffix trees and their linear-time
construction with the Ukkonen algorithm.  This implementation is based
on notes from Gusfield, "Algorithms on Strings, Trees, and Sequences".


@section{Example}

Let's rush into a minimal example:

@interaction[#:eval my-evaluator 
(require (planet dyoo/suffixtree))
(define tree (make-tree))
(tree-add! tree (string->label "00010010$"))
(define root (tree-root tree))
(node-children root)
(label->string (node-up-label (car (node-children root))))
]


@section{Introduction}

Suffix trees encode all the nonempty suffixes of a string.  For
example, the string "0100101$" corresponds to the following suffix
tree.

@verbatim|{
   root
    |
    V
    +--- $
    |
    +--- 1 --- $
    |    |
    |    +---- 0 --- 0101$
    |          |
    |          +---- 1$
    |
    +--- 0 --- 0101$
         |
         +---- 1 ---- $
               |
               +----- 0 --- 1$
                      |
                      +---- 0101$
}|

Every path from the root to any leaf spells out a suffix of the string
@racket["0100101$"], and every suffix is accounted for.  This in
itself might not sound too sexy, but by preprocessing a string as a
suffix tree, we can then do some amazing things.

For example, we can see if a substring is present in a suffix tree in
time bounded by the length of the substring by following characters
starting from the root.  Suffix trees also allow us to find the
longest common substring between strings in linear time.  Dan
Gusfield's book "Algorithms on Strings, Trees, and Sequences" sings
praises about suffix trees, and deservedly so.

Constructing a suffix tree can be done in linear-time; the algorithm
used here is Ukkonen's algorithm, since it's one of the simplest to
code.  That being said, the algorithm is not quite simple; for more
information on the construction algorithm, see the References section
below.



@section{API}
@defmodule/this-package[main]

The API consists of the main suffix tree algorithm, and auxillary
utilities and applications.

The main structures are trees, nodes, and labels.

@subsection{Trees}

A suffix tree consists of a root.  This implementation allows multiple
labels to be added to the tree.

> (make-tree)

    Constructs an empty suffix tree with a single root node.  

> (tree? datum)

    Returns #t if datum is a suffix tree.

> (tree-root tree)

    Selects the root node from a tree.

> (tree-add! tree label)

    Adds a label and all of its nonempty suffixes to the tree.

> (tree-walk tree label succeed-f fail-f)

    Starting from the tree-root, walks along a path whose path
    label exactly matches the input label.

    If the label matched completely, calls succeed-f with the
    position where the matching had succeeded.

        succeed-f node up-label-offset -> A

    If the label mismatched, calls fail-f with the tree position where
    the matching had failed.

        fail-f: node up-label-offset input-label-offset -> B

    The return value from tree-walk will either be A or B.


> (tree-contains? tree label)

    Returns #t if a path exists starting from the tree-root of the
    tree whose path-label exactly matches label.

    tree-contains? is an application of tree-walk:

        (define (tree-contains? tree label)
          (tree-walk tree label 
                     (lambda (node up-label-offset) #t)
                     (lambda (node up-label-offset input-label-offset) #f)))



@subsection{Nodes}

Nodes form the structure of the suffix tree, and link up children
nodes as well.  Every internal node I of a suffix tree will also have
a suffix-node whose path-label is the immediate suffix of node I.

> (node-up-label node)

    Selects the label of the edge that connects this node to its
    parent.  The up-label of the root node is empty.

> (node-parent node)

    Selects the parent of this node.  The root of a suffix tree has no
    parent, so (node-parent (tree-root tree)) returns #f.

> (node-suffix-link node)

    Selects the suffix node of this node.  If the suffix-link is not
    set, returns #f.

> (node-find-child node label-element)

    Selects the child whose up-label starts with the label-element.  If no
    such child can be found, returns #f.

> (node-children node)

    Selects the list of children nodes to this node.  If the node is a
    leaf, returns ().


@subsection{Labels}

Labels represent an immutable sequence of label-elements.
Label-elements can be anything that compare with equal?, but the most
common label-elements will be characters.  Labels can be sublabeled
with efficiency.


> (string->label string)

    Constructs a label from a string.  Each of the label-elements of
    this label will be a character.

> (string->label/with-sentinel string)

    Constructs a label from a string with a trailing sentinel
    character to guarantee that all suffixes can be explicitely
    represented in a suffix tree.  (See the Caveats section below for
    details.)

    Note that label->string can't be directly used on a label with a
    sentinel.

> (label->string label)

    Constructs a string from a label, assuming that all label-elements
    of the string are characters.

> (vector->label vector)

    Constructs a label from a vector.

> (vector->label/with-sentinel vector)

    Constructs a label from a vector with a trailing sentinel character.

> (label->vector label)

    Selects a vector of the label-elements that represent the label.
    This vector is immutable.

> (sublabel label left-offset [right-offset])

    Derives a new sliced label from the parent label, along the
    half-open interval [left-offset, right-offset).

    If right-offset is omitted, it defaults to (label-length label).

    (<= left-offset right-offset) should be true.

> (label-ref label offset)

    Returns the label's label-element at that offset.

> (label-length label)

    Returns the length of a label.

> (label-equal? label-1 label-2)

    Warning: two labels may have equal content, but come from
    different sources.

> (label-source-eq? label-1 label-2)

    Returns #t if both labels share a common derivation from
    sublabeling.

> (label-source-id label)

    Returns an numeric identifier for this label.

    (label-source-eq? label-1 label-2) 

         logically implies:

    (= (label-source-id label-1) (label-source-id label-2))



@subsection{Other utilities}

This module provides some example applications of suffix trees.

> (longest-common-substring string-1 string-2)

    Returns the longest common substring between the two strings.

> (longest-common-sublabel label-1 label-2)

    Returns the longest sublabel that's shared between label-1 and label-2.

> (path-label node)

    Returns a new label that represents the path from the root to this
    node.



@section{Caveats}

The code in tree-add! assumes that the construction of the full
suffix tree on its input string is possible.  Certain strings don't
have an full explicit suffix tree, such as "foo".

@verbatim|{
    +-- "foo"
    |
    +-- "oo"
}|

In this case, when we try to construct a suffix tree out of "foo", we
have an implicit suffix tree, where not every leaf corresponds to a
suffix of the input string.  The suffix "o" is implicit in this tree.


In order to guarantee that all suffixes will have a place in the
suffix tree, we'll often add a sentinel character at the end the
string to make sure all suffixes have a unique path in the suffix
tree.  For example, assuming that we use "$" as our sentinel:

@verbatim|{
    +-- "foo$"
    |
    +-- "o" -- "o$"
    |    |
    |    +---- "$"
    |
    +-- "$"
}|   

The API has the function string->label/with-sentinel to automatically add
a unique sentinel character at the end of a string.

@racketblock[
(let ([tree (make-tree)]
      [label (string->label/with-sentinel "foo")])
  (tree-add! label))
]

so be sure to use this if you need to ensure the representation of all
suffixes in the suffix tree.



@section{References}

Dan Gusfield.  Algorithms on Strings, Trees, and Sequences: Computer
Science and Computational Biology.  Cambridge University Press, New
York, NY, 1997.

Lloyd Allison.  Suffix Trees.
@url{http://www.allisons.org/ll/AlgDS/Tree/Suffix/}

Mark Nelson.  Fast String Searching With Suffix Trees.  Dr. Dobb's
Journal, August, 1996.
@url{http://www.dogma.net/markn/articles/suffixt/suffixt.htm}

Mummer: Ultra-fast alignment of large-scale DNA and protein sequences.
@url{http://mummer.sourceforge.net/}
