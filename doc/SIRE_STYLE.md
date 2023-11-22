Sire Style Guide
================


Types of Comments
-----------------

The sire codebase uses the common-lisp comments convention:

```commonlisp
;;;; This is a file header comment, giving a high-level overview of a
;;;; file and what it does.

;;; Section Header ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ; ^ anonymous section header

;;; This is a section comment, breaking files into sections and explaining
;;; those sections.

;; This is a documentation comment, usually explaining something about
;; a definition near the comment.

; This is a note, usually interspersed with code or layed out as a
; RHS column.
```



References within Comments
--------------------------

References to identifiers and code snippets within code should be quoted
as {curly strings}.

Code blocks should be indented with four spaces.


Copyright Notices
-----------------

Every file should start with a brief copyright notice, formatted as
a note.

If every file contains a notice followed by a newline, then sire files
can be simply concatenated into a valid contiguous sire input without
any tooling:

    cat sire/??_*.sire | plunder repl sire.seed
