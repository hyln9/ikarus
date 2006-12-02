
;;;  Copied From CSUG7:

;;; procedure: (pretty-format sym) 
;;; returns: see below 
;;; procedure: (pretty-format sym fmt) 
;;; returns: unspecified

;;; By default, the pretty printer uses a generic algorithm for printing
;;; each form. This procedure is used to override this default and guide
;;; the pretty-printers treatment of specific forms. The symbol sym
;;; names a syntactic form or procedure. With just one argument,
;;; pretty-format returns the current format associated with sym, or #f
;;; if no format is associated with sym.

;;; In the two-argument case, the format fmt is associated with sym for
;;; future invocations of the pretty printer. fmt must be in the
;;; formatting language described below.


;;; <fmt>    ::= (quote symbol)
;;;            | var
;;;            | symbol
;;;            | (read-macro string symbol)
;;;            | (meta)
;;;            | (bracket . fmt-tail)
;;;            | (alt fmt fmt*)
;;;            | fmt-tail
;;; fmt-tail ::= ()
;;;            | (tab fmt ...)
;;;            | (fmt tab ...)
;;;            | (tab fmt . fmt-tail)
;;;            | (fmt ...)
;;;            | (fmt . fmt-tail)
;;;            | (fill tab fmt ...)
;;; tab      ::= int
;;;            | #f

;;; Some of the format forms are used for matching when there are
;;; multiple alternatives, while others are used for matching and
;;; control indentation or printing. A description of each fmt is given
;;; below.


;;; (quote symbol):
;;; This matches only the symbol symbol.

;;; var:
;;; This matches any symbol.

;;; symbol:
;;; This matches any input.

;;; (read-macro string symbol):
;;; This is used for read macros like quote and syntax. It matches any
;;; input of the form (symbol subform). For forms that match, the pretty
;;; printer prints string immediately followed by subform.

;;; (meta):
;;; This is a special case used for the meta keyword (Section 10.7)
;;; which is used as a keyword prefix of another form.

;;; (alt fmt fmt*):
;;; This compares the input against the specified formats and uses the
;;; one that is the closest match. Most often, one of the formats will
;;; match exactly, but in other cases, as when input is malformed or
;;; appears in abstract form in the template of a syntactic abstraction,
;;; none of the formats will match exactly.

;;; (bracket . fmt-tail):
;;; This matches any list-structured input and prints the input enclosed
;;; in square brackets, i.e., [ and ], rather than parentheses.

;;; fmt-tail:
;;; This matches any list-structured input.  Indentation of
;;; list-structured forms is determined via the fmt-tail specifier used
;;; to the last two cases above. A description of each fmt-tail is given
;;; below.


;;; ():
;;; This matches an empty list tail.

;;; (tab fmt ...):
;;; This matches the tail of any proper list; if the tail is nonempty
;;; and the list does not fit entirely on the current line, a line break
;;; is inserted before the first subform of the tail and tab (see below)
;;; determines the amount by which this and all subsequent subforms are
;;; indented.

;;; (fmt tab ...):
;;; This matches the tail of any proper list; if the tail is nonempty
;;; and the list does not fit entirely on the current line, a line break
;;; is inserted after the first subform of the tail and tab (see below)
;;; determines the amount by which all subsequent subforms are indented.

;;; (tab fmt . fmt-tail):
;;; This matches a nonempty tail if the tail of the tail matches
;;; fmt-tail. If the list does not fit entirely on the current line, a
;;; line break is inserted before the first subform of the tail and tab
;;; (see below) determines the amount by which the subform is indented.

;;; (fmt ...):
;;; This matches the tail of any proper list and specified that no line
;;; breaks are to be inserted before or after the current or subsequent
;;; subforms.

;;; (fmt . fmt-tail):
;;; This matches a nonempty tail if the tail of the tail matches
;;; fmt-tail and specifies that no line break is to be inserted before
;;; or after the current subform.

;;; (fill tab fmt ...):
;;; This matches the tail of any proper list and invokes a fill mode in
;;; which the forms are packed with as many as will fit on each line.  A
;;; tab determines the amount by which a list subform is indented. If
;;; tab is a nonnegative exact integer int, the subform is indented int
;;; spaces in from the character position just after the opening
;;; parenthesis or bracket of the parent form. If tab is #f, the
;;; standard indentation is used. The standard indentation can be
;;; determined or changed via the parameter pretty-standard-indent,
;;; which is described later in this section.

;;; In cases where a format is given that doesn't quite match, the
;;; pretty printer tries to use the given format as far as it can. For
;;; example, if a format matches a list-structured form with a specific
;;; number of subforms, but more or fewer subform are given, the pretty
;;; printer will discard or replicate subform formats as necessary.

;;; Here is an example showing the formatting of let might be specified.

;;; (pretty-format 'let
;;;   '(alt (let ([bracket var x] 0 ...) #f e #f e ...)
;;;         (let var ([bracket var x] 0 ...) #f e #f e ...)))

;;; Since let comes in two forms, named and unnamed, two alternatives
;;; are specified. In either case, the bracket fmt is used to enclose
;;; the bindings in square brackets, with all bindings after the first
;;; appearing just below the first (and just after the enclosing opening
;;; parenthesis), if they don't all fit on one line. Each body form is 
;;; indented by the standard indentation.

;;; parameter: pretty-line-length 
;;; parameter: pretty-one-line-limit
;;; The value of each of these parameters must be a positive fixnum.

;;; The parameters pretty-line-length and pretty-one-line-limit control
;;; the output produced by pretty-print. pretty-line-length determines
;;; after which character position (starting from the first) on a line
;;; the pretty printer attempts to cut off output. This is a soft limit
;;; only; if necessary, the pretty-printer will go beyond
;;; pretty-line-length.

;;; pretty-one-line-limit is similar to pretty-line-length, except that
;;; it is relative to the first nonblank position on each line of
;;; output. It is also a soft limit.

;;; parameter: pretty-initial-indent
;;; The value of this parameter must be a nonnegative fixnum.

;;; The parameter pretty-initial-indent is used to tell pretty-print
;;; where on an output line it has been called. If pretty-initial-indent
;;; is zero (the default), pretty-print assumes that the first line of
;;; output it produces will start at the beginning of the line. If set
;;; to a nonzero value n, pretty-print assumes that the first line will
;;; appear at character position n and will adjust its printing of
;;; subsequent lines.

;;; parameter: pretty-standard-indent
;;; The value of this parameter must be a nonnegative fixnum.

;;; This determines the amount by which pretty-print indents
;;; subexpressions of most forms, such as let expressions, from the
;;; form's keyword or first subexpression.

;;; parameter: pretty-maximum-lines
;;; The parameter pretty-maximum-lines controls how many lines
;;; pretty-print prints when it is called. If set to #f (the default),
;;; no limit is imposed; if set to a nonnegative fixnum n, at most n
;;; lines are printed.
