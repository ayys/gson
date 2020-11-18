;;; This is a JSON parser written in Guile on top of the peg parser
;;; This code is licensed under GPL V3.0

(define-module (gson tokenizer)
  #:use-module (ice-9 peg)
  #:export (json))

;;; Whitespace Terminal
(define-peg-string-patterns
  "js-whitespace < [\t\v\f \r\n\u000C]")

;;; String
(define-peg-string-patterns
  "js-string <-- js-quotes js-string-inner js-quotes
js-string-inner <-- ( js-escaped-chars / js-unescaped-chars )*
js-unescaped-chars <- (! js-invalid-chars .)
js-escaped-chars <- '\\' (js-control-chars / json-hex-control-char)
js-control-chars <- '\"' / '\\' / '/' / 'b' / 'f' / 'n' / 'r' / 't' / '0' / 'l'
js-invalid-chars <- '\"' / '\\' / '\b' / '\n' / '\r' / '\t' / '\0' / '\u000C'
json-hex-control-char <- 'u' [a-fA-F0-9] [a-fA-F0-9] [a-fA-F0-9] [a-fA-F0-9]
js-quotes < '\"'
")

;;; Number
(define-peg-string-patterns
  "js-number <-- js-sign? ('0' js-optional? / js-integer js-optional?)
js-optional <- js-fraction? js-exponent?
js-integer <- js-real-digit js-whole-digit*
js-fraction <- '.' js-whole-digit+
js-exponent <- js-exponent-symbol js-exponent-sign? js-whole-digit+
js-exponent-symbol <- 'E' / 'e'
js-whole-digit <- [0-9]
js-real-digit <- [1-9]
js-exponent-sign <- '+' / '-'
js-sign <- '-'
")

;;; Value, Array, Object
;;; All three are compiled together because they are inter-dependent
(define-peg-string-patterns
  "js-object <-- js-object-open ((js-object-entry js-object-right-side)+ / js-object-close)
js-object-right-side <- (js-object-entry-delimiter & js-object-entry) / js-object-close
js-object-entry <-- js-object-entry-key js-object-entry-separator js-value
js-object-entry-key <- js-whitespace* js-string js-whitespace*
js-object-open < '{'
js-object-close < '}'
js-object-entry-separator < ':'
js-object-entry-delimiter < ','

js-array <-- js-array-open ((js-value  js-array-right-side)+ / js-array-close)
js-array-right-side <- (js-array-element-delimiter & js-value) / js-array-close
js-array-open < '['
js-array-close < ']'
js-array-element-delimiter < ',' js-whitespace*

js-value <-- js-whitespace* js-value-types js-whitespace*
js-value-types <- js-array / js-object / js-value-constant / js-string / js-number
js-value-constant <-- 'false' / 'true' / 'null'
")

;;; To Level Json Object
(define-peg-string-patterns
  "json <-- js-whitespace* json-inner js-whitespace* !.
json-inner <- js-object / js-array / js-number / js-string / js-value-constant
")
