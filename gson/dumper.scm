(define-module (gson dumper)
  #:use-module (ice-9 format)
  #:export (scm->json-string))

(define (scm->json-string val)
  (cond ((number? val) (number->string val))
        ((string? val) (format #f "\"~a\"" val))
        ((symbol? val) (format #f "\"~a\"" (symbol->string val)))
        ((vector? val) (vector->json-array val))
        ((list? val) (list->json-object val))
        ((nil? val) "null")
        ((boolean? val) (if val "true" "false"))))


(define (vector->json-array v)
  (if (eq? (vector-length v) 0) "[]"
      (format #f "[~a]"
              (string-join
               (map scm->json-string
                (vector->list v)) ", "))))


(define (list-entry->json-object-part pair)
  (format #f "\"~a\": ~a" (car pair) (scm->json-string (cdr pair))))

(define (list->json-object ls)
  (if (eq? (length ls) 0) "{}"
      (format #f "{~a}"
              (string-join
               (map list-entry->json-object-part ls) ", "))))
