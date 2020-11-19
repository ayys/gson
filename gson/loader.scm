(define-module (gson loader)
  #:use-module (ice-9 textual-ports)
  #:use-module (srfi srfi-43)
  #:use-module (ice-9 peg)
  #:use-module (gson tokenizer)
  #:use-module (oop goops)
  #:export (json-string->scm))

(define-class Hooks ()
  (number #:getter get-number-hook #:init-keyword #:number)
  (nil #:getter get-nil-hook #:init-keyword #:nil)
  (lst #:getter get-list-hook #:init-keyword #:list)
  (object #:getter get-object-hook #:init-keyword #:object)
  (string #:getter get-string-hook #:init-keyword #:string)
  (boolean #:getter get-boolean-hook #:init-keyword #:boolean))

(define* (parse-js-string obj #:key hooks)
  (let* ((string-hook (get-string-hook hooks))
         (string-inner (cadr obj))
         (string-value (if (symbol? string-inner) "" (cadr string-inner))))
    (string-hook string-value)))

(define* (parse-js-number obj #:key hooks)
  (let ((number (cadr obj))
        (hook (get-number-hook hooks)))
    (hook (catch
            'out-of-range
            (lambda () (string->number number))
            (lambda _ number)))))

(define* (parse-js-value-constant obj #:key hooks)
  (let ((nil-hook (get-nil-hook hooks))
        (boolean-hook (get-boolean-hook hooks)))
    (cond
     ((string=? (cadr obj) "false") (boolean-hook #f))
     ((string=? (cadr obj) "null") (nil-hook #nil))
     ((string=? (cadr obj) "true") (boolean-hook #t)))))

(define* (parse-js-object-entry obj #:key hooks)
  (let* ((values (cdr obj))
         (entry-key (car values))
         (entry-value (cadr values)))
    `( ,(parse-js-string entry-key #:hooks hooks) . ,(parse-js-value
                                                      entry-value #:hooks hooks))))

(define* (parse-js-object obj #:key hooks)
  (if (symbol? obj) '()
      (let ((name (car obj))
            (object-hook (get-object-hook hooks))
            (values (cdr obj)))
        (object-hook (map (lambda (val) (parse-js-object-entry val #:hooks hooks)) values)))))


(define* (parse-js-array obj #:key hooks)
  (if (symbol? obj) #()
      (let* ((name (car obj))
             (values (cdr obj))
             (list-hook (get-list-hook hooks))
             (lenvalues (length values)))
        (list-hook (list->vector (map
                                  (lambda (v) (parse-js-value v #:hooks hooks))
                                  values))))))



(define* (parse-js-value  obj #:key hooks)
  (let* ((value (cadr obj))
         (value-type (if (symbol? value) value (caadr obj))))
    (cond
     ((eq? value-type 'js-number) (parse-js-number value #:hooks hooks))
     ((eq? value-type 'js-string) (parse-js-string value #:hooks hooks))
     ((eq? value-type 'js-value-constant) (parse-js-value-constant value #:hooks hooks))
     ((eq? value-type 'js-array) (parse-js-array value #:hooks hooks))
     ((eq? value-type 'js-object) (parse-js-object value #:hooks hooks)))))

(define (i _) _)

(define* (json-string->scm string
                           #:optional #:key
                           (number-hook i)
                           (nil-hook i)
                           (list-hook i)
                           (object-hook i)
                           (string-hook i)
                           (boolean-hook i))
  (let* ((match (match-pattern json string))
         (object (peg:tree match))
         (output-start (peg:start match))
         (output-length (peg:end match))
         (input-length (string-length string))
         (hooks (make Hooks
                  #:number number-hook
                  #:string string-hook
                  #:nil nil-hook
                  #:list list-hook
                  #:object object-hook
                  #:boolean boolean-hook)))
    (if (not (eq? 0 output-start))
        (throw GSON-JSON-INVALID '(0 0)))
    (if (> input-length output-length)
        (begin
          (let* ((number-of-lines (string-count (peg:substring match) #\newline))
                 (column (if (zero? number-of-lines)
                             output-length
                             (remainder output-length number-of-lines))))
            (throw GSON-JSON-INVALID (list number-of-lines column)))))
    (if object
        (parse-js-value object #:hooks hooks)
        (throw GSON-JSON-INVALID '(0 0)))))

