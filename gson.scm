(define-module (gson)
  #:use-module (gson loader)
  #:use-module (gson dumper)
  #:use-module (gson tokenizer))

(define-syntax re-export-modules
  (syntax-rules ()
    ((_ (mod ...) ...)
     (begin
       (module-use! (module-public-interface (current-module))
                    (resolve-interface '(mod ...)))
       ...))))

(re-export-modules (gson loader)
                   (gson dumper))
