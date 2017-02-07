; (null-ld? obj)
(define (null-ld? obj)
  (if (not (pair? obj))
      #f
      (eq? (car obj) (cdr obj))))

; (listdiff? obj)
(define (listdiff? obj)
  (cond
    ((null-ld? obj) #t)
    ((not (pair? obj)) #f)
    (else (let ((tail (cdr obj)))
            (let islistdiff? ((curlist (car obj)))
              (cond
                ((null? curlist) #f)
                ((not (pair? curlist)) (eq? curlist tail))
                ((eq? curlist tail) #t)
                (else (islistdiff? (cdr curlist)))))))))

; (cons-ld obj listdiff)
(define (cons-ld obj listdiff)
  (cons (cons obj (car listdiff)) (cdr listdiff)))

; (car-ld listdiff)
(define (car-ld listdiff)
  (cond
    ((null-ld? listdiff) "error")
    ((listdiff? listdiff) (car (car listdiff)))
    (else "error")))

; (cdr-ld listdiff)
(define (cdr-ld listdiff)
  (cond
    ((null-ld? listdiff) "error")
    ((listdiff? listdiff) (cons (cdr (car listdiff)) (cdr listdiff)))
    (else "error")))

; (listdiff obj ...)
(define (listdiff obj . arg)
  (let ((newobj (list obj)))
    (let ((newlist (append (cons obj arg) newobj)))
      (cons newlist newobj))))

; (length-ld listdiff)
(define (length-ld listdiff)
  (if (not (listdiff? listdiff))
      "error"
      (let ((tail (cdr listdiff)))
        (let count-length ((curlist (car listdiff)))
          (cond
            ((eq? curlist tail) 0)
            (else (+ 1 (count-length (cdr curlist)))))))))

; (append-ld listdiff . arg)
(define (append-ld listdiff . arg)
  (if (null? arg)
      listdiff   
      (let generate-objs ((curlist (cons listdiff arg)))
        (cond
          ((null? (cdr curlist)) (car curlist))
          (else (let cons-obj ((curldiff (listdiff->list (car curlist))))
                  (if (null? curldiff)
                      (generate-objs (cdr curlist))
                      (cons-ld (car curldiff) (cons-obj (cdr curldiff))))))))))

; (assq-ld obj alistdiff)
(define (assq-ld obj alistdiff)
  (let ((tail (cdr alistdiff)))
    (let findobj ((curlist (car alistdiff)))
        (cond
          ((null? curlist) #f)
          ((eq? curlist tail) #f)
          ((not (pair? (car curlist))) #f)
          ((eq? obj (car (car curlist))) (car curlist))
          (else (findobj (cdr curlist)))))))

; (list->listdiff list)
(define (list->listdiff list)
  (cond
    ((not (pair? list)) "error")
    ((null? list) (cons list list))
    (else (let ((first-element (cons (car list) null)))
            (cons (append list first-element) first-element)))))

; (listdiff->list listdiff)
(define (listdiff->list listdiff)
  (cond
    ((null-ld? listdiff) null)
    ((not (listdiff? listdiff)) "error")
    (else (let ((tail (cdr listdiff)))
            (let generate-list ((curlist (car listdiff)))
              (if (eq? tail curlist)
                  null
                  (cons (car curlist) (generate-list (cdr curlist)))))))))

; (expr-returning listdiff)
(define (expr-returning listdiff)
  (define (generate-symbols list)
    (if (null? list)
        (quasiquote tail)
        (let ((inner (generate-symbols (cdr list))))
          (quasiquote (cons (quote (unquote (car list))) (unquote inner))))))
  (if (null-ld? listdiff)
      (quasiquote (let ((last-el '(1)))
                    (cons last-el last-el)))
      (let ((list (listdiff->list listdiff)))  
        (quasiquote (let ((tail (quote (unquote (cdr listdiff)))))
                      (cons (unquote (generate-symbols list)) tail))))))

