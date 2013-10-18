(import (scheme base)
        (scheme write)
        (scheme read)
        (scheme cxr)
        (srfi 1))

;; The alphabet type

(define-record-type alphabet
  (make-alphabet-0 empty symbols)
  alphabet?
  (empty empty-symbol)
  (symbols non-empty-symbols))

(define (make-alphabet empty single . rest-of-symbols)
  (make-alphabet-0 empty
                   (cons single rest-of-symbols)))

(define (alphabet-contains? a s)
  (or (char=? (empty-symbol a) s)
      (not (null? (filter (lambda (s-in-a)
                            (char=? s s-in-a))
                          (non-empty-symbols a))))))

(define (all-symbols alphabet)

  (cons (empty-symbol alphabet)
        (non-empty-symbols alphabet)))

(define (symbol-encoding alphabet symbol)

  (let loop ((i 0)
             (symbols (all-symbols alphabet)))

    (cond ((null? symbols) #f)
          ((char=? (car symbols) symbol) i)
          (else (loop (+ i 1)
                      (cdr symbols))))))

(define (symbol-encoded-by alphabet encoding)

  (let loop ((i 0)
             (symbols (all-symbols alphabet)))

    (cond ((null? symbols) #f)
          ((= i encoding) (car symbols))
          (else (loop (+ i 1) (cdr symbols))))))

;; The tape type

(define-record-type tape
  (make-tape-0 left right alphabet)
  tape?
  (left tape-left)
  (right tape-right)
  (alphabet tape-alphabet))

(define (empty-tape alphabet)
  (make-tape-0 (list)
               (list)
               alphabet))

(define (empty-symbol-encoding alphabet)

  (symbol-encoding alphabet (empty-symbol alphabet)))

(define (move-left tape)

  (let ((left (tape-left tape))
        (right (tape-right tape))
        (alphabet (tape-alphabet tape)))

    (make-tape-0 (if (null? left)
                     left
                     (cdr left))
                 (cons (if (null? left)
                           (empty-symbol-encoding alphabet)
                           (car left))
                       right)
                 alphabet)))

(define (move-right tape)

  (let ((left (tape-left tape))
        (right (tape-right tape))
        (alphabet (tape-alphabet tape)))

    (make-tape-0 (cons (if (null? right)
                           (empty-symbol-encoding alphabet)
                           (car right))
                       left)
                 (if (null? right)
                     right
                     (cdr right))
                 alphabet)))

(define (read-from-tape tape)

  (let ((right (tape-right tape))
        (alphabet (tape-alphabet tape)))

    (if (null? right)
        (symbol-encoding alphabet (empty-symbol alphabet))
        (car right))))

(define (write-to-tape tape symbol)

  (let ((left (tape-left tape))
        (right (tape-right tape))
        (alphabet (tape-alphabet tape)))

    (when (not (alphabet-contains? alphabet symbol))
       (error "write-to-tape -- symbol is not in alphabet"
              alphabet
              symbol))

    (make-tape-0 left
                 (cons (symbol-encoding alphabet symbol)
                       (if (null? right)
                           right
                           (cdr right)))
                 alphabet)))

(define (make-tape alphabet string)

  (let ((symbols (string->list string)))
    (let loop ((tape (empty-tape alphabet))
               (symbols (reverse symbols)))

      (if (null? symbols)
          (move-right tape)
          (loop (move-left (write-to-tape tape (car symbols)))
                (cdr symbols))))))

(define (shift-tape tape shift)

  (let ((shift (abs shift))
        (move (if (< 0 shift)
                  move-right
                  move-left)))

    (let loop ((i 0)
               (tape tape))

      (if (< i shift)
          (loop (+ i 1)
                (move tape))
          tape))))

;; Transition type

(define-record-type transition
  (make-transition write move next-state)
  transition?
  (write transition-write)
  (move transition-move)
  (next-state transition-next-state))

(define (apply-transition transition tape)

  (let ((move (transition-move transition))
        (write (transition-write transition)))

    (move (write-to-tape tape write))))

;; Machine state type

(define-record-type machine-state
  (make-machine-state-0 name transitions)
  machine-state?
  (name machine-state-name)
  (transitions machine-state-transitions))

(define (end-state? state)
  (not (machine-state-transitions state)))

(define (make-end-state name)

  (make-machine-state-0 name #f))

(define (make-machine-state name alphabet)

  (make-machine-state-0 name 
                        (make-vector (length (all-symbols alphabet)) #f)))

(define (set-state-transition! state alphabet symbol transition)

  (let ((encoding (symbol-encoding alphabet symbol))
        (transitions (machine-state-transitions state)))

    (unless (< -1 encoding (vector-length transitions))
      (error "set-state-transition! -- transition index out of bounds"
             alphabet
             symbol
             transitions))
    (vector-set! transitions
                 (symbol-encoding alphabet symbol)
                 transition)))

(define (choose-transition state tape)

  (vector-ref (machine-state-transitions state) 
              (read-from-tape tape)))

;; State transition diagram

(define-record-type std
  (make-std start)
  std?
  (start std-start))

(define log-machine-step (make-parameter
                          (lambda (state-name tape) #f)))

(define (run-machine diagram tape)

  (let loop ((state (std-start diagram))
             (tape tape))

    ((log-machine-step) (machine-state-name state) tape)
     
    (if (end-state? state)
        (values tape)

        (let* ((transition (choose-transition state tape))
                        (next-state (transition-next-state transition))
                        (next-tape (apply-transition transition tape)))

                   (loop next-state next-tape)))))

;; Output

(define (first-line tape)

  (let ((alphabet (tape-alphabet tape))
        (left (tape-left tape))
        (right (tape-right tape)))
    
    (list->string (map (lambda (symbol) (symbol-encoded-by alphabet symbol))
                       (append (reverse left) (if (null? right)
                                                  (list (empty-symbol-encoding alphabet))
                                                  right))))))
    
(define (second-line tape)

  (let* ((left-len (length (tape-left tape)))
         (spaces (map (lambda (i) #\space)
                      (iota left-len))))

    (list->string (append spaces (list #\^)))))

(define (display-tape tape)

  (display (first-line tape))
  (newline)
  (display (second-line tape))
  (newline))

(define (log state-name tape) 

  (display "Machine in state ")
  (display state-name)
  (newline)
  (display-tape tape)
  (newline))

;; Input

(define (alphabet-from-string letters)

  (when (> 2 (string-length letters))
    (error "alphabet-from-string -- to few letters for an alphabet"
           letters))

  (apply make-alphabet (string->list letters)))

(define (tape-from-input alphabet input)

  (let ((tape-letters (car input))
        (shift (if (null? (cdr input))
                   0
                   (cadr input))))

    (shift-tape (make-tape alphabet tape-letters) shift)))

(define (empty-states alphabet state-specs end-state-names)

  (append (map (lambda (state-spec) 
                 (make-machine-state (car state-spec) alphabet))
               state-specs)
          (map make-end-state end-state-names)))
                             
(define (add-transitions! alphabet states state-specs)

  (for-each 
   (lambda (state) 

     (unless (end-state? state)
       (for-each 
        (lambda (trans-spec)

          (set-state-transition! state
                                 alphabet
                                 (car trans-spec)
                                 (transition-from-spec trans-spec
                                                       states)))
        (cdr (assoc (machine-state-name state)
                    state-specs)))))

   states))

(define (find-state-named name states)

  (cond ((null? states)
         (error "find-state-named -- no state with name given" name))
        ((symbol=? name (machine-state-name (car states)))
         (car states))
        (else (find-state-named name (cdr states)))))
      

(define (transition-from-spec trans-spec states)

  (let ((write (cadr trans-spec))

        (move (let ((move-name (caddr trans-spec)))

                (cond ((eq? '<- move-name) move-left)
                      ((eq? '-> move-name) move-right)
                      (else (error "transition-from-spec -- unknown move specifier"
                                   move-name)))))

        (next-state (find-state-named (list-ref trans-spec 3) states)))

    (make-transition write move next-state)))

(define (diagram-from-spec alphabet spec)

  
  (let* ((state-specs (cddr spec))
         (states (empty-states alphabet state-specs (cadr spec))))

    (add-transitions! alphabet states state-specs)
    (make-std (find-state-named (car spec) states))))

(define (read-machine-with-input)

  (let* ((alphabet (alphabet-from-string (read)))
         (diagram (diagram-from-spec alphabet (read)))
         (tape (tape-from-input alphabet (read))))

    (values diagram tape)))

;; Program body

(let-values (((diagram tape) (read-machine-with-input)))

  (parameterize ((log-machine-step log))

    (run-machine diagram tape)))
