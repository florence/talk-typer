#lang racket/gui

(require framework)


(define-syntax-rule (cons! x id)
  (set! id (cons x id)))
(define-syntax-rule (rest! id)
  (let ([tmp (first id)])
    (set! id (rest id))
    tmp))
(define on-local-char/c
  (->m (is-a?/c key-event%) void?))

(define on-paint/c
  (->m any/c (is-a?/c dc<%>) real? real? real? real? real? real?
       (or/c (one-of/c 'no-caret 'show-inactive-caret 'show-caret)
             (cons/c natural-number/c natural-number/c))
       void?))

(provide/contract
  [talk-typer<%> interface?]
  [talk-typer-mixin
    (-> (class/c
          (inherit insert)
          (super on-local-char on-paint))
      (class/c
        [on-paint on-paint/c]
        [on-local-char on-local-char/c]
        (override [on-paint on-paint/c]
                  [on-local-char on-local-char/c])
        [talk-typer? (->m boolean?)]
        [toggle-talk-typer! (->m any/c)]
        [talk-typer-load-buffer! (->m any/c)]))])

(define talk-typer<%>
  (interface ()
    talk-typer?
    toggle-talk-typer!
    talk-typer-load-buffer!))

(define talk-typer-mixin
  (λ (cls)
    (class* cls (talk-typer<%>)

      ;; ==== public state & accessors ====
      (define/public-final (talk-typer?) talk-typer-on?)

      (define/public-final (toggle-talk-typer!)
        (when (or talk-next-buffer
                  (talk-typer-load-buffer!))
          (set! talk-typer-on? (not talk-typer-on?))))

      (define/public-final (talk-typer-load-buffer!)
        (define path (finder:get-file #f "Select TalkType Source File"))
        (and path (do-load-buffer! path)))

      ;;; Private state

      (define talk-typer-on? #f)
      ;; (U #f (Queueof Char)
      ;; invar: either both are lists, or both are false
      (define talk-next-buffer #f)
      (define talk-prev-buffer #f)

      ;; ==== overrides ====
      (define/override (on-local-char event)
        (cond [(null? talk-next-buffer) (void)]
              [(talk-typer?)
               (do-key event)]
              [else (super on-local-char event)]))

      ;; ==== private functionality ====
      (define (do-load-buffer! path)
        (set! talk-next-buffer
              (file->list path read-char #:mode 'text))
        (set! talk-prev-buffer null))

      (define (do-key event)
        (case (send event get-key-code)
          ;; todo backspace
          [else (insert-chars!)]))

      (define (insert-chars!)
        (define c (get-next-char))
        (when c
          (send this insert c)
          (next-char!)
          (when (and (whitespace? c)
                     (whitespace? (get-next-char)))
            (insert-chars!))))

      (define (get-next-char)
        (and (not (null? talk-next-buffer))
             (first talk-next-buffer)))
      (define (next-char!)
        (define x (rest! talk-next-buffer))
        (cons! x talk-prev-buffer))
      (define (prev-char!)
        (unless (null? talk-prev-buffer)
          (define x (rest! talk-prev-buffer))
          (cons! x talk-next-buffer)))

      (define (whitespace? c)
        (case c
          [(#\tab #\space #\newline #\linefeed
            #\return #\vtab #\page)
           #t]
          [else #f]))

      (super-new))))
