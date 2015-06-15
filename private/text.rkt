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

(define-local-member-name super-on-local-char)
(define talk-typer-mixin
  (λ (cls)
    (class* cls (talk-typer<%>)
      (super-new)
      ;; ==== private state ====
      (define off-delegate (new off-delegate% [sup this]))
      (define input-delegate (new input-delegate% [sup this]))
      (define delegate off-delegate)

      ;; ==== public state & accessors ====
      (define/public-final (talk-typer?) (send delegate insert-mode?))

      (define/public-final (toggle-talk-typer!)
        (set-delegate!
         (if (send delegate insert-mode?) off-delegate input-delegate)))

      (define/public-final (talk-typer-load-buffer!)
        (define path (finder:get-file #f "Select TalkType Source File"))
        (and path (send delegate get-buffer! path)))

      (define/public (super-on-local-char event)
        (super on-local-char event))
      ;; ==== overrides ====

      (define/override (on-local-char event)
        (send delegate handle-char event))

      ;; private methods
      (define/private (set-delegate! d)
        (set! delegate d)
        (send delegate enable!)))))

(define delegate<%>
  (interface ()
    enable!
    handle-char
    insert-mode?
    record-mode?
    get-buffer!))

(define off-delegate%
  (class* object% (delegate<%>)
    [init-field sup]
    (define/public (enable!) (void))
    (define/public (handle-char evt)
      (send sup super-on-local-char evt))
    (define/public (insert-mode?) #f)
    (define/public (record-mode?) #f)
    (define/public (get-buffer!) (void))
    (super-new)))

(define input-delegate%
  (class* object% (delegate<%>)
    (super-new)
    [init-field sup]
    ;; ==== private fields ====
    ;; (U #f (List Char)
    ;; invar: either both are lists, or both are false
    (define talk-next-buffer #f)
    (define talk-prev-buffer #f)


    ;; ==== overrides ====
    (define/public (enable!)
      (unless talk-next-buffer
        (get-buffer!)))

    (define/public (handle-char event)
      (unless (null? talk-next-buffer)
        (do-key event)))

    (define/public (insert-mode?) #t)
    (define/public (record-mode?) #f)

    (define/public-final (get-buffer!)
      (define path (finder:get-file #f "Select TalkType Source File"))
      (and path (do-load-buffer! path)))

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
        (send sup insert c)
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
        [else #f]))))
