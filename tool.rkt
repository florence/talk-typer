#lang racket/unit

(require drracket/tool
         framework
         racket/class
         racket/gui
         "private/text.rkt")

(import drracket:tool^)
(export drracket:tool-exports^)

(define talk-typer-frame-mixin
  (mixin (drracket:unit:frame<%>) ()

    (inherit get-definitions-text)

    (define/override (edit-menu:between-find-and-preferences edit-menu)
      (super edit-menu:between-find-and-preferences edit-menu)
      (new checkable-menu-item%
           [label "TalkTyper Enabled"]
           [parent edit-menu]
           [callback
             (λ (i e) (send (get-definitions-text) toggle-talk-typer!))]
           [checked (send (get-definitions-text) talk-typer?)])
      (new menu-item%
           [label "Pick TalkTyper File"]
           [parent edit-menu]
           [callback
            (lambda (i e) (send (get-definitions-text) talk-typer-load-buffer!))]))

    ;; for the talk-typer status (mode text, etc.)
    (define talk-typer-status-parent-panel 'uninitialized)
    (define talk-typer-status-panel 'uninitialized)
    (define talk-typer-status-message 'uninitialized)

    ;; overriden to add a status panel
    (define/override (make-root-area-container cls parent)
      (set! talk-typer-status-parent-panel
            (super make-root-area-container vertical-panel% parent))
      (define root (new cls [parent talk-typer-status-parent-panel]))
      (set! talk-typer-status-panel
            (new horizontal-panel%
                 [style '(border)]
                 [stretchable-height #f]
                 [parent talk-typer-status-parent-panel]))
      (set! talk-typer-status-message
            (new message%
                 [parent talk-typer-status-panel]
                 [auto-resize #t]
                 [label ""]))
      root)

    (define/public (set-talk-typer-status-message str)
      (send talk-typer-status-message set-label str))

    (super-new)))

(define (phase1) (void))
(define (phase2) (void))

(drracket:get/extend:extend-definitions-text talk-typer-mixin)
(drracket:get/extend:extend-unit-frame talk-typer-frame-mixin)
