#lang setup/infotab

(define drracket-tools '(("tool.rkt")))
(define drracket-tool-names (list "TalkType"))
(define drracket-tool-icons '(#f))

(define categories '(devtools))
(define primary-file "tool.rkt")

(define deps '("base" "gui-lib" "data-lib" "drracket-plugin-lib"))
(define single-collection "talk-typer")

(define verion "0.1")
