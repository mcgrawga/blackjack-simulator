(load "functions.lisp")


(format t "  ____  _            _     _            _    ~%")
(format t " | __ )| | __ _  ___| | __(_) __ _  ___| | __~%")
(format t " |  _ \\| |/ _` |/ __| |/ /| |/ _` |/ __| |/ /~%")
(format t " | |_) | | (_| | (__|   < | | (_| | (__|   < ~%")
(format t " |____/|_|\\__,_|\\___|_|\\_\\/ |\\__,_|\\___|_|\\_\\~%")
(format t "                        |__/                 ~%")


(setq player-stack 100)
(setq shoe (create-shoe 8))
(setq my-list `(a b c))
(multiple-value-setq (first-item new-list) (deal-card my-list))
(format t "first item ~a~%" first-item)
(format t "list ~a~%" my-list)
(format t "list ~a~%" new-list)


;(format t "You have $~a.  How much do you want to bet?:  " player-stack)
;(setq player-bet (read))
;(setq player-hand)