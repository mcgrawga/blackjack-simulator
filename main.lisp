;(load "functions.lisp")
;
;(format t "~%")
;(format t "  ____  _            _     _            _    ~%")
;(format t " | __ )| | __ _  ___| | __(_) __ _  ___| | __~%")
;(format t " |  _ \\| |/ _` |/ __| |/ /| |/ _` |/ __| |/ /~%")
;(format t " | |_) | | (_| | (__|   < | | (_| | (__|   < ~%")
;(format t " |____/|_|\\__,_|\\___|_|\\_\\/ |\\__,_|\\___|_|\\_\\~%")
;(format t "                        |__/                 ~%")
;(format t "~%")
;(format t "~%")
;
;
;(setq player-stack 100)
;(setq shoe (shuffle-cards (create-deck)))
;
;(format t "You have $~a.  How much do you want to bet?:  " player-stack)
;(setq player-bet (read))
;(setq player-hand nil)
;(setq dealer-hand nil)
;(multiple-value-setq (card shoe) (deal-card shoe))
;(push card player-hand)
;(multiple-value-setq (card shoe) (deal-card shoe))
;(push card dealer-hand)
;(multiple-value-setq (card shoe) (deal-card shoe))
;(push card player-hand)
;(multiple-value-setq (card shoe) (deal-card shoe))
;(push card dealer-hand)
;(multiple-value-setq (card shoe) (deal-card shoe))
;(push (create-card 'spades 'ace) player-hand)
;(multiple-value-setq (player-hand-val-1 player-hand-val-2) (calculate-hand-value player-hand))
;
;(format t "Dealer shows a: ~:(~a~)~%" (getf (car dealer-hand) :val))
;(format t "You have a ~a~@[ or ~a~]. (~{~a~^ ~})~%" player-hand-val-1 player-hand-val-2 (get-hand-vals player-hand))
;(format t "Do you want to Hit[h] Stand[s] Double Down[d]:  ")
;(setq player-action (read))


;(screen:clear-window (screen:make-window))

(handler-case
    (progn
        (format t "Step 1~%")
        (format t "Step 2~%")
        (format t "Step 3~%")
        (error "Can't deal a card from a deck with less than one card.")
        (format t "Step 4~%")
    )
    (t (error)
        (format t "Exception: ~a~%" error)
        (format t "Exception: ~a~%" error))
)