(load "functions.lisp")

(setq player-stack 100)
(setq shoe (shuffle-cards (create-shoe 8)))
(setq quit nil)
(loop while (not quit) do
    (screen:clear-window (screen:make-window))
    (print-title)
    ; Get bet.
    (setq player-bet (get-player-bet player-stack))

    ; Initial deal.
    (multiple-value-setq (player-hand dealer-hand shoe) (deal-player-and-dealer shoe))

    ; Let player play hand
    (multiple-value-setq (player-hand shoe) (player-play player-hand dealer-hand shoe))

    ; Dealer play
    (multiple-value-setq (dealer-hand shoe) (dealer-play dealer-hand shoe))

    ; Print final values
    (setq player-hand-val (get-best-hand-value player-hand))
    (setq dealer-hand-val (get-best-hand-value dealer-hand))
    (setq winner (get-winner player-hand dealer-hand))
    (format t "~%")
    (format t "Dealer has ~a. (~{~a~^ ~})~%" dealer-hand-val (get-hand-vals dealer-hand))
    (format t "You have ~a. (~{~a~^ ~})~%" player-hand-val (get-hand-vals player-hand))
    (if (equal winner 'player) (incf player-stack player-bet))
    (if (equal winner 'dealer) (decf player-stack player-bet))
    (if (equal winner 'player)
        (format t "You won $~a and you now have $~a.~%" player-bet player-stack))
    (if (equal winner 'dealer)
        (format t "You lost $~a and you now have $~a.~%" player-bet player-stack))
    (if (equal winner 'push)
        (format t "It was a push and you now have $~a.~%" player-stack))

    ; See if player wants to continue playing, if player has money left.
    (if (> player-stack 0)
        (if (equal (continue-playing) 'q) (setq quit T))
        (setq quit T))
)