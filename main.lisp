(load "functions.lisp")

;(format t "~%")
;(format t " ______   _____          _        ______  ___  ____      _____     _        ______  ___  ____~%")
;(format t "|_   _ \\ |_   _|        / \\     .' ___  ||_  ||_  _|    |_   _|   / \\     .' ___  ||_  ||_  _|~%")
;(format t "  | |_) |  | |         / _ \\   / .'   \\_|  | |_/ /        | |    / _ \\   / .'   \\_|  | |_/ /~%")
;(format t "  |  __'.  | |   _    / ___ \\  | |         |  __'.    _   | |   / ___ \\  | |         |  __'.~%")
;(format t " _| |__) |_| |__/ | _/ /   \\ \\_\\ `.___.'\\ _| |  \\ \\_ | |__' | _/ /   \\ \\_\\ `.___.'\\ _| |  \\ \\_~%")
;(format t "|_______/|________||____| |____|`.____ .'|____||____|`.____.'|____| |____|`.____ .'|____||____|~%")
;(format t "~%")
;(format t "~%")


(setq player-stack 100)
(setq shoe (shuffle-cards (create-deck)))
;(setq player-bet (get-player-bet))

; Initial deal.
(multiple-value-setq (player-hand dealer-hand shoe) (deal-player-and-dealer shoe))

; Let player play hand
(multiple-value-setq (player-hand shoe) (player-play player-hand dealer-hand shoe))

; Dealer play
(multiple-value-setq (dealer-hand shoe) (dealer-play dealer-hand shoe))

; Print final values
(multiple-value-setq (player-hand-val-1 player-hand-val-2) (calculate-hand-value player-hand))
(format t "You ended up with ~a~@[ or ~a~]. (~{~a~^ ~})~%" player-hand-val-1 player-hand-val-2 (get-hand-vals player-hand))
(multiple-value-setq (dealer-hand-val-1 dealer-hand-val-2) (calculate-hand-value dealer-hand))
(format t "Dealer ended up with ~a~@[ or ~a~]. (~{~a~^ ~})~%" dealer-hand-val-1 dealer-hand-val-2 (get-hand-vals dealer-hand))





;(screen:clear-window (screen:make-window))