(setf *random-state* (make-random-state t)) ;Seed random number generator

(defun create-card(suit val)
    "Creates a card, i.e list with suit and value"
    (list :suit suit :val val))

(defun create-deck()
    "Creates a deck of cards"
    (setq deck nil)
    (dolist (suit '(spades diamonds hearts clubs))
        (dolist (val '("ace" "king" "queen" "jack" "10" "9" "8" "7" "6" "5" "4" "3" "2"))
            (push (create-card suit val) deck)))
    (return-from create-deck deck))

(defun create-shoe(num-decks)
    "Creates an n deck shoe of card decks"
    (setq shoe nil)
    (loop repeat num-decks do 
        (setq deck (create-deck))
        (dolist (card deck) (push card shoe)))
    (return-from create-shoe shoe))

(defun remove-card-from-deck(deck index)
    "Removes the list item at index and returns
    the resulting list"
    (setq new-deck nil)
    (loop for x in deck
        for i from 0
        if (/= i index) do (push x new-deck))
    (return-from remove-card-from-deck (reverse new-deck)))

(defun shuffle-cards(cards)
    "Shuffles a list of cards"
    (setq shuffled-cards nil)
    (loop
        until (< (list-length cards) 1)
        do  (setq index (random (list-length cards)))
            (push (nth index cards) shuffled-cards)
            (setq cards (remove-card-from-deck cards index)))
    (return-from shuffle-cards shuffled-cards))

(defun deal-card(cards)
    "Removes the first card from the deck and returns it
     along with the modified deck"
    (setq first-card (car cards))
    (setq cards (cdr cards))
    (values first-card cards))

(defun get-num-val-card(card)
    (setq string-val (getf card :val))
    (if (string-equal string-val "2") (return-from get-num-val-card 2))
    (if (string-equal string-val "3") (return-from get-num-val-card 3))
    (if (string-equal string-val "4") (return-from get-num-val-card 4))
    (if (string-equal string-val "5") (return-from get-num-val-card 5))
    (if (string-equal string-val "6") (return-from get-num-val-card 6))
    (if (string-equal string-val "7") (return-from get-num-val-card 7))
    (if (string-equal string-val "8") (return-from get-num-val-card 8))
    (if (string-equal string-val "9") (return-from get-num-val-card 9))
    (if (string-equal string-val "10") (return-from get-num-val-card 10))
    (if (string-equal string-val "jack") (return-from get-num-val-card 10))
    (if (string-equal string-val "queen") (return-from get-num-val-card 10))
    (if (string-equal string-val "king") (return-from get-num-val-card 10))
    (if (string-equal string-val "ace") (values 1 11)))



(defun calculate-hand-value(hand)
    "Calculates the value of a blackjack hand.  Returns 2 values.
     If it's not a soft hand (contains ace), second value is nil"
    (setq total-1 0)
    (setq total-2 0)

    (setq hand-contains-ace nil)
    (loop for card in hand do
        (if (string-equal (getf card :val) "ace") (setq hand-contains-ace T) )
        (incf total-1 (get-num-val-card card)))
    (if hand-contains-ace 
        (setq total-2 (+ total-1 10))
        (setq total-2 nil))
    (if total-2
        (if (> total-2 21) 
            (setq total-2 nil)
            (if (= total-2 21) 
                (progn 
                    (setq total-2 nil) 
                    (setq total-1 21)))))
    (values total-1 total-2))

(setq hand ())
;(push (create-card "spades" "jack") hand)
;(push (create-card "spades" "ace") hand)
(push (create-card "spades" "10") hand)
(push (create-card "spades" "10") hand)
(push (create-card "spades" "queen") hand)
(format t "hand: ~a~%" hand)
(multiple-value-setq (v1 v2) (calculate-hand-value hand))
(format t "v1: ~a  v2: ~a~%" v1 v2)