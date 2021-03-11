(setf *random-state* (make-random-state t)) ;Seed random number generator

(defun create-card(suit val)
    "Creates a card, i.e list with suit and value"
    (if (not (find suit '(spades diamonds hearts clubs))) 
        (error "Cards must have suit of spades diamonds hearts clubs."))
    (if (not (find val `(ace king queen jack 10 9 8 7 6 5 4 3 2))) 
        (error "Cards must have val of ace king queen jack 10 9 8 7 6 5 4 3 2."))
    (list :suit suit :val val))


(defun create-deck()
    "Creates a deck of cards"
    (setq deck nil)
    (dolist (suit `(spades diamonds hearts clubs))
        (dolist (val `(ace king queen jack 10 9 8 7 6 5 4 3 2))
            (push (create-card suit val) deck)))
    (return-from create-deck deck))


(defun create-shoe(num-decks)
    "Creates an n deck shoe of card decks"
    (if (< num-decks 1) 
        (error "Can not create a shoe with less than 1 deck."))
    (if (> num-decks 8)
        (error "Can not create a shoe with more than 8 decks."))
    (setq shoe nil)
    (loop repeat num-decks do 
        (setq deck (create-deck))
        (dolist (card deck) (push card shoe)))
    (return-from create-shoe shoe))

(defun remove-card-from-deck(deck index)
    "Removes the list item at index and returns
    the resulting list"
    (setq cards-in-deck (list-length deck))
    (if (< cards-in-deck 1) 
        (error "Can't remove a card from deck with less than 1 card."))
    (if (< index 0) 
        (error "Index can't be less than zero."))
    (if (> index (- cards-in-deck 1)) 
        (error (format nil "Index can't be greater than ~a." (- cards-in-deck 1))))
    (setq new-deck nil)
    (loop for x in deck
        for i from 0
        if (/= i index) do (push x new-deck))
    (return-from remove-card-from-deck (reverse new-deck)))

(defun shuffle-cards(cards)
    "Shuffles a list of cards"
    (setq cards-in-deck (list-length cards))
    (if (< cards-in-deck 1) 
        (error "Can't shuffle an empty deck."))
    (setq shuffled-cards nil)
    (loop
        until (< (list-length cards) 1)
        do  (setq index (random (list-length cards)))
            (push (nth index cards) shuffled-cards)
            (setq cards (remove-card-from-deck cards index)))
    (return-from shuffle-cards shuffled-cards))

(defun deck-has-at-least-n-cards(deck n)
    "Returns true if deck has at least n cards"
    (setq num-cards-in-deck (list-length deck))
    (>= num-cards-in-deck n))

(defun deal-card(cards)
    "Removes the first card from the deck and returns it
     along with the modified deck"
    (if (not (deck-has-at-least-n-cards cards 1)) 
        (error "Can't deal a card from a deck with less than one card."))
    (setq first-card (car cards))
    (setq cards (cdr cards))
    (values first-card cards))


(defun get-num-val-card(card)
    "Returns the number value for a card.  Returns both 1 and 11 for an ace."
    (if (not card) 
        (error "Can't get num value for a nil card."))
    (setq string-val (getf card :val))
    (if (eq string-val `2) (return-from get-num-val-card 2))
    (if (eq string-val `3) (return-from get-num-val-card 3))
    (if (eq string-val `4) (return-from get-num-val-card 4))
    (if (eq string-val `5) (return-from get-num-val-card 5))
    (if (eq string-val `6) (return-from get-num-val-card 6))
    (if (eq string-val `7) (return-from get-num-val-card 7))
    (if (eq string-val `8) (return-from get-num-val-card 8))
    (if (eq string-val `9) (return-from get-num-val-card 9))
    (if (eq string-val `10) (return-from get-num-val-card 10))
    (if (eq string-val `jack) (return-from get-num-val-card 10))
    (if (eq string-val `queen) (return-from get-num-val-card 10))
    (if (eq string-val `king) (return-from get-num-val-card 10))
    (if (eq string-val `ace) (values 1 11)))




(defun get-hand-vals(hand)
    "Returns a list of only values for a hand.  Does not include suits"
    (if (not hand) 
        (error "Can't get list of values for empty hand."))
    (setq vals nil)
    (loop for card in hand do
        (push (getf card :val) vals))
    (return-from get-hand-vals vals))
   


(defun calculate-hand-value(hand)
    "Calculates the value of a blackjack hand.  Returns 2 values.
     If it's not a soft hand (contains ace), second value is nil"
     (if (not hand) 
        (error "Can't calculate hand value for empty hand."))
    (setq total-1 0)
    (setq total-2 0)

    (setq hand-contains-ace nil)
    (loop for card in hand do
        (if (eq (getf card :val) `ace) (setq hand-contains-ace T) )
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

;(defun deal-player-and-dealer(shoe)
;    "Deals the initial hand to player and dealer.
;     Returns both hands and the modified deck"
;    (setq player-hand nil)
;    (setq dealer-hand nil)
;    (setq cards-in-shoe (list-length shoe))
;    (if (list-length shoe))
;)