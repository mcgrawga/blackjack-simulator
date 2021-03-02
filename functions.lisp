(setf *random-state* (make-random-state t)) ;Seed random number generator

(defun create-card(suit val)
    "Creates a card, i.e list with suit and value"
    (list :suit suit :val val))

(defun create-deck()
    "Creates a deck of cards"
    (setq deck nil)
    (dolist (suit '(spades diamonds hearts clubs))
        (dolist (val '(ace king queen jack 10 9 8 7 6 5 4 3 2))
            (push (create-card suit val) deck)))
    (return-from create-deck deck))

(defun create-shoe(num-decks)
    "Creates an n deck shoe of card decks"
    (setq shoe nil)
    (loop repeat num-decks do 
        (setq deck (create-deck))
        (dolist (card deck) (push card shoe)))
    (return-from create-shoe shoe))

(defun remove-item-from-list(the-list index)
    "Removes the list item at index and returns
    the resulting list"
    (setq new-list nil)
    (loop for x in the-list
        for i from 0
        if (/= i index) do (push x new-list))
    (return-from remove-item-from-list (reverse new-list)))

(defun shuffle-cards(cards)
    "Shuffles a list of cards"
    (setq shuffled-cards nil)
    (loop
        until (< (list-length cards) 1)
        do  (setq index (random (list-length cards)))
            (push (nth index cards) shuffled-cards)
            (setq cards (remove-item-from-list cards index)))
    (return-from shuffle-cards shuffled-cards))

(defun deal-card(cards)
    "Removes the first card from the deck and returns it
     along with the modified deck"
    (setq first-card (car cards))
    (setq cards (cdr cards))
    (values first-card cards))