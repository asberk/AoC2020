(ql:quickload 'split-sequence :silent t)

(defun parse-input ()
  (labels ((load-data () (read-file "d22.txt"))
           (parse-deck (deck)
             (mapcar #'parse-integer (subseq deck 1)))
           (split-players ()
             (let ((decks
                    (split-sequence:split-sequence-if
                     #'(lambda (x) (zerop (length x)))
                     (load-data))))
               (mapcar #'parse-deck decks)))
           (main ()
             (destructuring-bind (deck1 deck2) (split-players)
               (defparameter *deck1* deck1)
               (defparameter *deck2* deck2))))
    (main)))

(defun play-round ()
  (when (and *deck1* *deck2*)
    (let ((card1 (pop *deck1*))
          (card2 (pop *deck2*)))
      (cond ((> card1 card2) (setf *deck1* (append *deck1* (list card1 card2))))
            ((> card2 card1) (setf *deck2* (append *deck2* (list card2 card1))))
            (t (error "The cards should never be equal."))))
    t))


;;;; part 2

(defun record-round (deck1 deck2)
  "Return T if round was already in record; if not this function has the side
  effect of creating a record. Key is DECK1, value is DECK2."
  (cond ((gethash (list deck1 deck2) *game-record*) t)
        (t (setf (gethash (list deck1 deck2) *game-record*) :seen)
           nil)))

(defun deck-subset (deck card)
  (assert (>= (length deck) card))
  (loop for i from 0 below card
     for x in deck collect x))

(defun auto-win-p (arg)
  (eql arg :player-one-wins))

(defun play-recursive-round (deck1 deck2 &optional (game-number 1) (round-number 1))
  (when (record-round deck1 deck2)
    (return-from play-recursive-round (list :player-one-wins nil)))
  (when (and deck1 deck2)
    (let ((card1 (pop deck1))
          (card2 (pop deck2))
          (num-remaining1 (length deck1))
          (num-remaining2 (length deck2)))
      (if (and (>= num-remaining1 card1)
               (>= num-remaining2 card2))
          (let ((subgame-result
                 (play-recursive-combat (deck-subset deck1 card1)
                                        (deck-subset deck2 card2)
                                        (incf game-number) 0)))
            (cond ((eql (car subgame-result) :player-one-wins)
                   (list (append deck1 (list card1 card2)) deck2))
                  ((eql (car subgame-result) :player-two-wins)
                   (list deck1 (append deck2 (list card2 card1))))
                  (t (error "not implemented."))))
          (cond ((> card1 card2)
                 (list (append deck1 (list card1 card2)) deck2))
                ((> card2 card1)
                 (list deck1 (append deck2 (list card2 card1))))
                (t (error "The cards should never be equal.")))))))


(defun play-recursive-combat (deck1 deck2 &optional (game-number 1) (starting-round-number 0))
  (let ((*game-record* (make-hash-table :test #'equal)))
    (loop while (and deck1 deck2)
       do (destructuring-bind (new-deck1 new-deck2)
              (play-recursive-round deck1 deck2 game-number (incf starting-round-number))
            (when (auto-win-p new-deck1)
              (return-from play-recursive-combat (list new-deck1 deck1 deck2)))
            (setf deck1 new-deck1)
            (setf deck2 new-deck2))
       finally (return (list (if deck1
                                 :player-one-wins
                                 :player-two-wins)
                             deck1 deck2)))))

;;;; final

(defun day22-part1 ()
  (parse-input)
  (loop while (play-round))
  (let ((deck (if *deck1* *deck1* *deck2*)))
    (loop for card in deck
       for counter from (length deck) downto 0
       summing (* card counter))))

(defun day22-part2 ()
  (defparameter *game-record* (make-hash-table :test #'equal))
  (parse-input)
  (let* ((result (play-recursive-combat *deck1* *deck2*))
         (deck (if (eql (car result) :player-one-wins) (cadr result) (caddr result))))
    (loop for card in deck
       for counter from (length deck) downto 0
       summing (* card counter))))

(verbose1
  (day22-part1)
  (day22-part2))
