#lang racket

; Required modules for string manipulation and hash table functionality
(require racket/string)
(require racket/hash)

; Initialize hash tables to keep track of individual and team scores
(define player-total-scores (make-hash))
(define team-total-scores (make-hash))

; Function to parse individual scores, handling strikes, spares, numeric scores, and defaults
(define (parse-score score next1 next2)
  (cond
    [(string=? score "X") (+ 10 next1 next2)] ; Handle strike
    [(string=? score "/") (+ 10 next1)] ; Handle spare
    [(regexp-match? #rx"^[0-9]$" score) (string->number score)] ; Handle numeric score
    [else 0])) ; Default case

; Calculates total score for a single game, accounting for strikes and spares
(define (calculate-game-score scores)
  (let loop ([scores scores] [total 0])
    (if (null? scores)
        total
        (let* ([score (car scores)]
               [next1 (if (>= (length scores) 2) (parse-score (cadr scores) 0 0) 0)]
               [next2 (if (>= (length scores) 3) (parse-score (caddr scores) 0 0) 0)]
               [numeric-score (parse-score score next1 next2)])
          (loop (cdr scores) (+ total numeric-score))))))

; Updates a player's total score in the hash table
(define (update-player-total-scores player game-score)
  (hash-update! player-total-scores player (lambda (old-score) (+ old-score game-score)) 0))

; Updates a team's total score based on its players' scores
(define (update-team-total-score team-name player-scores)
  (let ([team-score (apply + (map cadr player-scores))])
    (hash-update! team-total-scores team-name (lambda (old-score) (+ old-score team-score)) 0)))

; Processes each game line, extracting and updating scores for each player
(define (process-player-games games)
  (map (lambda (game-line)
         (let* ([tokens (string-split game-line " ")]
                [player-name (string-join (take tokens 2) " ")] ; Assumes player names are two words
                [scores (drop tokens 2)]
                [game-score (calculate-game-score scores)])
           (update-player-total-scores player-name game-score)
           (list player-name game-score)))
       games))

; List of known team names for validation
(define known-team-names '("Oddballs" "Geeks"))

; Checks if a line is a known team name
(define (is-team-name line)
  (member line known-team-names))

; Splits the input into sections by team, organizing lines accordingly
(define (split-teams lines)
  (let loop ([lines lines] [teams '()] [current-team '()] [current-team-name ""])
    (cond
      [(null? lines) (reverse (cons (cons current-team-name (reverse current-team)) teams))]
      [else
       (let ([line (car lines)])
         (if (is-team-name line)
             (loop (cdr lines)
                   (if (null? current-team) teams (reverse (cons (cons current-team-name (reverse current-team)) teams)))
                   '()
                   line)
             (loop (cdr lines) teams (cons line current-team) current-team-name)))])))

; Reads scores from a file, processes them, and updates team and player scores
(define (read-scores file)
  (define lines (filter (lambda (line) (not (string=? line ""))) (file->lines file)))
  (let ([team-sections (split-teams lines)])
    (map (lambda (team-section)
           (let* ([team-name (first team-section)]
                  [player-lines (cdr team-section)]
                  [player-scores (process-player-games player-lines)])
             (update-team-total-score team-name player-scores)
             (cons team-name player-scores)))
         team-sections)))

; Prints the final results, displaying team names and scores, and player scores
(define (print-results teams)
  (for ([team (in-list teams)])
    (let ([team-name (car team)])
      (displayln (format "Team: ~a, Overall Score: ~a" team-name (hash-ref team-total-scores team-name 0)))
      (for ([(player-name total-score) (in-hash player-total-scores)])
        (when (member player-name (map car (cdr team)))
          (displayln (format "Player: ~a, Total Score Across All Games: ~a" player-name total-score)))))))

; Main
(define (main)
  (define file "scores.txt")
  (let ([teams (read-scores file)])
    (displayln "Printing final results:")
    (print-results teams)))

(main)