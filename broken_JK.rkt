#lang racket
(require racket/string)
(require rackunit)
(require rackunit/text-ui)

; Check if the file is a text file, if not raise an error.
; Same as the test suite below this was used when I started with a prompt for user input similar to a C++ program.
; Got rid of that concept since you can't test every situation possible. 
(define (is-txt? filename)
  (unless (string-suffix? filename ".txt")
    (raise-user-error "NonTxtFileTypeException" "Please use a .txt for the input file type")))

; File type extension for unit testing. This was fun to build out and use with user prompt. 
(define-test-suite file-type-validation
  (test-case
   "Test with a .txt file"
   (check-not-exn (lambda () (is-txt? "./scores.txt"))))
  (test-case
   "Test with non .txt file"
   (check-exn exn:fail? (lambda () (is-txt? "./scores.csv"))))
  (test-case
   "Test with file with no extension"
   (check-exn exn:fail? (lambda () (is-txt? "./scores")))))
(run-tests file-type-validation)

; Creating intial data strucutre for our data.
(define-struct team(name players))

; Creating a new team name starting with an empty list. 
(define (new-team name)
  (new-team name'()))

; Fucntion to add a player to a team from the team and teh name
(define (add-player player team)
  (new-team (team-name team) (cons player (team-players team))))

; Function to read file in
(define (file-input filename)
  (with-input-from-file filename
    (lambda ()
      (let loop ((lines '())
                 (line-number 1))
        (let ((line (read-line)))
          (if (eof-object? line)
              (reverse lines)
              (loop (cons (cons line-number line) lines)
                    (+ line-number 1))))))))

; Helper function to validate if the line is a team name.
(define (is-team? line) 
  (and (= (length (string-split line)) 1) ; Checking if lines only have one word (team name)
       (not (string=? line ""))))


; Parse lines from file for team names and display to user
(define (process-lines lines)
  (define (helper lines processed-lines)
    (cond
      [(null? lines) (reverse processed-lines)] ; All lines processed, return result
      [else
       (let ([line-pair (car lines)]) ; Extract the current line pair
         (let ([player (parse-player line-pair)]) ; Parse the current line
           (helper (cdr lines) lines (if current-team (cons (list current-team) teams) teams)) ; Start a new team
               (let ([player (parse-player line)]) ; Parse the current line for player information
                 (helper (cdr lines) current-team (cons player (if (null? current-team) teams (car teams)))))))])) ; Add player to current team or process rest
  (helper lines '() '()))

; Function to process lines
(define (parse-player line-pair)
  (let* ([line (cdr line-pair)] ; Extract the string part of the pair
         [parts (string-split line ",")]
         [name (string-trim (list-ref parts 0))]
         [position (string-trim (list-ref parts 1))]
         [number (string-trim (list-ref parts 2))])
    (list name position number)))

; Function for converting score symbols to numbers


; Function for calculating simple bowling scores


; Print team name and highest player score, Pass player score into print-team-high-scores
(define (print-team-high-scores player-scores teams)
  (for-each
   (lambda (t)
     (define max-score
       (apply max (map (lambda (p) (apply max (player-scores p))) (team-players t))))
     (printf "Team: ~a, Highest Score: ~a\n" (team-name t) max-score))
   teams))

; Main function to tie it all together
(define (main)
  (define lines (file-input "./scores.txt"))
    (define teams (process-lines lines))
    ; Now print each team and the highest player score
    (print-team-high-scores teams))
(main)
