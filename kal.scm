#!/usr/bin/ol --run

(import
   (owl parse)
   (owl date)
   (owl args))

(define version "0.1")

(define (check-week-info date)
   (lets 
      ((_ d m y wday week date)
       (cweek cwday (week-info d m y))
       (wday (or wday cwday))
       (week (or week cweek)))
      (cond
         ((not (eq? wday cwday))
            (print-to stderr "incorrect week day")
            #false)
         ((not (eq? week cweek))
            (print-to stderr "incorrect week number")
            #false)
         (else 
            (tuple 'date d m y wday week)))))


;;;

(define (whitespace? x)
   (or (eq? x #\space) 
       (eq? x #\newline)
       (eq? x #\tab)
       (eq? x #\return)))

(define maybe-whitespace
   (get-greedy* (get-byte-if whitespace?)))

(define (ascii-digit? x)
   (and (<= #\0 x) (<= x #\9)))

(define get-nat
   (let-parses
      ((chars (get-greedy+ (get-byte-if ascii-digit?))))
      (fold (λ (n b) (+ (* n 10) (- b 48))) 0 chars)))

(define get-date
   (let-parses
      ((d get-nat) (skip (get-imm #\.))
       (m get-nat) (skip (get-imm #\.))
       (y get-nat)
       (verify (valid-date? d m y) "This date is not ok."))
      (tuple 'date d m y #f #f)))

(define (add-week-info date info)
   (if date
      (lets ((_ d m y wday week date))
         (tuple-case info
            ((week-day n)
               (if (and wday (not (eq? wday n)))
                  #false
                  (tuple 'date d m y n week)))
            ((week n)
               (if (and week (not (eq? week n)))
                  #false
                  (tuple 'date d m y wday n)))
            (else #false)))
      #false))

(define get-week-number
   (let-parses
      ((skip maybe-whitespace)
       (skip (get-word "week" 42))
       (skip maybe-whitespace)
       (n get-nat))
      (tuple 'week n)))

(define get-day-name
   (let-parses
      ((skip maybe-whitespace)
       (day
          (get-any-of
            (get-word "ma" 1)
            (get-word "ti" 2)
            (get-word "ke" 3)
            (get-word "to" 4)
            (get-word "pe" 5)
            (get-word "la" 6)
            (get-word "su" 7))))
      (tuple 'week-day day)))

(define get-week-info
   (get-either
      get-day-name 
      get-week-number))
      
(define get-date-info
   (let-parses
      ((d get-date)
       (is (get-greedy* get-week-info)))
      (check-week-info
         (fold add-week-info d is))))

(define get-line
   (let-parses
      ((rs (get-greedy* (get-rune-if (lambda (x) (not (eq? x #\newline))))))
       (skip (get-imm #\newline)))
      (list->string rs)))

(define get-event
   (let-parses
      ((skip (get-greedy+ (get-imm #\space)))
       (skip (get-imm #\-))
       (skip (get-imm #\space))
       (line get-line))
      (tuple 'event line)))

(define get-day
   (let-parses
      ((skip maybe-whitespace)
       (d get-date-info)
       (skip (get-imm #\newline))
       (es (get-greedy* get-event)))
      (tuple 'day d es)))

(define kal-grammar
   (let-parses
      ((vals
         (get-greedy* get-day))
       (tail maybe-whitespace))
      vals))

(define (kal-parse path)
   (let ((data (file->list path)))
      (if data
         (try-parse kal-grammar data path "bad kal: " #false)
         (begin
            (print-to stderr "Could not read " path)
            #false))))

;;;

(define command-line-rules
   (cl-rules
      `((date "-t" "--time" cook ,string->integer
            comment "use given current posix time")
        (help "-h" "--help"))))

(define usage-text 
   "Usage: kal ...")

(define (print-usage)
   (print usage-text)
   (print (format-rules command-line-rules)))

(define (kal-write x)
   (write x))

(define (kal dict args)
   (cond
      ((getf dict 'help)
         (print-usage)
         0)
      ((null? args)
         (print "dict: " dict)
         (print "args: " args)
         0)
      (else
         (map
            (lambda (x)
               (print x)
               (kal-write (kal-parse x))
               (print ""))
            args)
         0)))

(
(lambda (args)
   (process-arguments (cdr args) command-line-rules usage-text kal))
   (list "kal" "test.txt"))

