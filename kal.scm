#!/usr/bin/ol --run

(import
   (owl parse)
   (owl date)
   (owl args))

(define version "0.1")


;;;

(define (ascii-digit? x)
   (and (<= #\0 x) (<= x #\9)))

(define get-nat
   (let-parses
      ((chars (get-greedy+ (get-byte-if ascii-digit?))))
      (fold (λ (n b) (+ (* n 10) (- b 48))) 0 chars)))

(define get-date
   (let-parses
      ((d get-nat)
       (skip (get-imm #\.))
       (m get-nat)
       (skip (get-imm #\.))
       (y get-nat)
       (verify (valid-date? d m y) "This date is not ok."))
      (tuple 'date d m y)))
 
(define get-line
   (let-parses
      ((rs (get-greedy* (get-rune-if (lambda (x) (not (eq? x #\newline))))))
       (skip (get-imm #\newline)))
      (list->string rs)))

(define kal-grammar
   (let-parses
      ((vals
         (get-greedy*
            (get-either
               (let-parses
                  ((d get-date)
                   (skip (get-imm #\newline)))
                  d)
               get-line))))
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
               (write (kal-parse x))
               (print ""))
            args)
         0)))

(lambda (args)
   (process-arguments (cdr args) command-line-rules usage-text kal))
