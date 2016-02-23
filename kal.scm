#!/usr/bin/ol --run

(import
   (owl parse)
   (owl args))

(define version "0.1")


;;;

(define get-line
   (let-parses
      ((rs (get-greedy* (get-rune-if (lambda (x) (not (eq? x #\newline))))))
       (skip (get-imm #\newline)))
      (list->string rs)))

(define kal-grammar
   (let-parses
      ((lines (get-greedy* get-line)))
      lines))

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
