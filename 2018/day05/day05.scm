#!/usr/bin/csi -script

(define TEST-INPUT "dabAcCaCBAcCcaDA")

(import (chicken io))
(import (chicken irregex))
(import (chicken string))
(import srfi-1)

(define (one-pair n)
  (let* ((lchar (integer->char (+ n (char->integer #\a))))
         (uchar (char-upcase lchar)))
    (list (string lchar uchar) (string uchar lchar))))

(define all-pairs (append-map one-pair (iota 26)))

(define (polymer-react s)
  (string-translate* s (map (lambda (pair) (cons pair "")) all-pairs)))

(define (polymer-react/full data #!optional progress)
  (let loop ((s data)
             (len (string-length data)))
    (when progress
      (print "Current length: " len))
    (set! s (polymer-react s))
    (let ((new-len (string-length s)))
      (if (= new-len len)
          s
          (loop s new-len)))))

(define (solve-part1 data)
  (string-length (polymer-react/full data #t)))

(define (remove-char s n)
  (let ((char (integer->char (+ n (char->integer #\a)))))
    (irregex-replace/all (irregex char 'case-insensitive) s)))

(define (solve-part2 data)
  (let* ((baseline (begin
                    (print "Full reaction…")
                    (polymer-react/full data)))
         (shorts (map (lambda (n)
                        (print "Processing " n)
                        (polymer-react/full (remove-char baseline n)))
                      (iota 26))))
    (apply min (map string-length shorts))))

(let ((test-data TEST-INPUT)
      (input-data (string-chomp (with-input-from-file "input" read-string))))
  (print "Part 1")
  (print "======")
  (print "TEST " (solve-part1 test-data))
  (print "INPUT " (solve-part1 input-data))

  (print)
  (print "Part 2")
  (print "=======")
  (print "TEST " (solve-part2 test-data))
  (print "TEST " (solve-part2 input-data)))
