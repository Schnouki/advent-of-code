#!/usr/bin/csi -script

(define TEST-INPUT "dabAcCaCBAcCcaDA")

(import (chicken io))
(import (chicken irregex))
(import (chicken string))
(import srfi-1)

(define all-pairs
  (append-map (lambda (n)
                (let* ((lchar (integer->char (+ n (char->integer #\a))))
                       (uchar (char-upcase lchar)))
                  (list (string lchar uchar) (string uchar lchar))))
              (iota 26)))

(define reaction-regex (irregex-opt all-pairs))

(define (polymer-react s)
  (irregex-replace/all reaction-regex s))

(define (polymer-react/full data #!optional progress)
  (do ((s data (polymer-react s)))
      ((not (irregex-search reaction-regex s)) s)
    (when progress
      (print "Current length: " (string-length s)))))

(define (solve-part1 data)
  (string-length (polymer-react/full data #t)))

(let ((test-data TEST-INPUT)
      (input-data (string-chomp (with-input-from-file "input" read-string))))
  (print "Part 1")
  (print "======")
  (print "TEST " (solve-part1 test-data))
  (print "INPUT " (solve-part1 input-data)))
