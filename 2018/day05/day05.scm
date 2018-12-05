#!/usr/bin/csi -script

(define TEST-INPUT "dabAcCaCBAcCcaDA")

(import (chicken io))
(import (chicken irregex))
(import (chicken string))
(import srfi-1)

(define (is-pair? c1 c2)
  (char=? c2 (if (char-upper-case? c1)
                 (char-downcase c1)
                 (char-upcase c1))))

(define (polymer-react/full data)
  (let loop ((s (string->list data))
             (len (string-length data)))
    (set! s (fold (lambda (c rest)
                    (if (and (pair? rest)
                             (is-pair? c (car rest)))
                        (cdr rest)
                        (cons c rest)))
                  '() s))
    (let ((new-len (length s)))
      (if (= new-len len)
          (apply string s)
          (loop s new-len)))))

(define (solve-part1 data)
  (string-length (polymer-react/full data)))

(define (remove-char s n)
  (let ((char (integer->char (+ n (char->integer #\a)))))
    (irregex-replace/all (irregex char 'case-insensitive) s)))

(define (solve-part2 data)
  (let* ((baseline (polymer-react/full data))
         (shorts (map (lambda (n)
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
