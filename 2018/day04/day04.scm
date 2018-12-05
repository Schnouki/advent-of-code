#!/usr/bin/csi -script

(define TEST-INPUT "[1518-11-01 00:00] Guard #10 begins shift
[1518-11-01 00:05] falls asleep
[1518-11-01 00:25] wakes up
[1518-11-01 00:30] falls asleep
[1518-11-01 00:55] wakes up
[1518-11-01 23:58] Guard #99 begins shift
[1518-11-02 00:40] falls asleep
[1518-11-02 00:50] wakes up
[1518-11-03 00:05] Guard #10 begins shift
[1518-11-03 00:24] falls asleep
[1518-11-03 00:29] wakes up
[1518-11-04 00:02] Guard #99 begins shift
[1518-11-04 00:36] falls asleep
[1518-11-04 00:46] wakes up
[1518-11-05 00:03] Guard #99 begins shift
[1518-11-05 00:45] falls asleep
[1518-11-05 00:55] wakes up")

(import (chicken io))
(import (chicken irregex))
(import (chicken sort))
(import (chicken string))
(import defstruct)
(import srfi-1)
(import srfi-69)

(defstruct shift (guard-id "") (sleeps '()) (wakeups '()))

(define (shift-minutes-asleep s)
  (fold + 0
        (map (lambda (pair) (- (second pair) (first pair)))
             (zip (map string->number (shift-sleeps s))
                  (map string->number (shift-wakeups s))))))

(define (parse-input text)
  (let ((lines (sort (string-split text "\n") string<?))
        (records (make-hash-table))
        (guard-id ""))
    (for-each
     (lambda (line)
       (let* ((m (irregex-search "\\[\\d+-(\\d+-\\d+) (\\d+):(\\d+)\\]" line))
              (ldate (irregex-match-substring m 1))
              (lhour (irregex-match-substring m 2))
              (lminute (irregex-match-substring m 3))
              (record (hash-table-ref/default records ldate (make-shift))))
         (cond ((begin (set! m (irregex-search "Guard #(\\d+) begins shift" line)) m)
                (set! guard-id (irregex-match-substring m 1)))
               ((and (string=? lhour "00")
                     (irregex-search "falls asleep" line))
                (shift-guard-id-set! record guard-id)
                (shift-sleeps-set! record (cons lminute (shift-sleeps record))))
               ((and (string=? lhour "00")
                     (irregex-search "wakes up" line))
                (shift-guard-id-set! record guard-id)
                (shift-wakeups-set! record (cons lminute (shift-wakeups record)))))
         (hash-table-set! records ldate record)))
     lines)
    records))

(define (find-guard-most-asleep records)
  (let ((h (make-hash-table)))
    (for-each (lambda (pair)
                (hash-table-set! h (car pair)
                                 (+ (cdr pair)
                                    (hash-table-ref/default h (car pair) 0))))
              (hash-table-map records
                              (lambda (k v)
                                (cons (shift-guard-id v) (shift-minutes-asleep v)))))
    (car (sort (hash-table->alist h)
               (lambda (a b) (> (cdr a) (cdr b)))))))

(define (find-guard-most-asleep-minute records guard-id)
  (let ((recs (filter (lambda (s) (string=? (shift-guard-id s) guard-id))
                      (hash-table-values records)))
        (minutes (make-hash-table)))
    (for-each (lambda (s)
                (for-each (lambda (pair)
                            (for-each (lambda (minute)
                                        (hash-table-set! minutes minute
                                                         (+ 1 (hash-table-ref/default minutes minute 0))))
                                      (iota (- (second pair) (first pair)) (first pair))))
                          (zip (map string->number (shift-sleeps s))
                               (map string->number (shift-wakeups s)))))
              recs)
    (let ((max-minutes (hash-table-fold minutes (lambda (key val acc) (max acc val)) -1)))
      (car (find (lambda (pair) (= (cdr pair) max-minutes))
                 (reverse (hash-table->alist minutes)))))))

(define (solve-part1 input)
  (let* ((records (parse-input input))
         (most-asleep-guard (car (find-guard-most-asleep records)))
         (most-asleep-minute (find-guard-most-asleep-minute records most-asleep-guard)))
    (* (string->number most-asleep-guard)
       most-asleep-minute)))

(print "Part 1")
(print "======")
(print "TEST " (solve-part1 TEST-INPUT))
(print "INPUT " (solve-part1 (with-input-from-file "input" read-string)))
