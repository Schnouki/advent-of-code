#!/usr/bin/csi -script

(define TEST-INPUT "Step C must be finished before step A can begin.
Step C must be finished before step F can begin.
Step A must be finished before step B can begin.
Step A must be finished before step D can begin.
Step B must be finished before step E can begin.
Step D must be finished before step E can begin.
Step F must be finished before step E can begin.")

(import (chicken io))
(import (chicken sort))
(import (chicken string))
(import srfi-1)

(define (parse-input data)
  (map (lambda (line)
         (let ((words (string-split line)))
           (cons (car (string->list (list-ref words 1)))
                 (car (string->list (list-ref words 7))))))
       (string-split (string-chomp data) "\n")))

(define-type STATE char)
(define-type STATES (list-of STATE))
(define-type RULE (pair STATE STATE))
(define-type RULES (list-of RULE))
(: all-states (RULES --> STATES))
(define (all-states rules)
  "Build a list of all the possible states from the list of rules."
  (sort (delete-duplicates
         (append-map (lambda (r) (list (car r) (cdr r)))
                     rules))
        char<?))

(: find-state (RULES STATES --> (or STATE false)))
(define (find-state rules completed)
  "Find an available state given the rules and a list of completed states."
  (let* ((states (all-states rules))
         (state-complete? (lambda (s) (member s completed)))
         (rules-for-state (lambda (s) (filter (lambda (rule) (char=? (cdr rule) s))
                                              rules)))
         (reqs-for-state (lambda (s) (map car (rules-for-state s))))
         (next-states (sort (filter (lambda (s)
                                      (every state-complete? (reqs-for-state s)))
                                    (filter (lambda (s) (not (state-complete? s)))
                                            states))
                            char<?
                            )))
    (if (null-list? next-states)
        #f
        (car next-states))))

(define (solve-part1 rules)
  (reverse-list->string
   (let loop ((completed '()))
     (let ((next (find-state rules completed)))
       (if next
           (loop (cons next completed))
           completed)))))

(let ((test-data (parse-input TEST-INPUT))
      (input-data (parse-input (with-input-from-file "input" read-string))))
  (print "Part 1")
  (print "======")
  (print "TEST  " (solve-part1 test-data))
  (print "INPUT " (solve-part1 input-data)))
