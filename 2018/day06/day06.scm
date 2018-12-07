#!/usr/bin/csi -script

(define TEST-INPUT "1, 1
1, 6
8, 3
3, 4
5, 5
8, 9")

(import (chicken io))
(import (chicken string))
(import srfi-1)

(define (parse-input data)
  (map (lambda (line)
         (let ((pair (string-split line ", ")))
           (cons (string->number (car pair)) (string->number (cadr pair)))))
       (string-split data "\n")))

(define (min-x points)
  (apply min (map car points)))
(define (max-x points)
  (apply max (map car points)))
(define (min-y points)
  (apply min (map cdr points)))
(define (max-y points)
  (apply max (map cdr points)))

(define (dist point1 point2)
  (+ (abs (- (car point1) (car point2)))
     (abs (- (cdr point1) (cdr point2)))))

(define (find-closest seeds point)
  "Find which seed the point is the closest from."
  (let* ((distances (map (lambda (seed) (dist point seed)) seeds))
         (min-dist (apply min distances))
         (find-dist (lambda (d) (= d min-dist)))
         (nb-dists (count find-dist distances)))
    (if (= nb-dists 1)
        ;; Only 1: the seed!
        (list-index find-dist distances)
        ;; Several matches: too bad
        #f)))

(define (solve-part1 seeds)
  "Compute the size of the areas for each seed, ignoring infinite areas."
  ;; Inifite areas are guessed by examining which area "touch" an edge of the
  ;; grid.
  (let* ((width (+ 2 (max-x seeds)))
         (height (+ 2 (max-y seeds)))
         (find-seed
          (lambda (c)
            (find-closest seeds (cons (remainder c width)
                                      (quotient c width)))))
         (grid (list->vector (map find-seed (iota (* width height)))))
         (grid-ref (lambda (c) (vector-ref grid c)))
         (infinites (delete-duplicates
                     (append
                      (map grid-ref (iota width))
                      (map grid-ref (iota width (* height (- width 1))))
                      (map grid-ref (iota height 0 width))
                      (map grid-ref (iota height (- width 1) width)))))
         (counts (make-vector (length seeds) 0)))
    ;; (print "GRID")
    ;; (for-each (lambda (y)
    ;;             (print (subvector grid (* width y) (* width (+ 1 y)))))
    ;;           (iota height))
    ;; (print "INIFINITES " infinites)
    (for-each (lambda (a)
                (when a
                  (vector-set! counts a
                               (+ 1 (vector-ref counts a)))))
              (vector->list grid))
    (apply max
           (map third
                (filter (lambda (z) (not (member (car z) infinites)))
                        (zip (iota (length seeds))
                             seeds
                             (vector->list counts)))))))

(define (solve-part2 seeds max-dist)
  "Compute the size of the region with a total distance of less than MAX-DIST."
  (let* ((width (+ 2 (max-x seeds)))
         (height (+ 2 (max-y seeds)))
         (sum-dist (lambda (c)
                     (let* ((point (cons (remainder c width)
                                        (quotient c width)))
                            (dist-seed (lambda (seed) (dist seed point))))
                       (apply + (map dist-seed seeds))))))
    (length (filter (lambda (sd) (< sd max-dist))
                    (map sum-dist (iota (* width height)))))))



(let ((test-data (parse-input TEST-INPUT))
      (input-data (parse-input (string-chomp (with-input-from-file "input" read-string)))))
  (print "Part 1")
  (print "======")
  (print "TEST  " (solve-part1 test-data))
  (print "INPUT " (solve-part1 input-data))

  (print)
  (print "Part 2")
  (print "======")
  (print "TEST  " (solve-part2 test-data 32))
  (print "INPUT " (solve-part2 input-data 10000)))
