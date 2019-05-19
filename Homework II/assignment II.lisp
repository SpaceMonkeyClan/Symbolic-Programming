; CompSci 3520 - Assignment No. II
; Author: Dena, Rene
; Last Modified: 3/14/19



; Exercise I _________________________________________________________

(defun occurrences (lst)
  (let ((res (mapcar #'(lambda (elt) (cons elt (count elt lst))) (remove-duplicates lst))))
    (sort res #'> :key #'cdr)))

(occurrences '(a b a d a d)) ; yeilds: ((A. 3) (D . 2) (B . 1))

; Exercise II ________________________________________________________

;; a.) Function using itteration
(defun posPlus-iterative (l)
  (loop for elm in l
     for ind from 0
     collect (+ elm ind)))

(print (posPlus-iterative '(7 5 1))) ; yeilds: (7 6 3)

;; b.) Function using recursion
(defun posPlus-recursive (l &optional (index 0))
  (if (null l)
      'nil
      (cons (+ (car l) index)
	    (posPlus-recursive (cdr l) (1+ index)))))

(print (posPlus-recursive '(7 5 1))) ; yeilds: (7 6 3)

; Exercise III _______________________________________________________

(defparameter arr
  (make-array '(3 3) :initial-contents '((1 2 3) (4 5 6) (7 8 9))))

(defun rotate-arr (a)
  "Takes a 2d array and rotates it by 90degrees clock wise and then returns the result as a string"
  (loop for r below (array-dimension a 0)
      collect (reverse (loop for c below (array-dimension a 1)
			   collect (aref a c r)))))

(print (rotate-arr arr)) ; yeilds: ((7 4 1) (8 5 2) (9 6 3))

; Exercise IV ________________________________________________________

;; a.)
(defun assoc-list-to-hashtable (assoc-list)
  "Takes an assoc list and returns a hashtable"
  (let ((out (make-hash-table)))
    (loop for i in assoc-list
	do (setf (gethash (car i) out) (cdr i)))
    out))

;; b.)
(defun hashtable-to-assoc-list (hash-table)
  "Takes a hash map and returns a list of dotted pairs"
  (reverse
   (let ((out '()))
     (maphash (lambda (k v) (setf out (cons (cons k v) out))) hash-table)
     out)))

(defparameter h (make-hash-table))
(setf (gethash '1 h) 'a)
(setf (gethash '2 h) 'b)
(setf (gethash '3 h) 'c)

(defun display-hashtable (h)
  (print (maphash (lambda (k v) (format t "~%~a => ~a" k v)) h)))

(display-hashtable h)
(display-hashtable (assoc-list-to-hashtable (hashtable-to-assoc-list h)))
