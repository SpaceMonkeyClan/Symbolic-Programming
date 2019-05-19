; Lisp Implimentation
(defun abstract-sort (fn &optional l)
  (if (null l)
      #'(lambda (l) (funcall fn l))
      (funcall fn l)))

(defun sorted-p (l)
  (every #'<= l (cdr l)))

;; Bubble-sort

(defun bubble (l)
  (if (or (null l) (null (cdr l)))
      l
      (if (< (car l) (cadr l))
	  (cons (car l) (bubble (cdr l)))
	  (cons (cadr l) (bubble (cons (car l) (cddr l)))))))

(defun bubble-sort (l)
  (if (or (null l) (null (cdr l))) l
      (if (sorted-p l) l
	  (bubble-sort (bubble l)))))

(defparameter *sort* (abstract-sort #'bubble-sort))

(print (funcall *sort* '(12 3 5))) ;=> (3 5 12)
(print (funcall *sort* '(1 6 9 3 7 5))) ;=> (1 3 5 6 7 9)
(print (abstract-sort #'bubble-sort '(1 6 9 3 7 5))) ;=> (1 3 5 6 7 9)

;; Insertion-sort

(defun insert (l e)
  (if (or (null l) (<= e (car l)))
      (cons e l)
      (cons (car l) (insert (cdr l) e))))

(defun insertion-sort (l)
  (if (null l) 
    l
    (insert (insertion-sort (cdr l)) (car l))))

(defparameter *sort* (abstract-sort #'insertion-sort))

(print (funcall *sort* '(12 3 5))) ;=> (3 5 12)
(print (funcall *sort* '(1 6 9 3 7 5))) ;=> (1 3 5 6 7 9)
(print (abstract-sort #'insertion-sort '(1 6 9 3 7 5))) ;=> (1 3 5 6 7 9)

;; Selection-sort

(defun selection-sort (l)
  (if (not (null l))
      (let ((m (reduce #'min l)))
	(cons m (selection-sort (remove m l))))))

(defparameter *sort* (abstract-sort #'selection-sort))

(print (funcall *sort* '(12 3 5))) ;=> (3 5 12)
(print (funcall *sort* '(1 6 9 3 7 5))) ;=> (1 3 5 6 7 9)
(print (abstract-sort #'selection-sort '(1 6 9 3 7 5))) ;=> (1 3 5 6 7 9)

; prolog implimentation


abstract_sort(Method, List, SortedList) :-
    call(Method, List, SortedList).

%% Bubble Sort
swap([X1, X2| Xs], [X2, X1| Xs]) :- X1 > X2, !.
swap([X|Xs], [X|Ys]) :- swap(Xs, Ys).

bubble_sort(L, S) :-
    swap(L, L1), !,
    bubble_sort(L1, S).
%% Base Case when Bubble Sort 
bubble_sort(L, L).

%% "Assigning" sort1 to abstract_sort(bubble_sort). 
sort1(List, SortedList) :- abstract_sort(bubble_sort, List, SortedList).

%% sort1([5, 3, 12], S). %% returns: S = [3, 5, 12].
%% sort1([5, 3, 12, 3, 100, 1, 5, 1], S). %% S = [1, 1, 3, 3, 5, 5, 12, 100].

%% Insertion Sort
insert(X, [], [X]) :- !.
insert(X, [X1|Xs], [X, X1|Xs]) :-  X1 >= X, !.
insert(X, [X1|Xs], [X1|L]) :- insert(X, Xs, L).

insertion_sort([], []) :- !.
insertion_sort([X|Xs], S) :-
    insertion_sort(Xs, S1),
    insert(X, S1, S).

sort2(List, SortedList) :- abstract_sort(insertion_sort, List, SortedList).
%% sort2([5, 3, 12], S). %% S = [3, 5, 12].
%% sort2([5, 3, 12, 3, 100, 1, 5, 1], S). %% S = [1, 1, 3, 3, 5, 5, 12, 100].

selection_sort([], []).
selection_sort(L, [M|S]) :-
    min_list(L, M),
    select(M, L, L1),
    selection_sort(L1, S).

sort3(List, SortedList) :- abstract_sort(selection_sort, List, SortedList).

%% sort3([5, 3, 12], S). %% returns: S = [3, 5, 12].
%% sort3([5, 3, 12, 3, 100, 1, 5, 1], S). %% S = [1, 1, 3, 3, 5, 5, 12, 100] 
