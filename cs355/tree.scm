#lang sicp

(define (insert e t)
  (cond ((null? t) (list e '() '()))
        ((= e (datum t)) t) ; no duplicates allowed
        ((< e (datum t)) (make-tree (datum t)
                                    (insert e (left-tree t))
                                    (right-tree t)))
        (else (make-tree (datum t)
                         (left-tree t)
                         (insert e (right-tree t))))))

(define (make-tree datum left right)
  (list datum left right))

;; Accessor
(define (datum t) (car t))
(define (left-tree t) (cadr t))
(define (right-tree t) (caddr t))

(define t (make-tree 4 '() '()))
(define t1 (insert 6 (insert 3 (insert 5 t))))


;; Checking for the presence of an element in the Tree!
(define (elem? e t)
  (cond ((null? t) #f)
        ((= (datum t) e) #t)
        ((< e (datum t)) (elem? e (left-tree t)))
        (else (elem? e (right-tree t)))))

;; Inorder Traversal of the Tree!
(define (inorder-try t)
  (if (null? t)
      nil
      (cons (inorder-try (left-tree t))
            (cons (datum t) (inorder-try (right-tree t))))))


;; Properly formatted inorder traversal of a tree!
(define (concat l1 l2)
  (if (null? l1)
      l2
      (cons (car l1) (concat (cdr l1) l2))))

(define (inorder t)
  (if (null? t)
      nil
      (concat (inorder (left-tree t))
              (concat (list (datum t))
                      (inorder (right-tree t))))))

(define (list2tree l)
  (define (l2r-iter t l)
    (if (null? l)
        t
        (l2r-iter (insert (car l) t) (cdr l))))
  (l2r-iter (make-tree (car l) nil nil) (cdr l)))

(define (tree-sort l)
  (inorder (list2tree l)))

(define l0 (list 89 23 13 833 81 34 11 392 183 355 782))
