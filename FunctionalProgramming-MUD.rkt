#lang racket
(require srfi/1)
(require srfi/13)
(require srfi/48)

;; items in the game
(define objects '((1 "a silver dagger")
                  (1 "a gold coin"))) 

;; world map
(define descriptions '((1 "You are in the lobby")
                       (2 "You are in the hallway")
                       (3 "You are in a swamp")
                       (4 "You are at fire exit")
                       (5 "You are at the street")))


;; define actions that are possible in rooms
(define look '(((directions) look) ((look) look) ((examine room) look)))
(define pick '(((get) pick) ((pickup) pick) ((pick) pick)))
(define put '(((put) drop) ((drop) drop) ((place) drop) ((remove) drop )))
(define inventory '(((inventory) inventory) ((bag) inventory)))
(define quit '(((exit game) quit) ((quit game) quit) ((exit) quit) ((quit) quit)))
(define jump '(((jump) jump)))
(define actions `(,@look ,@pick ,@put ,@jump ,@inventory ,@quit))

;; directions map
(define decisiontable `((1 ((north) 2) ((north west) 3) ,@actions)
                        (2 ((south) 1) ((north) 4) ,@actions)
                        (3 ,@actions)
                        (4 ((north) 5) ,@actions)
                        (5 ((south) 4) ,@actions)
                        ))

;; code to keep items in pockets

;; items in the room
(define objectdb (make-hash))
;; items in the possesion
(define inventorydb (make-hash))
;; function to add an item to hash table
(define (add-object db id object)
  (if (hash-has-key? db id)
      (let ((record (hash-ref db id)))
        (hash-set! db id (cons object record)))
      (hash-set! db id (cons object empty))))
;; function to add all items from objects list to hash table
(define (add-objects db)
  (for-each
   (lambda (r)
     (add-object db (first r) (second r))) objects))
;; put all items from objects list to objectdb (the database of all items in the room)
(add-objects objectdb)

;; remove an item from a room
;; you can't just remove an item to nowhere, so add it to invevtory
(define (remove-object-from-room db id str)
  ;; check whether hash table has an item with this key
  (when (hash-has-key? db id)
    (let* ((record (hash-ref db id))
           (result (remove (lambda (x) (string-suffix-ci? str x)) record))
           (item (lset-difference equal? record result)))
      (cond ((null? item)
             (printf "I don’t see that item in the room!\n"))
            (else
             (printf "Added ~a to your bag.\n" (first item))
             (add-object inventorydb 'bag (first item))
             (hash-set! db id result ))))))

;; remove an item from inventory and put it to a room
(define (remove-object-from-inventory db id str)
  ;; check whether hash table has an item with this key
  (when (hash-has-key? db 'bag)
    (let* ((record (hash-ref db 'bag))
           (result (remove (lambda (x) (string-suffix-ci? str x)) record))
           (item (lset-difference equal? record result)))
      (cond ((null? item)
             (printf "You are not carrying that item!\n"))
            (else
             (printf "Removed ~a from your bag.\n" (first item))
             (add-object objectdb id (first item))
             (hash-set! db 'bag result))))))

;; display objects in db (either room or inventory)
;; function improved to show empty room or bag
(define (display-objects db id)
  (if (hash-has-key? db id)
      (let* ((record (hash-ref db id))
             (output (string-join record " and ")))
        (if (not (equal? output ""))
            (if (eq? id 'bag )
                (printf "You are carrying ~a.\n" output)
                (printf "You can see ~a.\n" output))
            (if (eq? id 'bag )
                (printf "Your bag is empty.\n")
                (printf "You can see no items.\n"))
            ))
      (if (eq? id 'bag )
          (printf "Your bag is empty.\n")
          (printf "You can see no items.\n"))
      ))

;; pick up an item
(define (pick-item id input)
   (let ((item (string-join (cdr (string-split input)))))
      (remove-object-from-room objectdb id item)))
;; drop an item
(define (put-item id input)
   (let ((item (string-join (cdr (string-split input)))))
      (remove-object-from-inventory inventorydb id item)))
;; display what do we have in posession
(define (display-inventory)
   (display-objects inventorydb 'bag))

;; convert list of symbols to string
(define (slist->string l)
  (string-join (map symbol->string l)))

;; function prints possible exits from a room (if any)
(define (get-directions id)
  (let ((record (assq id decisiontable)))
    (let* ((result (filter (lambda (n) (number? (second n))) (cdr record)))
           (n (length result)))
      (cond ((= 0 n)
             (printf "You appear to have entered a room with no exits.\n"))
            ((= 1 n)
             (printf "You can see an exit to the ~a.\n" (slist->string (caar result))))
            (else
             (let* ((losym (map (lambda (x) (car x)) result))
                    (lostr (map (lambda (x) (slist->string x)) losym)))
               (printf "You can see exits to the ~a.\n" (string-join lostr " and "))))))))

;; find item (eq?) from assqlist by key "id" and return it cdr
(define (assq-ref assqlist id)
  (cdr (assq id assqlist)))
;; find item (eqv?) from assqlist by key "id" and return it cdr
(define (assv-ref assqlist id)
  (cdr (assv id assqlist)))
;; find description of a room by id
(define (get-response id)
  (car (assq-ref descriptions id)))
;; get actions for room #id
(define (get-keywords id)
  (let ((keys (assq-ref decisiontable id)))
    (map (lambda (key) (car key)) keys)))


;; function accepts keylist (list of lists of symbols) and tokenlist
;; and returns list of numbers with keylist length where every item is number showing how similar every possible action to entered input
;; example:
;; '((road) (directions) (look) (examine room) (get) (pickup) (pick) (put) (drop) (place) (remove) (inventory) (bag) (exit game) (quit game) (exit) (quit))
;; '(exit quit)
;; '(0 0 0 0 0 0 0 0 0 0 0 0 0 1/2 1/2 1 1)
;; outputs a list in the form: (0 0 0 2 0 0)
(define (list-of-lengths keylist tokens)
  (map 
   (lambda (x)
     (let ((set (lset-intersection eq? tokens x)))
       ;; apply some weighting to the result
       (* (/ (length set) (length x)) (length set))))
   keylist))

;; return position of largest number in a numbers list
(define (index-of-largest-number list-of-numbers)
  ;; find largest num
  (let ((n (car (sort list-of-numbers >))))
    (if (zero? n)
      #f
      ;; find num index in list-of-numbers (zero-based)
      (list-index (lambda (x) (eq? x n)) list-of-numbers))))

;; find possible action which looks more similar to input then others
(define (lookup id tokens)
  (let* ((record (assv-ref decisiontable id))
         (keylist (get-keywords id))
         (index (index-of-largest-number (list-of-lengths keylist tokens))))
    (if index 
      (cadr (list-ref record index))
      #f)))

;; main game loop
(define (startgame initial-id)
  (let loop ((id initial-id) (description #t))
    ;; print description?
    (when description
      ;; print room description
      (printf "~a\n" (get-response id)) 
      ;; what objects are in the room?
      (display-objects objectdb id))
    ;; input prompt
    (printf "> ")
    ;; read input
    (let* ((input (read-line))
           ;; split input to list of string by space
           (string-tokens (string-tokenize input))
           ;; and convert it to list of symbols
           (tokens (map string->symbol string-tokens)))
      (let ((response (lookup id tokens)))
        (cond ((number? response)
               (loop response #t))
              ((eq? #f response)
               (format #t "huh? I didn't understand that!\n")
               (loop id #f))
              ((eq? response 'look)
               (get-directions id)
               (loop id #t))
              ((eq? response 'pick)
               (pick-item id input)
               (loop id #f))
              ((eq? response 'drop)
               (put-item id input)
               (loop id #f))
              ((eq? response 'jump)
               (format #t "You jumped high.\n")
               (loop id #f))
              ((eq? response 'inventory)
               (display-inventory)
               (loop id #f))
              ((eq? response 'quit)
               (format #t "So Long, and Thanks for All the Fish...\n")
               (exit)))))))

;(startgame 1)