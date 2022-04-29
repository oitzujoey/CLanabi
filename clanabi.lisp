
;; Usage: (L body* \(WHILE condition body*\)*)
(defmacro l (&rest body)
  (let ((body-copy body)
		new-body
		(label-loop (gensym))
		(label-end (gensym)))
	(do ((element (car body-copy) (car body-copy)))
		((null body-copy) nil)
	  (cond
		((and (symbolp element) (eq element 'while))
		 (setf body-copy (cdr body-copy))
		 (setf element (car body-copy))
		 (when (null element)
		   (error "`while' requires a non-nil argument"))
		 (push `(unless ,element
				  (go ,label-end))
			   new-body))
		((and (symbolp element) (eq element 'until))
		 (setf body-copy (cdr body-copy))
		 (setf element (car body-copy))
		 (when (null element)
		   (error "`until' requires a non-nil argument"))
		 (push `(when ,element
				  (go ,label-end))
			   new-body))
		(t (push element new-body)))
	  (setf body-copy (cdr body-copy)))
	`(tagbody
		,label-loop
		,@(nreverse new-body)
		(go ,label-loop)
		,label-end
	  nil)))

;; Usage: (WHILE condition body*)
(defmacro while (condition &rest body)
  `(do () ((not ,condition) nil)
	 ,@body))



;; (defun eat-whitespace (stream)
;;   (do ((char (read-char stream) (read-char stream)))
;; 	  ((not (char= char #\Space))
;; 	   nil)))

(defstruct reader-status
  ast
  success)

;; This is the Common Lisp reader macro for Hanabi. Other reader readers will be used by it.
;; TODO: Macro should only return after a single form has been read.
(defun hanabi-reader (stream sub-char numarg)
  (declare (ignore sub-char numarg))
  ;; Order matters. Readers higher in the list will be called first.
  (let ((readers '(hanabi-end-reader))
		(hstring (make-array 0
							 :element-type 'character
							 :fill-pointer 0
							 :adjustable t))  ; This is the "string" that the stream is stored in.
		(hpos 0)  ; This is the hstring index.
		(hlength 0)  ; This is the farthest we have consumed so far.
		)
	;; Oh boy! Here come the closures!
	;; This is probably a bad idea, but let's learn by doing.
	(labels ((hstream (command &rest args)
			   ;; Take that OO! Oh wait.
			   (case command
				 (:save hpos)
				 (:restore
				  (let ((new-pos (nth 0 args)))
					(if (and (< new-pos hlength) (>= new-pos 0))
						(setf hpos new-pos)
						(error "hstream→:restore: Position out of range"))))
				 (:next
				  (let (char)
					(if (< hpos hlength)
						(progn
						  (setf char (elt hstring hpos))
						  (incf hpos))
						(progn
						  (setf char (read-char stream nil 'eof))
						  (print char)
						  (unless (eq char 'eof)
							(vector-push-extend char hstring)
							(incf hlength)
							(incf hpos))))
					char))
				 (:try-readers
				  (let ((readers (nth 0 args))
						position
						ret)
					(setf position (hstream :save))
					(l
					 while readers
					 (setf ret (funcall (car readers) #'hstream))
					 until (reader-status-success ret)
					 (pop readers)
					 (hstream :restore position))
					(if readers
						(reader-status-ast ret)
						nil)))
				 (otherwise (error "hstream: Key absent or unrecognized")))))
	  (hstream :try-readers readers))))

;; (defun gotoend-reader (hstring)
;;   (l
;;    (setf char (funcall hstream :next))
;;    until (char= char #%)))

;;; Hanabi readers ;;;

;; Readers must be added in order, otherwise you get problems. It's a lot like the duck-lisp reader.
;; Readers return an AST.

(defun hanabi-end-reader (hstream)
  (make-reader-status :ast nil
					  :success (let (char)
								 (l
								  (setf char (funcall hstream :next))
								  while (characterp char)
								  until (char= char #\%))
								 t)))

;; (def-hanabi-reader hanabi-integer-reader (stream)
;;   (let ((int 0))
;; 	(do ((char (read-char stream nil 'eof) (read-char stream nil 'eof)))
;; 		((or (not (characterp char)) (char= char #\ )))
;; 	  (when (not (digit-char-p char))
;; 		(return-from hanabi-integer-reader nil))
;; 	  (setf int (+ (* int 10) (digit-char-p char))))
;; 	int))

;; (def-hanabi-reader hanabi-string-reader (stream)
;;   (with-output-to-string (out-stream)
;; 	(peek-char t stream)
;; 	(let (char)
;; 	  (setf char (read-char stream))
;; 	  (unless (char= #\" char)
;; 		(return-from hanabi-string-reader nil))
;; 	  (do ((char (read-char stream nil 'eof) (read-char stream nil 'eof)))
;; 		  ((and (or (not (characterp char)) (char= #\" char)) (characterp char)) nil)
;; 		(when (not (characterp char))
;; 		  (return-from hanabi-string-reader nil))
;; 		(write-char char out-stream)))))

;; (def-hanabi-reader hanabi-comment-reader (stream)
;;   (with-output-to-string (out-stream)
;; 	(peek-char t stream)
;; 	(let (char)
;; 	  (setf char (read-char stream))
;; 	  (unless (char= #\" char)
;; 		(return-from hanabi-string-reader nil))
;; 	  (do ((char (read-char stream nil 'eof) (read-char stream nil 'eof)))
;; 		  ((and (or (not (characterp char)) (char= #\" char)) (characterp char)) nil)
;; 		(when (not (characterp char))
;; 		  (return-from hanabi-string-reader nil))
;;		(write-char char out-stream)))))

;; And… Execute!
(set-dispatch-macro-character #\# #\h #'hanabi-reader)
