

;;; Helper functions ;;;


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

(defun whitespacep (char)
  (member char '(#\  #\Tab #\Return #\Newline)))


;;; Hanabi top-level reader ;;;


(defstruct reader-status
  ast
  success)

(defvar hanabi-top-level-readers '(hanabi-quote-reader
				   hanabi-string-reader
				   hanabi-integer-reader
				   hanabi-symbol-reader))

(defvar hanabi-expression-readers '(hanabi-quote-reader
				    hanabi-string-reader
				    hanabi-integer-reader
				    hanabi-symbol-reader))

(defvar hanabi-whitespace-readers '(hanabi-whitespace-reader
				    hanabi-comment-reader))

;; This is the Common Lisp reader macro for Hanabi. Other reader readers will be used by it.
;; TODO: Macro should only return after a single form has been read.
(defun hanabi-reader (stream sub-char numarg)
  (declare (ignore sub-char numarg))
  ;; Order matters. Readers higher in the list will be called first.
  (let ((hstring (make-array 0
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
		 (:peek
		  (let (char)
		    (if (< hpos hlength)
			(setf char (elt hstring hpos))
			(progn
			  (setf char (peek-char nil stream nil 'eof))))
		    char))
		 (:next
		  (let (char)
		    (if (< hpos hlength)
			(progn
			  (setf char (elt hstring hpos))
			  (incf hpos))
			(progn
			  (setf char (read-char stream nil 'eof))
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
			ret
			nil)))
		 (otherwise (error "hstream: Key absent or unrecognized")))))
      (reader-status-ast (hstream :try-readers hanabi-top-level-readers)))))

;;; Hanabi readers ;;;

(defun hanabi-end-reader (hstream)
  (make-reader-status :ast nil
		      :success (let (char)
				 (l
				  (setf char (funcall hstream :next))
				  while (characterp char)
				  until (char= char #\%))
				 t)))

(defun hanabi-string-reader (hstream)
  (let (char
	success
	(string (make-array 0
			    :element-type 'character
			    :fill-pointer 0
			    :adjustable t)))
    (setf char (funcall hstream :next))
    (if (and (characterp char) (eq char #\"))
	(progn
	  (l
	   (setf char (funcall hstream :next))
	   while (and (characterp char) (not (eq char #\")))
	   (vector-push-extend char string))
	  (setf success (if (characterp char)
			    t
			    nil)))
	(setf success nil))
    (make-reader-status :ast (coerce string 'string)
			:success success)))

(defun hanabi-integer-reader (hstream)
  (let (char
	success
	(integer 0)
	found-digit)
    (l
     (setf char (funcall hstream :peek))
     while (and (characterp char) (digit-char-p char))
     (funcall hstream :next)
     (setf found-digit t)
     (setf integer (+ (* 10 integer) (digit-char-p char))))
    (setf success found-digit)
    (make-reader-status :ast integer
			:success success)))

(defun hanabi-symbol-reader (hstream)
  (let (char
	success
	(string (make-array 0
			    :element-type 'character
			    :fill-pointer 0
			    :adjustable t))
	found-char)
    (l
     (setf char (funcall hstream :peek))
     while (and (characterp char)
		(not (char= char #\ ))
		(not (char= char #\))))
     (funcall hstream :next)
     (setf found-char t)
     (vector-push-extend char string))
    (setf success found-char)
    (make-reader-status :ast (read-from-string (coerce string 'string))
			:success success)))

(defun hanabi-quote-reader (hstream)
  (let (char
	success
	ast
	ret)
    (setf char (funcall hstream :next))
    (when (char= char #\')
      (setf ret (funcall hstream :try-readers hanabi-expression-readers))
      (setf success (reader-status-success ret))
      (setf ast `(quote ,(reader-status-ast ret))))
    (make-reader-status :ast ast
			:success success)))

(defun hanabi-comment-reader (hstream)
  (let (char
	success)
    (setf char (funcall hstream :next))
    (when (and (characterp char) (eq char #\/))
      (setf char (funcall hstream :next))
      (when (characterp char)
	(case char
	  (#\/
	   (l
	    (setf char (funcall hstream :peek))
	    while (and (characterp char)
		       (not (or (eq char #\Linefeed)
				(eq char #\Return))))
	    (funcall hstream :next))
	   (setf success t)))))
    (make-reader-status :ast nil
			:success success)))

(defun hanabi-whitespace-reader (hstream)
  (let (char
	found-char)
    (l
     (setf char (funcall hstream :peek))
     while (whitespacep char)
     (funcall hstream :next)
     (setf found-char t))
    (make-reader-status :ast nil
			:success found-char)))

;; And… Execute!
(set-dispatch-macro-character #\# #\h #'hanabi-reader)
