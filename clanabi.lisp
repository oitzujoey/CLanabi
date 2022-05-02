

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

(defun lassoc (item lalist)
  (cadr (assoc item lalist)))


;;; Hanabi top-level reader ;;;


(defstruct reader-status
  ast
  success)

(defvar hanabi-special-characters '(#\( #\) #\{ #\}))

(defvar hanabi-safe-expression-readers '(hanabi-expression-reader
										 hanabi-scope-reader
										 hanabi-quote-reader
										 hanabi-string-reader
										 hanabi-integer-reader
										 hanabi-callback-reader
										 hanabi-symbol-reader))

(defvar hanabi-expression-readers '(hanabi-expression-reader
									hanabi-scope-reader
									hanabi-quote-reader
									hanabi-string-reader
									hanabi-integer-reader
									hanabi-callback-reader
									hanabi-forth-reader
									hanabi-symbol-reader))

(defvar hanabi-whitespace-readers '(hanabi-whitespace-reader))

(defvar hanabi-function-info '((print 1)
							   (+     2)
							   (list  2)
							   (def   3)))

(defmacro def (name args body)
  `(progn
	 (defun ,name ,args ,body)
	 (setf hanabi-function-info (append (list (list ',name ,(length args))) hanabi-function-info))))

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
					(if ret
						ret
						(make-reader-status :ast nil :success nil))))
				 (otherwise (error "hstream: Key absent or unrecognized")))))
      (reader-status-ast (hstream :try-readers hanabi-expression-readers)))))


;;; Hanabi readers ;;;


(defun hanabi-expression-reader (hstream)
  (let (char
		success
		ret
		wret
		ast)
	(setf char (funcall hstream :next))
	(when (and (characterp char) (char= char #\())
	  (funcall hstream :try-readers hanabi-whitespace-readers)
	  (setf ret (funcall hstream :try-readers hanabi-safe-expression-readers))
	  (push (reader-status-ast ret) ast)
	  (when (reader-status-success ret)
		(setf wret (funcall hstream :try-readers hanabi-whitespace-readers))
		(when (reader-status-success wret)
		  (l
		   (setf ret (funcall hstream :try-readers hanabi-expression-readers))
		   (push (reader-status-ast ret) ast)
		   while (reader-status-success ret)
		   (setf wret (funcall hstream :try-readers hanabi-whitespace-readers))
		   while (reader-status-success wret)
		   until (char= (funcall hstream :peek) #\)))))
	  (setf char (funcall hstream :next))
	  (when (and (characterp char) (char= char #\)))
		(setf success t)))
	(make-reader-status :ast (nreverse ast)
						:success success)))

(defun hanabi-scope-reader (hstream)
  (let (char
		success
		ret
		wret
		ast)
	(setf char (funcall hstream :next))
	(when (and (characterp char) (char= char #\{))
	  (push 'progn ast)
	  (funcall hstream :try-readers hanabi-whitespace-readers)
	  (l
	   (setf ret (funcall hstream :try-readers hanabi-expression-readers))
	   (push (reader-status-ast ret) ast)
	   while (reader-status-success ret)
	   (setf wret (funcall hstream :try-readers hanabi-whitespace-readers))
	   while (reader-status-success wret)
	   until (char= (funcall hstream :peek) #\}))
	  (setf char (funcall hstream :next))
	  (when (and (characterp char) (char= char #\}))
		(setf success t)))
	(make-reader-status :ast (nreverse ast)
						:success success)))

(defun hanabi-forth-reader (hstream)
  (let (success
		ast
		ret
		(args-left 0))
	(setf ret (funcall hstream :try-readers hanabi-safe-expression-readers))
	(when (reader-status-success ret)
	  (when (symbolp (reader-status-ast ret))
		(setq args-left (lassoc (reader-status-ast ret) hanabi-function-info)))
	  (when (not (null args-left))
		(push (reader-status-ast ret) ast)
		(l
		 while (and (not (null args-left)) (> args-left 0))
		 (setf ret (funcall hstream :try-readers hanabi-whitespace-readers))
		 while (reader-status-success ret)
		 (setf ret (funcall hstream :try-readers hanabi-expression-readers))
		 (push (reader-status-ast ret) ast)
		 while (reader-status-success ret)
		 (decf args-left))
		(unless (> args-left 0)
		  (setf success t))))
	(make-reader-status :ast (nreverse ast)
						:success success)))

(defun hanabi-callback-reader (hstream)
  (let (char
		(string (make-array 0
							:element-type 'character
							:fill-pointer 0
							:adjustable t))
		found-char
		ast)
	(setf char (funcall hstream :next))
	(when (and (characterp char) (char= char #\#))
	  (l
	   (setf char (funcall hstream :peek))
	   while (and (characterp char)
				  (not (or (member char hanabi-special-characters)
						   (whitespacep char))))
	   (funcall hstream :next)
	   (setf found-char t)
	   (vector-push-extend char string))
	  (setf ast (read-from-string (coerce string 'string))))
    (make-reader-status :ast `(function ,ast)
						:success found-char)))

(defun hanabi-string-reader (hstream)
  (let (char
		success
		(string (make-array 0
							:element-type 'character
							:fill-pointer 0
							:adjustable t))
		ast)
    (setf char (funcall hstream :next))
    (when (and (characterp char) (char= char #\"))
	  (l
	   (setf char (funcall hstream :next))
	   while (and (characterp char) (not (char= char #\")))
	   (vector-push-extend char string))
	  (setf ast (coerce string 'string))
	  (setf success (if (characterp char)
						t
						nil)))
    (make-reader-status :ast ast
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
		(string (make-array 0
							:element-type 'character
							:fill-pointer 0
							:adjustable t))
		found-char)
    (l
     (setf char (funcall hstream :peek))
     while (and (characterp char)
				(not (or (member char hanabi-special-characters)
						 (whitespacep char))))
     (funcall hstream :next)
     (setf found-char t)
     (vector-push-extend char string))
    (make-reader-status :ast (read-from-string (coerce string 'string))
						:success found-char)))

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
		found-comment)
    (setf char (funcall hstream :next))
    (when (and (characterp char) (char= char #\/))
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
		   (setf found-comment t)))))
    (make-reader-status :ast nil
						:success found-comment)))

(defun hanabi-whitespace-reader (hstream)
  (let (char
		ret
		found-char)
    (l
	 (l
	  (setf char (funcall hstream :peek))
	  while (whitespacep char)
	  (funcall hstream :next)
	  (setf found-char t))
	 (setf ret (funcall hstream :try-readers '(hanabi-comment-reader)))
	 while (reader-status-success ret)
	 (setf found-char t))
    (make-reader-status :ast nil
						:success found-char)))

;; And… Execute!
(set-dispatch-macro-character #\# #\h #'hanabi-reader)
