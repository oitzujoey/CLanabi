
;; (defun eat-whitespace (stream)
;;   (do ((char (read-char stream) (read-char stream)))
;; 	  ((not (char= char #\Space))
;; 	   nil)))


(defvar *hanabi-reader-callbacks* '())

;; This is the Common Lisp reader macro for Hanabi. Other reader readers will be used by it.
;; TODO: Macro should only return after a single form has been read.
(defun hanabi-reader (stream sub-char numarg)
  (declare (ignore sub-char numarg))
  (let (chars)
	;; Get source
	(do ((char (read-char stream) (read-char stream)))
		((and (char= char #\%)))
	  (push char chars))
	(let ((source (coerce (nreverse chars) 'string))
		  ast)
	  ;; Call readers
	  (do ((reader *hanabi-reader-callbacks* (cdr reader)))
		  ((or ast (null reader)) ast)
		(with-input-from-string (stream source)
		  (setf ast (funcall (car reader) stream))))
	  ast)))


;;; Hanabi readers ;;;

;; Readers must be added in order, otherwise you get problems. It's a lot like the duck-lisp reader.
;; Readers return an AST.

(setf *hanabi-reader-callbacks* '())

;; (defun hanabi-expression (string)
;;   (do ((char ))))
;; (push #'hanabi-expression *hanabi-reader-callbacks*)

;; Defines a Hanabi reader and adds it to the reader list.
(defmacro def-hanabi-reader (name stream &rest body)
  `(progn
	 (defun ,name ,stream
	   ,@body)
	 (push #',name *hanabi-reader-callbacks*)))

(def-hanabi-reader hanabi-integer-reader (stream)
  (let ((int 0))
	(do ((char (read-char stream nil 'eof) (read-char stream nil 'eof)))
		((not (characterp char)))
	  (when (not (digit-char-p char))
		(return-from hanabi-integer-reader nil))
	  (setf int (+ (* int 10) (digit-char-p char))))
	int))

(def-hanabi-reader hanabi-string-reader (stream)
  (with-output-to-string (out-stream)
	(peek-char t stream)
	(let (char)
	  (setf char (read-char stream))
	  (unless (char= #\" char)
		(return-from hanabi-string-reader nil))
	  (do ((char (read-char stream nil 'eof) (read-char stream nil 'eof)))
		  ((and (or (not (characterp char)) (char= #\" char)) (characterp char)) nil)
		(when (not (characterp char))
		  (return-from hanabi-string-reader nil))
		(write-char char out-stream)))))

;; And… Execute!
(set-dispatch-macro-character #\# #\h #'hanabi-reader)
