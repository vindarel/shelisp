;;;; shelisp: unix shell interface for CommonLisp
;;;; Copyright (c) 2003-2006 Alexandru Dan Corlan MD PhD (http://dan.corlan.net)
;;;;

;;;; This program is free software; you can redistribute it and/or modify
;;;; it under the terms of the Lisp Lesser General Public License version 2, as published by
;;;; the Free Software Foundation and with the following preamble:
;;;; http://opensource.franz.com/preamble.html (permalink: http://www.webcitation.org/63hwrOQRA)

;;;; This program is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;; Lisp Lesser General Public License for more details.



;;;; HISTORY
;;;; created: july 4, 2006, released under the GPL (GNU General Public Licence)
;;;; v2:      august 20, 2006
;;;;          #[ ]# syntax for template strings; eval expressions cand
;;;;          be added with ?expr
;;;;          read-preserving-whitespace now used in '?', cleaner results
;;;;          wrappers for: tex gs ls pwd mktemp dirname basename md5sum
;;;;          recursive version of md5sum that also works for directories
;;;; v2.1:    august 14, 2007
;;;;          sbcl compatibility with conditional compilation

;;;; v3.0:    december 5, 2011
;;;;          rereleased under the LLGPL
;;;;          bash-result, parse exit value of last command
;;;;          nogui, simple menu
;;;;          wget-test, test that an url exists

;;;; v3.1:    december 6, 2011
;;;;          merged clisp port contributed by Kaz (@Kylheku.com)

;;;; v3.2:    december 7, 2011
;;;;          the sh macro added
;;;; v..      january, 2018
;;;;          copied to github, small fixes.

(provide "shelisp")

(defvar *shelisp-temp-directory* "/tmp/"
  "Directory where all temporary files are created.")

;; the following two defvar's are just reminders for what to do.

(defvar *expr-escape-chars* "?"
  "Not yet used definition of the string that introduces expressions
   to be evaluated and returned in scripts.")

(defvar *expr-escape-escape-chars* "\\?"
  "Not yet used definition of the string that prevents the
   introduction of expressions to be evaluated and returned in scripts.")

(defun script (str &key (program "/bin/bash") (options ()))
  "Execute the STR string as a script of the program, with the eventual options,
   and return the standard-output of the command as a string."
  #+(or sbcl cmu)
  (with-output-to-string (so)
    (with-input-from-string (si str)
      (#+sbcl sb-ext:run-program
              #+cmu extensions:run-program
              (format nil "~A" program)
              options
              :input si :output so)
      so))
  #+clisp
  (multiple-value-bind (out-stream)
      (ext:run-program
       (format nil "~A" program)
       :arguments `("-c" ,str ,@options)
       :output :stream)
    (unwind-protect
         (progn
           (with-output-to-string (so)
             (loop for ch = (read-char out-stream nil nil nil)
                while ch
                do (write-char ch so))))
      (close out-stream))))

(defun lowersym (sym)
  (if (symbolp sym)
      (format nil "~(~a~)" sym)
      sym))

(defun exparg (a)
  (if (and (symbolp a) (eql #\- (elt (symbol-name a) 0)))
           (lowersym a) ;(list 'quote a)
           a))

(defmacro sh (name &rest args)
  (list 'script
    (list 'format 'nil "~A ~{\"~A\" ~}"
       (lowersym name) ;(list 'quote name)
       (list 'quote (mapcar #'eval (mapcar #'exparg args))))))

(defun bash-result (str)
  "Runs the commands from the str through bash appending a print
   of the result of the last command. Reads only this result
   and returns it as an integer."
  (let* ((res (script (concatenate 'string str (format nil "~%echo $?~%"))))
	 (ptr (1- (1- (length res)))))
    (loop
       (if (or (eql ptr 0) (eql (elt res ptr) #\Newline))
           (with-input-from-string (s res :start ptr) (return (read s)))
           (decf ptr)))))

(defun mixed-script (&rest strlist)
  "Execute the STR string as a script of the program, with the eventual options,
   and return the standard-output of the command as a string."
  (let ((evs (apply #'concatenate
		    (cons 'string
			  (mapcar #'(lambda (x)
				      (format nil "~A" x))
				  strlist)))))
    (script evs :program "/bin/bash" :options nil)))

(defun mixed-template (&rest strlist)
  "Execute the STR string as a script of the program, with the eventual options,
   and return the standard-output of the command as a string."
  (let ((evs (apply #'concatenate
		    (cons 'string
			  (mapcar #'(lambda (x)
				      (format nil "~A" x))
				  strlist)))))
    evs))

(defun lines-to-list (text)
  "Transform the string TEXT into a list of strings, each representing
   on line of TEXT. This is suitable to postprocessing the standard output
   of many Unix commands (such as find or df) that return one result
   per line."
  (let ((po (position #\Newline text)))
    (if (not po)
        (if (zerop (length text))
            nil
            (list text))
        (cons (subseq text 0 po)
              (lines-to-list (subseq text (1+ po)))))))

(defun read-script-list (str end-char)
  "This is the 'delayed' read macro, that provides an
   expression that will be evaluated at eval time."
  (do ((bu (make-string 100000 :initial-element #\Space))
       ;; bu should be smaller and growing
       (bul 0)
       (mixl nil))
      (nil)
    (setf (elt bu bul) (read-char str))
    (cond
      ((eql (elt bu bul) end-char)
       (if (or (eql bul 0) (not (eql (elt bu (1- bul)) #\\)))
           (progn
             (setf mixl (cons (subseq bu 0 bul) mixl))
             (return-from read-script-list (reverse mixl)))
           (setf (elt bu (1- bul)) end-char)))
      ((eql (elt bu bul) #\?)
       (if (or (eql bul 0) (not (eql (elt bu (1- bul)) #\\)))
           (let ((form  (read-preserving-whitespace str)))
             (setf mixl (cons (subseq bu 0 bul) mixl))
             (setf mixl (cons form mixl))
             (setf bul 0))
           (setf (elt bu (1- bul)) #\?)))
      (t (incf bul)))))

(defun read-template-list (str end-char-1 end-char-2)
  " This is the 'delayed' read macro, that provides an
    expression that will be evaluated at eval time.
    problem: must ungetc the space after the ? expression! "
  (do ((bu (make-string 100000 :initial-element #\Space))
       (bul 0)
       (mixl nil))
      (nil)
    (setf (elt bu bul) (read-char str))
    (cond
      ((and (> bul 2) (eql (elt bu bul) end-char-2) (eql (elt bu (1- bul)) end-char-1))
       (setf mixl (cons (subseq bu 0 (1- bul)) mixl))
       (return-from read-template-list (reverse mixl)))
      ((eql (elt bu bul) #\?)
       (if (or (eql bul 0) (not (eql (elt bu (1- bul)) #\\)))
           (let ((form  (read-preserving-whitespace str)))
             (setf mixl (cons (subseq bu 0 bul) mixl))
             (setf mixl (cons form mixl))
             (setf bul 0))
           (setf (elt bu (1- bul)) #\?)))
      (t (incf bul)))))

(defun read-script-line (str end-char)
  "This is the 'direct' read macro, that executes the embedded
   expressions at read time."
  (do ((bu (make-string 1000 :initial-element #\Space))
       (bul 0))
      (nil)
    (setf (elt bu bul) (read-char str))
    (cond
      ((eql (elt bu bul) end-char)
       (if (or (eql bul 0) (not (eql (elt bu (1- bul)) #\\)))
           (return-from read-script-line (subseq bu 0 bul))
           (setf (elt bu (1- bul)) end-char)))
      ((eql (elt bu bul) #\?)
       (if (or (eql bul 0) (not (eql (elt bu (1- bul)) #\\)))
           (let ((form (format nil "~A" (eval (read-preserving-whitespace str)))))
             (replace bu form :start1 bul)
             (incf bul (length form)))
           (setf (elt bu (1- bul)) #\?)))
      (t (incf bul)))))

(defun enter-shell-mode (stream)
  "Read and execute successive shell commands, with eventual
   lisp expressions embedded. Expressions are evaluated at
   read time, as soon as a line is delivered. Implements the !! macro."

  (do () (nil)
    (princ "$ " *standard-output*)
    (let ((ll (read-script-line stream #\Newline)))
      (when (and (> (length ll) 1) (string= (subseq ll 0 2) "!!"))
	(return-from enter-shell-mode))
      (princ (script ll)))))

(defun simple-shell-escape-reader (stream char)
  (declare (ignore char))
  (let ((ll (read-script-line stream #\Newline)))
    (when (and (> (length ll) 0) (string= (subseq ll 0 1) "!"))
      (enter-shell-mode stream)
      (return-from simple-shell-escape-reader))
    (princ (script ll)))
  nil)

(set-macro-character #\! #'simple-shell-escape-reader)

(defun embedded-shell-escape-reader (stream char)
    (declare (ignore char))
    (cons 'mixed-script (read-script-list stream #\] )))

(set-macro-character #\[ #'embedded-shell-escape-reader)

(defun template-escape-reader (stream char1 char2)
    (declare (ignore char1 char2))
    (cons 'mixed-template (read-template-list stream #\] #\#)))

(set-dispatch-macro-character #\# #\[ #'template-escape-reader)

(defun storable-template-escape-reader (stream char1 char2)
    (declare (ignore char1 char2))
    (list 'quote (cons 'mixed-template (read-template-list stream #\} #\#))))

(set-dispatch-macro-character #\# #\{ #'storable-template-escape-reader)


;;;; Specific interface to individual Unix commands
;;;;
;;;; The purpose of this interface is to arrive to a stable method of
;;;; access to unix command
;;;; functionality; once it's stabilised, further optimisations will become
;;;; practical.

(declaim #+sbcl(sb-ext:muffle-conditions style-warning))

(defun ls (&optional (pattern "") &key (options "") (dir "."))
  "Return a list of strings representing the file names from the DIR directory
   matching [in the Unix glob way] the specified PATTERN string."
  (lines-to-list (script (format nil "cd ~A~%ls ~A ~A~%" dir options pattern))))

(declaim #+sbcl(sb-ext:unmuffle-conditions style-warning))

;(defun grep (&key (options "") (pattern "") (files ()))
;  "Broken: Return a list of strings representing the file names from the DIR directory
;   matching [in the Unix glob way] the specified PATTERN string."
;  (lines-to-list (script (format nil "cd ~A~%ls ~A ~A~%" dir options pattern)))
;  )

(defun pwd ()
  "Return the string with the current directory (as returned by
   the Unix command pwd)."
  ;; remove end of line
  (first (lines-to-list (script "pwd"))))

(defun md5sum (file)
  "Return the md5sum of the argument FILE as a 32 character string.
   The argument must be a single file, otherwise NIL is returned.
   If it is a directory, :DIR is returned."
  (let ((rl (lines-to-list (script (format nil "md5sum ~A" file)))))
    (when (> (length rl) 1)
      (return-from md5sum nil))
    (when (equal (subseq (first rl) 0 5) "error")
      (return-from md5sum :dir))
    (when (< (length (first rl)) 32)
      (return-from md5sum nil))
    (subseq (first rl) 0 32)))

(defun dir-md5sum (dir &key (verbose nil))
  "Calculate the md5sum of a directory DIR by feeding through md5sum the
   sorted list of md5sums of each file (or directory) found in DIR.
   If a stream is given for VERBOSE, a list of all md5sums in the directory
   is printe on the stream."
  (let* ((ll (ls "" :dir dir))
	 (md5list
	  (mapcar
	   #'(lambda (f l)
	       (if (eq f :dir)
		   (dir-md5sum (concatenate 'string dir "/" l))
                   f))
	   (mapcar
	    #'(lambda (fn)
		(md5sum (concatenate 'string dir "/" fn))) ll)
	   ll)))
    (if verbose
	(mapcar #'(lambda (s l)
		    (format verbose "~A ~A/~A~%" s dir l))
		md5list ll))
    (first
     (lines-to-list
      (script (format nil "~{~S~%~}" (sort md5list #'string<))
	      :program "/usr/bin/md5sum")))))

(defun mktemp (pattern)
  "Makes a temporary filename starting from PATTERN, than must contain
   6 X characters (like: XXXXXX) and returns it as a string."
  (first (lines-to-list (script (format nil "mktemp ~A" pattern)))))

(defun basename (path &optional (termination nil))
  "Calls basename with PATH and eventually TERMINATION"
  (if termination
      (script (format nil "basename ~A ~A" path termination))
      (script (format nil "basename ~A" path))))

(defun dirname (path)
  "Calls dirname."
  (script (format nil "dirname ~A" path)))

(defun clear-screen ()
  "Clear terminal screen."
  (format *standard-output* (script (format nil "clear"))))

(defun make-temp-file ()
  "Simple to invoke call to mktemp with defaults for shelisp."
  (mktemp (concatenate 'string *shelisp-temp-directory* "shelisp-XXXXXX")))

(defun gs (ps &key (result-type 'png) (driver 'png256)
                (output-file nil) (crop t) (resolution 150)
                (comment "Created by SHELISP with GS."))
  "Runs gs on the PS string, that must be a correctly formatted PostScript
   program. Returns a bitmap file as a string or sends it to OUTPUT-FILE if
   specified. Normally, the imaged is cropped (unless CROP is set to NIL)
   and COMMENT is inserted as a comment. RESOLUTION can be set and also
   the gs devise set with DRIVER and the resulting image format with RESULT-TYPE."
  (let* ((tempname (make-temp-file))
	 (result tempname))
    (with-open-file (pso (concatenate 'string tempname ".ps") :direction :output)
      (format pso "~A" ps))
    (script
     (format nil
	     "gs -q -sDEVICE=~(~A~) -r~A -sOutputFile=~A.~(~A~) ~A.ps </dev/null"
	     driver resolution tempname result-type
	     tempname))
    (if crop
	(script
	 (format nil "convert ~A.~(~A~) -crop 0x0 -comment \"~A\" ~A:~A.crop ; mv ~A.crop ~A.~(~A~)"
		 tempname result-type comment result-type
		 tempname tempname tempname result-type)))
    (if output-file
	(script (format nil "cat ~A.~(~A~) >~A" tempname result-type output-file))
        (setf result (script (format nil "cat ~A.~(~A~)" tempname result-type))))
    (script (format nil "rm ~A*" tempname))
    result))

(defun tex (tex &key (output-file nil) (latex nil) (result-type 'pdf))
  "Runs TeX on the TEX script, which is a string,
   and returns the content of the resulting file
   as a string, unless OUTPUT-FILE is given, when the result
   is copied (with cat) into a new file with that name.

   The result can be of types DVI, PS or PDF. You can run LaTeX
   instead of TeX by specifying :LATEX T."
  (let* ((tempname (make-temp-file))
	 (result tempname))
    (with-open-file (texo (concatenate 'string tempname ".tex") :direction :output)
      (format texo "~A" tex))
    (cond
      ((and (eq result-type 'pdf) (not latex))
       (script (format nil "cd ~A ; pdftex ~A.tex" *shelisp-temp-directory* tempname)))
      ((and (eq result-type 'dvi) (not latex))
       (script (format nil "cd ~A ; tex ~A.tex" *shelisp-temp-directory* tempname)))
      ((and (eq result-type 'ps) (not latex))
       (script (format nil "cd ~A ; tex ~A.tex ; dvips -V ~A -o ~A.ps"
                       *shelisp-temp-directory* tempname tempname tempname)))
      ((and (eq result-type 'pdf) latex)
       (script (format nil "cd ~A ; pdflatex ~A.tex" *shelisp-temp-directory* tempname)))
      ((and (eq result-type 'dvi) latex)
       (script (format nil "cd ~A ; latex ~A.tex" *shelisp-temp-directory* tempname)))
      ((and (eq result-type 'ps) latex)
       (script (format nil "cd ~A ; latex ~A.tex ; dvips -V ~A -o ~A.ps"
                       *shelisp-temp-directory* tempname tempname tempname)))
      (t (error "RESULT-TYPE must be one of: PDF, PS or DVI.")))
                                        ; check for processing error here
    (if output-file
	(script (format nil "mv ~A.~(~A~) ~A" tempname result-type output-file))
        (setf result (script (format nil "cat ~A.~(~A~)" tempname result-type))))

    (script (format nil "rm ~A*" tempname))
    result))


(defun test-gs-and-tex ()
  "Test function to produce an image hello.png in the current directory,
   that shows the words 'hello world' typeset by TeX."
  (gs (tex #[ hello world \footline{\hfill}\end ]# :result-type 'ps)
      :output-file "hello.png"))

(defun wget-test (url)
  "Returns T if the page is there and 0 otherwise."
  (equal (elt (script (format nil "wget -q --spider ~A ; echo $?" url)) 0) #\0))

(defun whoami ()
  "Return a symbol that is the user name."
  (intern
   (string-upcase
    (string-trim '(#\Space #\Tab #\Newline) (script "/usr/bin/whoami")))))

(defun file-equal (f1 f2)
  "Return NIL if content of files f1 and f2 differs in any way and T otherwise.
   Also returns NIL if any of the files does not exist. (but instead should throw
   something in that case)"
  (eql 0 (length (script (format nil "/usr/bin/diff ~a ~a" f1 f2)))))

;;;; NoGui -- character based interactive interface for common lisp programs
;;;  data structure <nogui> ::= (<title> <option>*)
;;;                 <title> ::= <string>
;;;                 <option> ::= (<character> <string>) | (<string>) | (character <nogui>)
;;;                 <question> ::= <string>

(defvar so *standard-output*)

(defvar testnogui nil)

(setf testnogui
  '(menu "test input list"
    (#\a "option a")
    (#\b "option b")
;    "..."
    (#\z "option z")
    (#\0 "cancel")))

(defun query-user (nogui &key (linestart "    ") (dialog :tty))
  "Display a nogui, read and return user input. "
  (case dialog
    (:dialog
     (return-from query-user
       (elt (script (format nil "Xdialog --stdout --menu ~S 25 80 20 ~:{ ~A ~S~}~%"
			    (cadr nogui) (cddr nogui)))
	    0)))
    (:tty
     (clear-screen)
     (format so "~|~A~%~%" (cadr nogui))
     (let ((chlist nil))
       (dolist (lio (cddr nogui))
         (if (stringp lio)
             (format so "~A~A~%" linestart lio)
             (progn
               (format so "~A~c -- ~A~%" linestart (car lio) (cadr lio))
               (setf chlist (cons (car lio) chlist)))))
       (loop
          (let ((ch (read-char)))
            (when (member ch chlist)
              (return ch))))))))
