#|
  This file is a part of Grantha project.
  Copyright (c) 2020 Edgard Bikelis (bikelis@gmail.com)

  Author: Edgard Bikelis (bikelis@gmail.com)

  Grantha is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or (at
  your option) any later version.

  Grantha is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with Grantha.  If not, see <https://www.gnu.org/licenses/>.
|#

(in-package :cl-user)

(defpackage grantha  
  (:use :cl)
  (:nicknames :gra)
  (:export alias
	   list-exported-symbols-with-docstrings
	   get-even-elements
	   get-odd-elements
	   make-plist
	   last-member
	   reverse-cons
	   tree-remove
	   nested-getf
	   ;; truncate-memory
	   ;; truncate-time
	   ;; round-to
	   pretty-number
	   human-readable-number
	   print-bin
	   print-hex
	   convert
	   subtract-time-of-day
	   time-to-execute-and-result
	   parse-time
	   current-date-string
	   int-to-char
	   char-to-int
	   int-to-str
	   str-to-int
	   next
	   previous
	   str
	   concat
	   padding
	   ;; from grantha-dependent.lisp
	   garbage-full
	   kill-all-threads-but-current
	   url-encode
	   url-decode))

(in-package :grantha)



;;
;; symbols
;;

;; (defmacro alias (new-name prev-name)
;;   "Makes an alias named NEW-NAME for PREV-NAME."
;;   `(defmacro ,new-name (&rest args)
;;      `(,',prev-name ,@args)))

(defun alias (quoted-new-name quoted-prev-name)
  "Makes an alias named QUOTED-NEW-NAME for QUOTED-PREV-NAME."
  (setf (symbol-function quoted-new-name) (symbol-function quoted-prev-name)))




;;
;; packages
;;


;; see https://stackoverflow.com/questions/9743056/common-lisp-exporting-symbols-from-packages
(defun list-exported-symbols-with-docstrings (package)
  "I return a simple definition list in org-mode with the exported
symbols of a package. I should do that in the order they are defined
in the source code, but I do not."
  (let ((out ""))
    (do-external-symbols (p package)
      (if (ignore-errors (symbol-function p))
	  (setq out (concat out *newline* "- " (symbol-name p) " (function) :: "
			    (coerce (subst #\space
					   #\Newline
					   (coerce (documentation p 'function)
						   'list))
				    'string))))
      (if (ignore-errors (symbol-value p))
	  (setq out (concat out *newline* "- " (symbol-name p) " (variable) :: "
			    (coerce (subst #\space
					   #\Newline
					   (coerce (documentation p 'variable)
						   'list))
				    'string)))))
    out))


;;
;; lists
;;

;; https://stackoverflow.com/a/36353587
(defun get-even-elements (lst)
  "Give me a list and I shall return thee its even elements."
  (loop :for element :in (rest lst) :by #'cddr :collect element))

(defun get-odd-elements (lst)
  "Give me a list and I shall return thee its odd elements."
  (loop :for element :in lst :by #'cddr :collect element))

;; inspired by /usr/share/sbcl-source/src/code/list.lisp, `pairlis'
(defun make-plist (keys data &optional append-to-car-p)
  "Construct a property list from KEYS and DATA."
  (let (out)
    (do ((x keys (cdr x))
	 (y data (cdr y)))
	((and (endp x) (endp y)) out)
      (if (or (endp x) (endp y))
          (error "The lists of keys and data are of unequal length."))
      (if append-to-car-p
	  (setq out (append (list (car x) (car y)) out))
	  (setq out (append out (list (car x) (car y))))))))

(defun last-member (lst)
  "I return the actual last member of a list."
  (car (last lst)))

(defun reverse-cons (cns)
  "I reverse a cons pair."
  (cons (cdr cns) (car cns)))

;; from https://github.com/briangu/OPS5/blob/master/src/ops-util.lisp
;; circa 1992
(defun tree-remove (element tree &key (test #'equal))
  "TREE-REMOVE is a function which deletes every occurrence
   of ELEMENT from TREE. This function was defined because Common Lisp's
   REMOVE function only removes top level elements from a list."
  (when tree
    (if (funcall test element (car tree)) 
	(tree-remove element (cdr tree) :test test)
	(cons (car tree)
	      (tree-remove element (cdr tree) :test test)))))


;;
;; association lists, alists
;;

(defun alist-remove-by-car (the-car alist)
  "I am not destructive.

(alist-remove-by-car 1 '((1 . \"a\") (2 . \"b\")))
→ ((2 . \"b\"))"
  (remove the-car alist :key #'car :test #'equal))


;;
;; property lists, plists
;;
(defun nested-getf (place &rest indicators)
  "(nested-getf a b c) → (getf (getf a b) c)."
  (let (out)
    (setq out place)
    (dolist (indicator indicators)
      (setq out (getf out indicator)))
    out))




;;
;; numbers
;;


;; lossy : /
;;
;; Heavily inspired by Roy Anderson, on lisp-hug@lispworks.com
(defun truncate-memory (bytes)
  "Give me some bytes, and them I shall return thee divided by the
maximum possible unit, together with that unit."
  (let* ((kb (truncate bytes 1024))
	 (mb (truncate kb 1024))
	 (gb (truncate mb 1024))
	 (tb (truncate gb 1024))
	 (pb (truncate tb 1024)))
    (cond ((zerop kb) (list bytes "B"))
	  ((zerop mb) (list kb "KB"))
	  ((zerop gb) (list mb "MB"))
	  ((zerop tb) (list gb "GB"))
	  ((zerop pb) (list tb "TB"))
	  (t (list pb "PB")))))

;; Heavily inspired by Roy Anderson, on lisp-hug@lispworks.com
(defun truncate-time (s)
    "Give me some seconds, and them I shall return thee divided by the
maximum possible unit, together with that unit."
  (let* ((min (truncate s 60))
	 (hours (truncate min 60))
	 (days (truncate hours 24))
	 (years (truncate days 365))
	 (millenia (truncate years 1000)))
    (cond ((zerop min) (list s "sec"))
	  ((zerop hours) (list min "min"))
	  ((zerop days) (list hours "hours"))
	  ((zerop years) (list days "days"))
	  ((zerop millenia) (list days "years"))
	  (t (list days "millenia")))))

(defun round-to (number precision &optional (what #'round))
  "I round NUMBER to PRECISION."
    (let ((div (expt 10 precision)))
         (/ (funcall what (* number div)) div)))

;; TODO: check serapeum:file-size-human-readable

(defun pretty-number (n)
  "I truncate the bytes you give me. I am more expensive than `truncate-memory'."
  (declare (integer n))
  (let ((parsed (cond ((< n 1024) (cons n              "B" ))
		      ((< n 1048576) (cons (float (/ n 1024)) "K"))
		      ((< n 1073741824) (cons (float (/ n 1048576)) "M"))
		      ((< n 1099511627776) (cons (float (/ n 1073741824)) "G"))
		      ((< n 1125899906842624) (cons (float (/ n 1099511627776)) "T"))
		      ((< n 1152921504606846976) (cons (float (/ n 1125899906842624)) "P"))
		      (t "out of bounds"))))
    (if (typep parsed 'simple-array)
	parsed
	(concatenate 'string (str (float (round-to (car parsed) 1))) (cdr parsed)))))

(defparameter +number-suffixes+
  (list (list 10 1024 "b")
	(list 20 1048576 "kb")
	(list 30 1073741824 "mb")
	(list 40 1099511627776 "gb")
	(list 50 1125899906842624 "tb")
	(list 60 1152921504606846976 "pb"))
    "A list of 2^ to what gives each unit, followed by the unit itself,
and the short string for it.")

;; FIX
(defun human-readable-number (number)
  (let (out)
    (dolist (each +number-suffixes+)
      (if (and (not out) (> number 1) (< number (expt 2 (car each))))
	  (progn (setq number (/ number (expt 2 (car each))))
		 (setq out (cons (float number) (cdr each))))))
    out))

(defun print-bin (n)
  "I print the given number in a binary representation."
  (format nil "~b" n))

(defun print-hex (n)
  "I print the given number in a hexadecimal representation."
  (format nil "~x" n))




;;
;; conversion of units
;;

(defparameter *equivalences*
  '(((foot . metre) . 0.3048)
    ((centimetre . inch) . 0.39370078740157))
  "I hold equivalences, probably for `convert' to use.")

(defun convert (quantity before after)
  "I try converting the given quantity from the before unit to the
after unit. I am not smart. Check `*equivalences*' to see all I know."
  (let (conversion-pair equivalence invertedp)
    (setq conversion-pair (cons before after))
    (setq equivalence (assoc conversion-pair *equivalences* :test 'equal))
    (unless equivalence
      (setq equivalence (assoc (reverse-cons conversion-pair) *equivalences* :test 'equal))
      (setq invertedp t))
    (if equivalence
	(if invertedp
	    (/ quantity (cdr equivalence))
	    (* quantity (cdr equivalence)))
	(error "I do not know how to convert this!"))))




;;
;; time
;;

(defun subtract-time-of-day (x y)
  "I get two times in the format (seconds microseconds), returned by
`get-time-of-day' for instance, and return their difference."
  (let (seconds microseconds)
    (setq seconds      (- (nth 0 x) (nth 0 y)))
    (setq microseconds (- (nth 1 x) (nth 1 y)))
    (list seconds microseconds)))

(defun time-to-execute-and-result (form &optional multiple-value-p)
  "Returns the time (always in miliseconds?) to execute FORM, and its
result. In order to grab multiple values, the result might be returned
as a list. If we got an error, return a plist: (:error
'the-error-object).  NOTA BENE: If this is to be memoized, the object
will be gone eventually, so `princ-to-string' it."
  (let ((before-time (get-internal-real-time))
	result
	after-time)
    (setq result (handler-case (eval (if multiple-value-p
					 `(multiple-value-list ,form)
					 form))
		   (sb-c:compiler-error (the-error)
		     (list :error (slot-value the-error 'condition)))
		   (t (the-error)
    		     (list :error the-error))))
    (setq after-time (get-internal-real-time))
    (list :miliseconds (- after-time before-time) :result result)))

(defun parse-time (&optional (given-time (multiple-value-list (get-decoded-time))))
  "I either return thee the current `get-decoded-time' as a plist, or
I parse the time given in the same format, that thou providest me."
  (let ((keywords (list :second :minute :hour :day :month :year :day-of-week
				:daylight-savings-times-p :timezone)))
    (make-plist keywords given-time)))

(defun current-date-string (&key timep timezonep)
  "I return thee a string with the current date, and time if thou so
wishest, and likewise the timezone.  Cf. ISO 8601.  Time should be
separated with 'T' instead of '_', but Edgard thinks it less legible."
  (let ((time (parse-time))
	(date "")
	(hour&c "")
	(timezone ""))
    (setq date (concatenate 'string
			    ;; behold the bug of milleniumX10
			    (padding (str (getf time :year))  4 "0") "-"
			    (padding (str (getf time :month)) 2 "0") "-"
			    (padding (str (getf time :day))   2 "0")))   
    (when (or timep timezonep)
      (setq hour&c (concatenate 'string "_"
				(padding (str (getf time :hour))   2 "0") ":"
				(padding (str (getf time :minute)) 2 "0") ":"
				(padding (str (getf time :second)) 2 "0"))))
    (when timezonep
      (setq timezone (concatenate 'string
				  (if (> (getf time :timezone) 0)
				      "+"
				      "-")
				  (padding (str (getf time :timezone)) 2 "0")))
      (if (equal timezone "+00")
	  (setq timezone "Z")))
    (concatenate 'string date hour&c timezone)))


;; (eval `(encode-universal-time ,@(subseq (multiple-value-list (decode-universal-time (get-universal-time))) 0 6)))
;; → 3814968316
;; CL-USER> (get-universal-time)
;; → 3814968322



;;
;; characters
;;

(defun int-to-char (chr)
  "Give me either an int or a list of them, and I shall return them
converted to chars to thee. I am the contrary of `char-to-int'."
  (let ((lst '())
	(out '()))
    ;; if it’s just a number, add it to a list,
    ;; so DOLIST won’t fail
    (if (numberp chr)
	(setq lst (list chr))
	(setq lst chr))    
    (dolist (item lst)
      (setq out (push (code-char item) out)))
    ;; if we got just a number, return just it,
    ;; instead of a list
    (if (= (length out) 1)
	(car out)
	(reverse out))))

(defun char-to-int (chr)
  "Give me either a character or a list of them, and I shall return them
converted to ints to thee. I am the contrary of `int-to-char'."
  (let ((lst '())
	(out '()))
    (if (characterp chr)
	(setq lst (list chr))
	(setq lst chr))    
    (dolist (item lst)
      (setq out (push (char-code item) out)))
    (if (= (length out) 1)
	(car out)
	(reverse out))))

(defun int-to-str (chr)
  "Give me either an int or a list of them, and I shall return them
as a string, after converting them to characters. I am the contrary of `str-to-int'."
  (let ((out '()))
    (if (listp chr)
	(dolist (item chr)
	  (setq out (concatenate 'string out (string (code-char item)))))
	(setq out (string (code-char chr))))
  out))

(defun str-to-int (str)
  "Give me either a string or a list of them, and I shall return everything
as a list, after converting them to integers. I am the contrary of `int-to-str'."
  (let ((lst (coerce str 'list))
	(out '()))
    (dolist (item lst)
      (push (char-to-int (character item)) out))
    (if (= (length out) 1)
	(car out)
    (reverse out))))

(defun next (thing)
  "Give me something, like an integer, a character, or a string, and I
will return thee the next one of its kind. I am the contrary of `previous'."
  (typecase thing
    (number (+ thing 1))
    (character (int-to-char (+ (char-to-int thing) 1)))
    (string (if (= (length thing) 1)
		(int-to-str (+ (str-to-int thing) 1))
		(concatenate 'string
			     (subseq thing 0 (- (length thing) 1))
			     (next (subseq thing (- (length thing) 1)))))
    ;; (error "I don’t know quite what to do with this.")
     )))

(defun previous (thing)
  "Give me something, like an integer, a character, or a string, and I
will return thee the previous one of its kind. I am the contrary of `next'."
  (typecase thing
    (number (- thing 1))
    (character (int-to-char (- (char-to-int thing) 1)))
    (string (int-to-str (- (str-to-int thing) 1)))))




;;
;; strings
;;

(defvar *newline* (string #\newline))

(defun str (i)
  "I return i represented as a string."
  (princ-to-string i))

(defun concat (&rest items)
  "I try to be smart in concatenating items, silently converting them to strings."
  (let ((out ""))
    (dolist (item items)
      (if (or (numberp item) (consp item))
	  (setq out (concatenate 'string out (princ-to-string item)))
	  (setq out (concatenate 'string out item))))
    out))

(defun padding (str length pad &optional afterp)
  "Give me a string, the length it should have, and what to pad it
with, and I shall return thee the selfsame string thus padded."
  (setq str (coerce str 'list))
  (if (not (eq (length pad) 1))
      (error "The pad given isn’t of length 1."))
  (if (stringp pad)
      (setq pad (coerce pad 'character)))
  (loop while (< (length str) length) do
    (if afterp
	(setq str (append str (list pad)))
	(push pad str)))
  (coerce str 'string))

(defun string-to-octets (str)
  (flexi-streams:string-to-octets str))

(defun octets-to-string (str)
  (flexi-streams:octets-to-string str))


;;
;; base64 foolery
;;

(defun base64-encode-octets-to-string (str)
  "I am the contrary of `base64-encode-octets-to-string'.
(base64-encode-octets-to-string #(1 2 3 224 164 178))
→ \"AQID4KSy\"."
  (with-output-to-string (out)
    (s-base64:encode-base64-bytes str out nil)))

(defun base64-decode-string-to-octets (str)
  "I am the contrary of `base64-encode-string-to-octets'.
(base64-decode-string-to-octets \"AQID4KSy\")
→ #(1 2 3 224 164 178)."
  (with-input-from-string (s str)
    (s-base64:decode-base64-bytes s)))

(defun base64-encode-string-to-string (str)
  "Weird, I know, but I am useful to hide control characters.
(base64-encode-string-to-string (concat \"a\" *newline* \"b\"))
→ \"YQpi\"."
  (with-output-to-string (out)
    (s-base64:encode-base64-bytes
     (flexi-streams:string-to-octets str :external-format :utf-8) out nil)))

(defun base64-decode-string-to-string (str)
  (flexi-streams:octets-to-string
   (with-input-from-string (s str)
     (s-base64:decode-base64-bytes s))))




;;
;; files
;;

(defun file-exists-p (file)
  "I check if `file' exists. I return NIL if not, the path if true."
  (directory file))

(defun write-file-as-sexp (thing file)
  "I write a THING into some FILE."
  (with-open-file (f file
		     :direction :output
		     :if-exists :supersede
		     :if-does-not-exist :create)
    ;; we change *print-pretty* to avoid useless newlines in the file
    (let ((*print-pretty* nil))
	  (write thing :stream f :escape t)))
  nil)

(defun read-file-as-sexp (file)
  "I READ something that was written into some FILE."
    (if (probe-file file)
	(with-open-file (f file
			   :direction :input
			   :if-does-not-exist nil)
	  (read f))
	(print "The file does not exist.")))

(defun file-append (thing file &optional (end-with-newline-p nil))
  (let (the-offset)
    (with-open-file (f file
		       :direction :output
		       :if-exists :append
		       :if-does-not-exist :create)
      (setq the-offset (file-length f))
      ;; we change *print-pretty* to avoid useless newlines in the file
      (write thing :stream f :escape t :pretty nil)
      (if end-with-newline-p
	  (terpri f)))
    (list :offset the-offset)))

(defun file-read-from-offset (file offset)
  (with-open-file (f file
		     :direction :input
		     :if-does-not-exist :error)
    (file-position f offset)
    (read-line f)))


(defun file-read-binary-with-offset (pathname begin end)
  "Read PATHNAME into a freshly allocated (unsigned-byte 8) vector."
  (alexandria:with-input-from-file (stream pathname :element-type '(unsigned-byte 8))
    (file-position stream begin)
    (coerce (loop for i from 0 to end
		  collect (read-byte stream))
	    'vector)))

;; while byte != 10 ;; *newline*


(defun file-write-octets (octets file)
  (alexandria:write-byte-vector-into-file octets
					  file
					  :if-exists :supersede
					  :if-does-not-exist :create))

(defun file-read-as-octets (file)
  (alexandria:read-file-into-byte-vector file))


;;
;; serialization / serialisation
;;

;; (defun serialize-as-octet=store (thing)
;;   "(store-as-octet-vector=store '(+ 1 1))
;; #(67 76 67 76 43 10 35 1 1 43 35 1 11 67 79 77 77 79 78 45 76 73 83 80 0 24 0 1
;;   1 0 24 0 1 1 1 41)"
;;   (flex:with-output-to-sequence (s) (cl-store:store thing s)))

(defun serialize-as-octet=encode (thing)
  (conspack:encode thing))

(defun serialize-as-octet=decode (thing)
  (conspack:decode thing))

(defun deserialize-file (file)
  (conspack:decode-file file))



;;
;; compression
;;

(defun compress-octets (octets)
  (salza2:compress-data octets 'salza2:gzip-compressor))

(defun decompress-octets (octets)
  (chipz:decompress nil 'chipz:gzip octets))


;;
;; memoization / memoisation
;;

(defun memoize=eval (sexp &optional multiple-value-p)
  "I eval SEXP and return a plist with :sexp, the time taken to
execute as :miliseconds, and the results as :result. If you want
multiple values to be grabbed, give me a true multiple-value-p."
  (let (result)
    (setq result (gra:time-to-execute-and-result sexp multiple-value-p))
    ;; if we got an error, print it so the result is more enlightening.
    (when (ignore-errors (gra:nested-getf result :result :error))
      (setf (getf (getf result :result) :error)
	    (princ-to-string (gra:nested-getf result :result :error))))
    (list :sexp sexp
	  :multiple-value-p multiple-value-p
	  :miliseconds (getf result :miliseconds)
	  :universal-time (get-universal-time)
	  :result (getf result :result))))

(defparameter *stash-path* "~/ksipra/stash/"
"We save the results of sexps in ‘stashes’, which are files with
related data, and for that we need a path to save them.  Not
everything is easily serialized, so beware.")

(defparameter *stash-key-hash-tables-alist* '()
  "I am a list composed of (\"stash\" . 'hash-table).")

(defun memoize=write (key value stash &optional (stash-path *stash-path*))
  "I receive a KEY and a VALUE, saving VALUE on a file named
STASH-values, which has one field per line. VALUE comes from
`memoize=eval', which returns a plist; the only one that we have to
take care of here is `:result', which is the information we want to
save for easy retrieval. In order to do that, we serialize it into
octets, then compress those octets, and finally encode them in a
base64 string.  This is the format in which `:result' will be written.
Now we append the whole VALUE thus updated on the STASH-values file,
which has one field per line. The VALUE is taken care of.  For
retrieval, I read the STASH-keys file, a serialized hash-table, and
append the new key having the new offset as its value, and finally I
rewrite that STASH-keys file.

  I do all that so we don’t need to have the entire STASH-values on
memory, while still being able to retrieve the VALUE at a decent
speed, hopefully. Also, appending the STASH-values file should be
cheap, which wouldn’t be the case if we saved the whole thing as a
hash-table, and superseding/rewriting it at every new addition.

WARNING: `:result's bigger than the usual, ie. > 800kb, choke everything.

TODO: make everything work with streams instead."
  (ensure-directories-exist stash-path)
  (let ((keys-file   (concat stash-path stash "-keys"))
	(values-file (concat stash-path stash "-values"))
	(keys-hash-table)
	(result)
	(result-encoded)
	(keys-file-offset))
    ;; now we find the correct hash-table. it should inhabit
    ;; *stash-key-hash-tables-alist*, so we try that first
    (setq keys-hash-table (cdr (assoc stash *stash-key-hash-tables-alist* :test #'equal)))
    ;; if we don’t get it, it might be in a file he haven’t read
    ;; yet. if the file exists, get it from there.
    (when (file-exists-p keys-file)
      (setq keys-hash-table
	    (serialize-as-octet=decode
	     (decompress-octets
	      (file-read-as-octets keys-file)))))
    ;; otherwise we never saw that stash before, and so we create a
    ;; new hash-table.
    (when (not keys-hash-table)
      (setq keys-hash-table (make-hash-table :test #'equal)))
    ;;
    ;; encode the result
    ;;
    (setq result (getf value :result))
    (setq result (serialize-as-octet=encode result))
    (setq result-encoded (compress-octets result))
    (setq result-encoded (base64-encode-octets-to-string result-encoded))
    ;; update VALUE with the encoded result
    (setf (getf value :result) result-encoded)
    ;; write the value on the file
    (setq keys-file-offset (last-member (file-append value values-file t)))
    ;; add the key and offset on the appropriate hash-table.
    (setf (gethash key keys-hash-table) keys-file-offset)
    ;; remove the hash-table from the list of hash-tables, in case
    ;; it’s there, so we don’t have two entries about it.
    (setq *stash-key-hash-tables-alist* (alist-remove-by-car stash *stash-key-hash-tables-alist*))
    ;; add it (back)
    (setq *stash-key-hash-tables-alist* (acons stash keys-hash-table *stash-key-hash-tables-alist*))
    ;; write the hash-table back
    ;; first, serialize it
    (setq keys-hash-table (serialize-as-octet=encode keys-hash-table))
    (setq keys-hash-table (compress-octets keys-hash-table))
    ;; then actually write it
    (file-write-octets keys-hash-table keys-file)
    (list :size-bytes-before (length result) :size-bytes-after (length result-encoded))))

(defun memoize=read (key stash &optional (stash-path *stash-path*))
  (let ((keys-file   (concat stash-path stash "-keys"))
	(values-file (concat stash-path stash "-values"))
	(keys-hash-table)
	(keys-file-offset)
	(value)
	(result))
    ;; now we find the correct hash-table. it should inhabit
    ;; *stash-key-hash-tables-alist*, so we try that first
    (setq keys-hash-table (cdr (assoc stash *stash-key-hash-tables-alist* :test #'equal)))
    ;; if we don’t get it, it might be in a file he haven’t read
    ;; yet. if the file exists, get it from there.
    (unless keys-hash-table
      (when (file-exists-p keys-file)
	(setq keys-hash-table
	      (serialize-as-octet=decode
	       (decompress-octets
		(file-read-as-octets keys-file))))))
    ;; otherwise we never saw that stash before, so we give an error.
    (when (not keys-hash-table)
      (error "This stash doesn’t exist."))
    (setq keys-file-offset (gethash key keys-hash-table))
    ;;
    ;; decode the result
    ;;;
    (setq value (file-read-from-offset values-file keys-file-offset))
    (setq value (read-from-string value))
    (setq result (getf value :result))
    (setq result (base64-decode-string-to-string result))
    (setq result (string-to-octets result))
    (setq result (decompress-octets result))
    (setq result (serialize-as-octet=decode result))
    (setf (getf value :result) result)
    value))

(defun memoizedp (key stash &optional (stash-path *stash-path*))
  (let ((keys-file   (concat stash-path stash "-keys"))
	(keys-hash-table))
    ;; now we find the correct hash-table. it should inhabit
    ;; *stash-key-hash-tables-alist*, so we try that first
    (setq keys-hash-table (cdr (assoc stash *stash-key-hash-tables-alist* :test #'equal)))
    ;; if we don’t get it, it might be in a file he haven’t read
    ;; yet. if the file exists, get it from there.
    (unless keys-hash-table
      (when (file-exists-p keys-file)
	(setq keys-hash-table
	      (serialize-as-octet=decode
	       (decompress-octets
		(file-read-as-octets keys-file))))))
    ;; otherwise we never saw that stash before.
    (if (not keys-hash-table)
	nil
	(gethash key keys-hash-table))))

(defun memoized (sexp &optional return-whole-entry-p (stash-path *stash-path*))
  "TODO: the function name shouldn’t have /, otherwise the file can’t
be written, at least on ext4 filesystems. What to do? Replace it by what?"
  ;; `princ-to-string' strips the package part of the name, as
  ;; `dex:get' → "GET", but `prin1-to-string does give "DEX:GET".
  (let ((stash (prin1-to-string (car sexp)))
	(key (str (cdr sexp)))
	value)
    (if (memoizedp key stash stash-path)
	(setq value (memoize=read key stash stash-path))
	(progn (setq value (memoize=eval sexp))
	       (memoize=write key value stash)))
    (if return-whole-entry-p
	value
	(getf value :result))))
