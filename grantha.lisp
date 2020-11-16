(defpackage grantha
  (:use :cl)
  (:export alias
	   get-even-elements
	   get-odd-elements
	   make-plist
	   truncate-memory
	   truncate-time
	   round-to
	   pretty-number
	   human-readable-number
	   subtract-time-of-day
	   time-to-execute-and-result
	   parse-time
	   current-date-string
	   str
	   concat
	   padding
	   garbage-full))

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
(defun make-plist (keys data)
  "Construct a property list from KEYS and DATA."
  (let (out)
    (do ((x keys (cdr x))
	 (y data (cdr y)))
	((and (endp x) (endp y)) out)
      (if (or (endp x) (endp y))
          (error "The lists of keys and data are of unequal length."))
      (setq out (append (list (car x) (car y)) out)))))




;;
;; numbers
;;

;;
;; lossy
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
    (let ((div (expt 10 precision)))
         (/ (funcall what (* number div)) div)))

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

(defun time-to-execute-and-result (form)
  "Returns the time (always in miliseconds?) to execute FORM, and its result."
  (let ((before-time (get-internal-real-time))
	result
	after-time)
    (setq result (eval form))
    (setq after-time (get-internal-real-time))
    (list (- after-time before-time) result)))

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


;;
;; strings
;;

(defun str (i)
  (princ-to-string i))

(defun concat (&rest items)
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




;;
;; garbage collection
;;

(defun garbage-full (&optional raw-bytes-p)
  "I ask for full garbage collection and return thee the number of
bytes freed. Sometimes I return negative numbers, what should I do
with them?"
  (let ((old-dynamic-usage (sb-kernel:dynamic-usage))
	difference)
    (sb-ext:gc :full t)
    (setq difference (- old-dynamic-usage (sb-kernel:dynamic-usage)))
    (if raw-bytes-p
	difference
	(pretty-number difference))))