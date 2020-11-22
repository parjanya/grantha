#|
  This file is a part of grantha project.
  Copyright (c) 2020 Edgard Bikelis (bikelis@gmail.com)

  Author: Edgard Bikelis (bikelis@gmail.com)

  Carteiro is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or (at
  your option) any later version.

  Carteiro is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with Grantha.  If not, see <https://www.gnu.org/licenses/>.
|#

;;
;; Here I place everything that depends on other packages.
;;

(in-package :grantha)


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
	(grantha:pretty-number difference))))




;;
;; threads
;;

(defun kill-all-threads-but-current ()
  (dolist (thread (sb-thread:list-all-threads))
    (unless (equal sb-thread:*current-thread* thread)
      (sb-thread:terminate-thread thread))))




;;
;; lisp images
;;





;;
;; internet related
;;

(defun url-encode (str)
  "I transform \"lá\" into \"l%C3%A1\", so you can use it in an URL."
  (do-urlencode:urlencode str))

(defun url-decode (str)
  "I transform \"l%C3%A1\" into \"lá\", so you can get the string from an URL."
  (do-urlencode:urldecode str))
