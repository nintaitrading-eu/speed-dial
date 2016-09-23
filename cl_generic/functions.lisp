;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; functions.lisp:
;;;; File that contains general functions, not related to a specific application.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :speed-dial)

(defun split-list-of-strings (a-list-of-strings a-delimiter)
  "Gets a list of <delimiter>-separated strings and turns it into a list of a list of strings."
; Example:
; ("test1a;test1b;test1c" "test2a;;test2c")
; -> (("test1a" "test1b" "test1c") ("test2a" "" "test2c"))
  (mapcar (lambda (x) (string-split x a-delimiter)) a-list-of-strings))

(defun take-first-char (a-string)
  "Take first char of string, unless string is empty."
  (if (equal (length a-string) 0) "" (substring a-string 0 1)))

(defun filter-comment-lines (a-lines a-comment-char)
"Filter out strings that start with a comment-character,
so only the data lines are added.
Note: This function trims spaces on the left."
  (remove-if-not #'(lambda (a-line)
    (not (equal (take-first-char (string-trim '(#\Space #\e #\t #\m) a-line)) a-comment-char)))
     a-lines))

(defun filter-empty-strings (a-stringlist)
"Removes empty strings from a list of strings."
  (remove-if-not #'(lambda (a-string)
     (not (equal (length (string-trim '(#\Space #\e #\t #\m) a-string)) 0)))
      a-stringlist))

(defun my-read-lines (a-file)
  "Read the lines of a file."
  (with-output-to-string (z-out) (with-open-file (stream a-file)
    (do ((char (read-char stream nil) (read-char stream nil))) ((null char)) (format z-out "~a" char)))))

(defun load-lines-from-file (a-file a-comment-chars)
  "Loads all lines from a file, ignoring comments."
  (mapcar (lambda (a-comment-char)
    (filter-comment-lines
      (my-read-lines a-file) a-comment-char)) a-comment-chars))
