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
  (map (lambda (a-string)
    (string-split a-string a-delimiter)) a-list-of-strings))

(defun take-first-char (a-string)
  "Take first char of string, unless string is empty."
  (if (eq? (string-length a-string) 0) "" (substring a-string 0 1)))

(defun filter-comment-lines (a-lines a-comment-char)
"Filter out strings that start with a comment-character,
so only the data lines are added.
Note: This function trims spaces on the left."
  (filter (lambda (a-line)
    (not (equal? (take-first-char (string-trim a-line)) a-comment-char)))
     a-lines))

(defun filter-empty-strings (a-stringlist)
"Removes empty strings from a list of strings."
  (filter (lambda (a-string)
     (not (equal? (string-length (string-trim a-string)) 0)))
      a-stringlist))

(defun load-lines-from-file (a-file a-comment-chars)
  "Loads all lines from a file, ignoring comments."
  (car
    (map (lambda (a-comment-char)
      (filter-comment-lines
        (read-lines a-file) a-comment-char)) a-comment-chars)))
