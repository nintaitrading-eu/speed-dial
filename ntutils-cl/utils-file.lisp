;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; utils-file.lisp:
;;;; File that contains general functions, related to file manipulation.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :utils-cl) ; Note: was the top-package, from which it was called... testing needed.

(defun my-read-lines (a-file)
  "Read the lines of a file."
  (with-output-to-string (z-out) (with-open-file (stream a-file)
    (do ((char (read-char stream nil) (read-char stream nil))) ((null char)) (format z-out "~a" char)))))

; Note: requires filter-comment-lines... need to remove the dependency
(defun load-lines-from-file (a-file a-comment-chars)
  "Loads all lines from a file, ignoring comments."
  (mapcar (lambda (a-comment-char)
    (filter-comment-lines
      (my-read-lines a-file) a-comment-char)) a-comment-chars))
