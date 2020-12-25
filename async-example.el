;;; -*- lexical-binding: t -*-

(defun consult--async-example ()
  (thread-first (consult--async-sink)
    (consult--async-transform (lambda (x) (concat "<" x ">")))
    (consult--async-transform (lambda (x) (concat "#" x "#")))
    (consult--async-timer)
    (consult--async-indicator)
    (consult--async-process '("sh" "/home/user/elisp-snippets/test.sh"))))

(consult--read
 "Test: " (consult--async-example)
 :sort nil)
