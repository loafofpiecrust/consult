;;; -*- lexical-binding: t -*-

(let* ((rest)
       (proc (make-process :name "consult-async"
                           :command '("sh" "/home/user/elisp-snippets/test.sh")))
       (async (consult--async :cleanup (lambda () (ignore-errors (kill-process proc))))))
  (set-process-filter proc (lambda (_ out)
                             (let ((lines (split-string out "\n")))
                               (if (cdr lines)
                                   (progn
                                     (setcar lines (concat rest (car lines)))
                                     (setq rest (car (last lines)))
                                     (funcall async (nbutlast lines)))
                                 (setq rest (concat rest (car lines)))))))
  (set-process-sentinel proc (lambda (_ event)
                               (when (and rest (string= event "finished\n"))
                                 (funcall async (list rest))
                                 (funcall async 'done))))
  (consult--read
   "Test: " async
   :sort nil))
