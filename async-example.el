;;; -*- lexical-binding: t -*-

(let* ((rest)
       (proc (make-process :name "consult-async"
                           :command '("sh" "/home/user/elisp-snippets/test.sh")))
       (async (consult--async-make :cleanup (lambda () (ignore-errors (kill-process proc))))))
  (set-process-filter proc (lambda (_ out)
                             (let ((lines (split-string out "\n")))
                               (if (cdr lines)
                                   (progn
                                     (setcar lines (concat rest (car lines)))
                                     (setq rest (car (last lines)))
                                     (consult--async-push async (nbutlast lines)))
                                 (setq rest (concat rest (car lines)))))))
  (set-process-sentinel proc (lambda (_ event)
                               (when (and rest (string= event "finished\n"))
                                 (consult--async-push async (list rest) 'done))))
  (consult--read
   "Test: " async
   :sort nil))
