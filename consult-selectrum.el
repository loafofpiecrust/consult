;;; consult-selectrum.el --- Selectrum integration for Consult  -*- lexical-binding: t; -*-

;; Author: Daniel Mendler, Consult and Selectrum contributors
;; Maintainer: Daniel Mendler
;; Created: 2020
;; License: GPL-3.0-or-later
;; Version: 0.1
;; Package-Requires: ((consult "0.1") (selectrum "3.0") (emacs "26.1"))
;; Homepage: https://github.com/minad/consult

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; The Selectrum integration for Consult ensures that previews work when using
;; Selectrum. Furthermore, some minor Selectrum-specific `completing-read'
;; tweaks are applied. This is an extra package, since the consult.el package
;; only depends on Emacs core components.

;;; Code:

(require 'consult)
(require 'selectrum)

(defun consult-selectrum--preview-update ()
  "Preview function used for Selectrum."
  (when consult--preview-function
    (when-let (cand (selectrum-get-current-candidate))
      (funcall consult--preview-function cand))))

(defun consult-selectrum--preview-setup ()
  "Setup preview support for selectrum."
  (if consult-preview-mode
      (advice-add 'selectrum--minibuffer-post-command-hook :after #'consult-selectrum--preview-update)
    (advice-remove 'selectrum--minibuffer-post-command-hook #'consult-selectrum--preview-update)))

(add-hook 'consult-preview-mode-hook #'consult-selectrum--preview-setup)
(consult-selectrum--preview-setup) ;; call immediately to ensure load-order independence

(defun consult-selectrum--refresh ()
  "Refresh selectrum view."
  (when (eq completing-read-function #'selectrum-completing-read)
    (selectrum-exhibit)))

(add-hook 'consult--completion-refresh-hook #'consult-selectrum--refresh)

;; HACK: Hopefully selectrum adds something like this to the official API.
;; https://github.com/raxod502/selectrum/issues/243
;; https://github.com/raxod502/selectrum/pull/244
(advice-add #'consult--read :around
            (lambda (fun prompt candidates &rest opts)
              (minibuffer-with-setup-hook
                  (lambda ()
                    (setq-local selectrum--move-default-candidate-p (plist-get opts :default-top)))
                (apply fun prompt candidates opts))))

(provide 'consult-selectrum)
;;; consult-selectrum.el ends here
