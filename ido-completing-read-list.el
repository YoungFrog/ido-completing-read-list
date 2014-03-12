;;; ido-completing-read-list.el --- Request multiple elements from a list of candidates  -*- lexical-binding: t; -*-

;; Copyright (C) 2014  Nicolas Richard

;; Author: Nicolas Richard <theonewiththeevillook@yahoo.fr>
;; Keywords: convenience

;; This program is free software; you can redistribute it and/or modify
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

;;;; Existing related functionnality :
;; completing-read-multiple exists. It allows one to build a list of
;; elements with completion, separating them some separator (a comma,
;; by default).

;; to try it: (completing-read-multiple "Prompt: " '(plaf plouf bloum))

;;;; What this one does
;; you can use ido's C-SPC to restrict matches then either
;; <M-S-return> to add all current matches to the list or RET to add
;; only match at point, or C-j to add prompt or finish.

;; Additionnaly, we redefine ido-restrict-to-matches to accept a
;; prefix arg. I guess this is bad, but I'm too lazy to modify this
;; just now.

;;;; Try it ?
;; M-: (yf/ido-completing-read-list '(foo bar bal baz)) RET
;; ba C-SPC <M-S-return>
;; C-j
;; => (bar bal baz)

;; Another one:
;; M-: (yf/ido-completing-read-list '(foo bar bal baz bla)) RET
;; ba C-u C-SPC <M-S-return>
;; l RET
;; C-j
;; => (foo bla bal)


;;; Code:

(require 'dash)
(defvar yf/ido-request-a-list-matches nil
  "Internal use. Dynamically bound.")


;; the code of yf/ido-completing-read-list is implemented on top of
;; yf/ido-completing-read-with-printfun, which I use in my own .emacs.
;; This is non-sense and should be rewritten (probably merging both
;; into the former)
(defun yf/ido-completing-read-with-printfun (prompt choices &optional printfun &rest rest)
  "CHOICES can be a list of any lisp objects PRINTFUN is used to
show them in the minibuffer prompt -- by default this is
`prin1-to-string'."
  (or printfun (setq printfun #'prin1-to-string))
  (setq choices (mapcar
                 (lambda (choice)
                   (cons (funcall printfun choice) choice))
                 choices))
  (let* ((userinput (apply #'ido-completing-read
                           prompt
                           (mapcar #'car choices)
                           rest))
         (found (assoc userinput choices)))
    (when yf/ido-request-a-list-matches
      (let ((result))
        (dolist (elt (copy-sequence yf/ido-request-a-list-matches))
          (push
           (--if-let (assoc elt choices)
               (cdr it)
             ;; 
             elt)
           result))
        (setq yf/ido-request-a-list-matches (nreverse result))))
    (if found 
        (cdr found)
      userinput)))


(defun yf/ido-request-a-list-exit ()
  (interactive)
  (setq yf/ido-request-a-list-matches ido-matches
        ido-exit 'done)
  (exit-minibuffer))
(defvar request-a-list-map
  (let
      ((map
        (make-sparse-keymap)))
    (define-key map
      (kbd "<M-S-return>")
      'yf/ido-request-a-list-exit)
    map))
(defalias 'yf/request-a-list 'yf/ido-completing-read-list)

;; the current signature might not be the best atm, but I have no idea
;; what the other args of the other "completing-read" family of
;; functions should do in the list context.

;; prompt comes after collection to allow it to be optional. Perhaps
;; this is bad ?
(defun yf/ido-completing-read-list (collection &optional prompt function)
  "COLLECTION is a list from which completion will be done.
PROMPT is a string in which % has the special role it has in the
string argument to `format', the argument will be the current
selected sublist. FUNCTION is the printfun arg of
`yf/ido-completing-read-with-printfun'."
  (setq prompt (or prompt "Add element to list %s: "))
  (let ((collection (copy-sequence collection)))
    (let (result
          elt
          done
          yf/ido-request-a-list-matches
          (minibuffer-local-map (make-composed-keymap request-a-list-map minibuffer-local-map)))
      
      (while (not done)
        (setq elt (yf/ido-completing-read-with-printfun 
                   (format prompt (or
                                   (mapcar (or function #'prin1-to-string)
                                           result)
                                   "[none yet]"))
                   collection
                   function))
        (cond (yf/ido-request-a-list-matches
               (dolist (elt yf/ido-request-a-list-matches)
                 (setq collection (delq elt collection)))
               (setq result (append yf/ido-request-a-list-matches result)
                     yf/ido-request-a-list-matches nil))
              ((and elt (not (equal elt "")))
               (setq collection (delq elt collection))
               (setq result (nreverse (cons elt (nreverse result)))))
              (t (setq done t))))
      result)))

(defun ido-restrict-to-matches (&optional removep)
  "Set current item list to the currently matched items.

When argument REMOVEP is non-nil, the currently matched items are
instead removed from the current item list."
  (interactive "P")
  (when ido-matches
    (setq ido-cur-list (cond
                        (removep
                         (delq nil
                               (mapcar
                                (lambda (elt)
                                  (when (not (member elt ido-matches))
                                    elt))
                                ido-cur-list)))
                        (t ido-matches))
          ido-matches ido-cur-list
	  ido-text-init ""
	  ido-rescan nil
	  ido-exit 'keep)
    (exit-minibuffer)))

(provide 'ido-completing-read-list)
;;; ido-completing-read-list.el ends here
