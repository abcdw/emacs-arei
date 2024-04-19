;;; arei-eldoc.el --- Eldoc support for Arei -*- lexical-binding: t; coding:utf-8 -*-

;; Copyright (C) 2024  Nikita Domnitskii

;; Author: Nikita Domnitskii <nikita@domnitskii.me>
;; Keywords: languages, guile, scheme, nrepl

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; eldoc support for Arei

;;; Code:

(require 'arei-client)
(require 'arei-nrepl)
(require 'map)
(eval-when-compile (require 'subr-x))
(eval-when-compile (require 'pcase))

(defvar arei-eldoc--last-sym nil)

(defun arei-eldoc--callback (sym pos callback)
  (lambda (response)
    (when-let* ((info (arei-nrepl-dict-get response "info")))
      (setq arei-eldoc--last-sym (cons sym info))
      (arei-eldoc--display-arglists sym pos info callback))))

(defun arei-eldoc--display-arglists (sym pos info callback)
  (pcase info
    ((map ns arglists)
     (funcall callback
              (arei-eldoc-format-arglists arglists pos)
              :thing (format "%s %s" ns sym)
              :face 'font-lock-function-name-face))))

(defun arei-eldoc--format-arglist (arglist pos)
  (let* ((highlighted-arglist (arei-eldoc--highlight-args arglist pos)))
    (concat "(" (string-join highlighted-arglist " ") ")")))

(defun arei-eldoc--highlight-args (arglist pos)
  (let ((rest-pos (seq-position arglist ".")))
    (thread-first
      (seq-reduce (lambda (acc arg)
                    (pcase-let (((map :idx :args) acc))
                      (cond
                       ((null arg) acc)
                       ((or
                         (string-prefix-p "#:" arg)
                         (string= arg "."))
                        (map-put! acc :args (cons arg args))
                        acc)
                       ((or (and (integerp pos)
                                 (or (= (1+ idx) pos)
                                     (and rest-pos
                                          (> (1+ idx) rest-pos)
                                          (> pos rest-pos))))
                            (and (stringp pos) (string= arg pos)))
                        (let ((arg (propertize arg 'face 'eldoc-highlight-function-argument)))
                          (map-put! acc :args (cons arg args))
                          (map-put! acc :idx (1+ idx))
                          acc))
                       (t
                        (map-put! acc :args (cons arg args))
                        (map-put! acc :idx (1+ idx))
                        acc))))
                  arglist
                  (list :idx 0 :args nil))
      (map-elt :args)
      (seq-reverse))))

(defun arei-eldoc--thing ()
  (save-excursion
    (when-let* ((pos (arei-eldoc--beginning-of-sexp))
                (thing (thing-at-point 'symbol t)))
      (cons thing pos))))

(defun arei-eldoc--beginning-of-sexp ()
  (let ((parse-sexp-ignore-comments t))
    (named-let lp ((key (when-let* ((key (thing-at-point 'sexp t))
                                    ((string-prefix-p "#:" key)))
                          (substring key 2)))
                   (pos (or (ignore-errors
                              (let ((p (point)))
                                (forward-sexp -1)
                                (forward-sexp 1)
                                (when (< (point) p) 1)))
                            0)))
      (let ((p (point)))
        (ignore-errors (forward-sexp -1))
        (cond
         ((and (null key)
               (string-prefix-p "#:" (thing-at-point 'sexp t)))
          (lp (substring (thing-at-point 'sexp t) 2) pos))
         ((< (point) p)
          (lp key (1+ pos)))
         (t
          (or key (max 0 (1- pos)))))))))

(defun arei-eldoc-format-arglists (arglists pos)
  (mapconcat
   (lambda (arglist)
     (arei-eldoc--format-arglist arglist pos))
   arglists
   " | "))

(defun arei-eldoc-arglist (callback)
  "Echo procedure arguments at point by calling CALLBACK.
Intended for `eldoc-documentation-functions' (which see)."
  (when (arei-connected-p)
    (pcase (arei-eldoc--thing)
      (`(,sym . ,pos)
       (if (string= sym (car arei-eldoc--last-sym))
           (arei-eldoc--display-arglists
            sym pos (cdr arei-eldoc--last-sym) callback)
         (let ((req (arei-nrepl-dict
                     "op" "lookup"
                     "sym" sym)))
           (when-let* ((module (arei-current-module)))
             (arei-nrepl-dict-put req "ns" module))
           (arei-send-request
            req
            (arei-connection-buffer)
            (arei-eldoc--callback sym pos callback)
            t)
           'wait-for-response))))))

(provide 'arei-eldoc)
;;; arei-eldoc.el ends here
