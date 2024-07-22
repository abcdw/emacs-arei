;;; arei-overlay.el --- Tools for working with overalys -*- lexical-binding: t; coding:utf-8 -*-

;; SPDX-License-Identifier: GPL-3.0-or-later
;; Copyright © 2024 Andrew Tropin <andrew@trop.in>

;;; Code:

(defun arei-overlay-blink-region (bounds)
  "Temporarily highlight the region from START to END."
  (let* ((start (car bounds))
         (end (cdr bounds))
         (overlay (make-overlay start end)))
    (overlay-put overlay 'face '(:inherit highlight :extend t))
    (overlay-put overlay 'priority 10000)
    (run-at-time "0.5 sec" nil 'delete-overlay overlay)))

(provide 'arei-overlay)
;;; arei-overlay.el ends here
