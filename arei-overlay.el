;;; arei-overlay.el --- Tools for working with overalys -*- lexical-binding: t; coding:utf-8 -*-

;; SPDX-License-Identifier: GPL-3.0-or-later
;; Copyright © 2024 Andrew Tropin <andrew@trop.in>

;;; Code:

(require 'pulse)

(defun arei-overlay-blink-region (bounds)
  "Temporarily highlight the region from START to END."
  (let* ((start (car bounds))
         (end (cdr bounds))
         ;; TODO: [Nikita Domnitskii, 2024-07-25] should be a custom
         (pulse-delay 0.06))
    (pulse-momentary-highlight-region start end)))

(provide 'arei-overlay)
;;; arei-overlay.el ends here
