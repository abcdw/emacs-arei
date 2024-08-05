;;; arei-ui.el --- Tools for working with ui components -*- lexical-binding: t; coding:utf-8 -*-

;; SPDX-License-Identifier: GPL-3.0-or-later
;; Copyright © 2024 Andrew Tropin <andrew@trop.in>

;;; Code:

(require 'pulse)

(defun arei-ui--pulse-momentary-highlight-overlay (o &optional face)
  "Pulse the overlay O, unhighlighting before next command.
Optional argument FACE specifies the face to do the highlighting.

This function is the same as original
`pulse-momentary-highlight-overlay', but overlay priority is set
to 10000."
  ;; We don't support simultaneous highlightings.
  (pulse-momentary-unhighlight)
  (overlay-put o 'original-face (overlay-get o 'face))
  ;; Make this overlay take priority over the `transient-mark-mode'
  ;; overlay.
  (overlay-put o 'original-priority (overlay-get o 'priority))
  (overlay-put o 'priority 10000)
  (setq pulse-momentary-overlay o)
  (if (eq pulse-flag 'never)
      nil
    (if (or (not pulse-flag) (not (pulse-available-p)))
	;; Provide a face... clear on next command
	(progn
	  (overlay-put o 'face (or face 'pulse-highlight-start-face))
	  (add-hook 'pre-command-hook
		    #'pulse-momentary-unhighlight))
      ;; Pulse it.
      (overlay-put o 'face 'pulse-highlight-face)
      ;; The pulse function puts FACE onto 'pulse-highlight-face.
      ;; Thus above we put our face on the overlay, but pulse
      ;; with a reference face needed for the color.
      (pulse-reset-face face)
      (let* ((start (color-name-to-rgb
                     (face-background 'pulse-highlight-face nil 'default)))
             (stop (color-name-to-rgb (face-background 'default)))
             (colors (mapcar (apply-partially 'apply 'color-rgb-to-hex)
                             (color-gradient start stop pulse-iterations))))
        (setq pulse-momentary-timer
              (run-with-timer 0 pulse-delay #'pulse-tick
                              colors
                              (time-add nil
                                        (* pulse-delay pulse-iterations))))))))

(advice-add 'pulse-momentary-highlight-overlay :override
            'arei-ui--pulse-momentary-highlight-overlay)

(defun arei-ui-blink-region (bounds)
  "Temporarily highlight the region from START to END."
  (let* ((start (car bounds))
         (end (cdr bounds))
         ;; TODO: [Nikita Domnitskii, 2024-07-25] should be a custom
         (pulse-delay 0.06))
    (pulse-momentary-highlight-region start end)))

(provide 'arei-ui)
;;; arei-ui.el ends here
