;;; arei-logging.el --- Control nREPL Messagnnnes Logging Verbosity for Ares -*- lexical-binding: t; -*-

;;; SPDX-FileCopyrightText: 2024, 2025 Andrew Tropin <andrew@trop.in>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

;; Control nREPL Messages Logging Verbosity for Ares

;;; Code:

(require 'arei-client)
(require 'arei-nrepl)

(eval-when-compile (require 'map))
(eval-when-compile (require 'pcase))


(defun arei--logging-callback ()
  "Set up a handler for `ares.logging/set-verbosity' request responses."
  (lambda (response)
    (pcase response
      ((map status ares.logging/verbosity)
       (unless (member "error" status)
         (message "Ares logging verbosity is set to `%s'"
                  ares.logging/verbosity))

       (when (and (member "error" status) (member "unknown-op" status))
         (user-error "The operationg ares.logging/set-verbosity is not found.  \
Probably the ares.logging extension is not loaded.  \
Which should not happen, but it is what is."))
       (when (and (member "error" status) (not (member "unknown-op" status)))
         (user-error "Something bad happend, please pray."))))))

;; TODO: [Andrew Tropin, 2025-07-29] Extract repeated code and
;; generalize it
(defun arei-logging-enable ()
  "Set `ares.logging/verbosity' to `normal'."
  (interactive)
  (let* ((request (arei-nrepl-dict
                   "ares.logging/verbosity" "normal"
                   "op" "ares.logging/set-verbosity")))
    (arei-client-send-request
     request
     (arei--logging-callback)
     (arei--tooling-session-id))))

(defun arei-logging-disable ()
  "Set `ares.logging/verbosity' to `quite'."
  (interactive)
  (let* ((request (arei-nrepl-dict
                   "ares.logging/verbosity" "quite"
                   "op" "ares.logging/set-verbosity")))
    (arei-client-send-request
     request
     (arei--logging-callback)
     (arei--tooling-session-id))))

(defvar-keymap arei-logging-map
  "C-e" #'arei-logging-enable
  "C-d" #'arei-logging-disable)

(provide 'arei-logging)
;;; arei-logging.el ends here
