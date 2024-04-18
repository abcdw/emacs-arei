;;; arei-client.el --- Abstraction over nREPL interactions -*- lexical-binding: t; coding:utf-8 -*-

;; Copyright © 2023, 2024 Andrew Tropin
;; Copyright © 2024 Nikita Domnitskii

;; Author: Andrew Tropin <andrew@trop.in>
;;         Nikita Domnitskii <nikita@domnitskii.me>

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

;; Abstraction over nREPL interactions

;; eldoc support for Arei

;;; Code:
(require 'arei-nrepl)
(require 'sesman)

(defconst arei--module-re
  "^[[:blank:]]*([[:blank:]\n]*define-module[[:blank:]\n]+\\(([^)]+)\\)"
  "Regular expression matching a top-level definition of the module.")

(defconst arei--library-re
  "^[[:blank:]]*([[:blank:]\n]*\\(?:define-\\)?library[[:blank:]\n]+\\(([^)]+)\\)"
  "Regular expression matching a top-level definition of the library.")

(defun arei-current-module ()
  "Find current buffer's module.  It goes backwards and looking for
`arei--module-re' or `arei--library-re' to match, after that it
goes the opposite direction.  This logic is not perfect, but
works good enough for this amount of code.  Also, it's similar
to geiser and cider approaches.  In the future it would be better
to extract this information from tree-sitter or some other more
preciese way."
  (save-excursion
    (when (or (re-search-backward arei--module-re nil t)
              (re-search-backward arei--library-re nil t)
              (re-search-forward arei--module-re nil t)
              (re-search-forward arei--library-re nil t))
      (match-string-no-properties 1))))

(defvar-local arei--request-counter 0
  "Serial number for message, used for association between request
and responses.")

(defvar-local arei-client--pending-requests nil
  "A hash-table containing callbacks for pending requests.")

(defvar-local arei-client--sessions nil
  "A hash-table containing name and session-id association.")

(defvar arei-client-sync-timeout 5
  "Number of seconds to wait for a sync response")

(defun arei-client--print-pending-requests ()
  (interactive)
  (with-current-buffer (arei-connection-buffer)
    (maphash (lambda (key value)
               (message "Key: %s, Value: %s" key value))
             arei-client--pending-requests)))

(defun arei-connection-buffer ()
  "Returns a connection buffer associated with the current session."
  (cadr (sesman-current-session 'Arei)))

(defun arei-connection ()
  "Returns a process associated with the current session connection."
  (get-buffer-process (arei-connection-buffer)))

;; TODO: [Nikita Domnitskii, 2024-04-16] move connection related code to
;; arei-connection or something
(defun arei-connected-p ()
  "Return t if AREI is currently connected, nil otherwise."
  (process-live-p (arei-connection)))

(defun arei-client--current-session ()
  (gethash "tooling" arei-client--sessions))

(defun arei-send-sync-request (request &optional connection with-session)
  "Send request to nREPL server synchronously."
  ;; TODO: handle the case, when connection is not available.
  (let ((time0 (current-time))
        (conn (or connection (arei-connection-buffer)))
        response
        global-status)
    (arei-send-request
     request
     conn
     (lambda (resp) (setq response resp))
     with-session)
    (while (not (member "done" global-status))
      (setq global-status (arei-nrepl-dict-get response "status"))
      (when (time-less-p arei-client-sync-timeout
                         (time-subtract nil time0))
        (error "Sync nREPL request timed out %s" request))
      (accept-process-output nil 0.01))
    (with-current-buffer conn
      (when-let* ((id (arei-nrepl-dict-get response "id")))
        (remhash id arei-client--pending-requests)))
    response))

(defun arei-send-request (request connection callback &optional with-session)
  "Send REQUEST and assign CALLBACK.
The CALLBACK function will be called when reply is received."
  (with-current-buffer connection
    (let* ((id (number-to-string (cl-incf arei--request-counter))))
      ;; TODO: [Andrew Tropin, 2023-11-20] Ensure that session is created
      ;; at the moment of calling, otherwise put a request into callback.
      (when-let* ((session (and with-session
                                (arei-client--current-session))))
        (arei-nrepl-dict-put request "session" session))
      (arei-nrepl-dict-put request "id" id)
      (puthash id callback arei-client--pending-requests)
      (process-send-string nil (arei-nrepl-bencode request)))))

(provide 'arei-client)
;;; arei-client.el ends here