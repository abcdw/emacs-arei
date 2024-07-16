;;; arei-client.el --- Abstraction over nREPL interactions -*- lexical-binding: t; coding:utf-8 -*-

;; Copyright © 2023, 2024 Andrew Tropin
;; Copyright © 2024 Nikita Domnitskii

;; Author: Andrew Tropin <andrew@trop.in>
;;         Nikita Domnitskii <nikita@domnitskii.me>

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

;;; Code:
(require 'arei-nrepl)
(require 'sesman)
(require 'map)

(defvar-local arei-client--request-counter 0
  "Serial number for message, used for association between request
and responses.")

(defvar-local arei-client--pending-requests nil
  "A hash-table containing callbacks for pending requests.")

(defvar-local arei-client--nrepl-sessions nil
  "A hash-table containing name and session-id association.")

(defvar arei-client-sync-timeout 5
  "Number of seconds to wait for a sync response")

(defvar arei-client--session-cache (make-hash-table :test 'equal)
  "Session cache for `arei-connection-buffer'.")

(defun arei-client--get-session-id (name)
  "Get session-id from session NAME."
  (with-current-buffer (arei-connection-buffer)
    (gethash name arei-client--nrepl-sessions)))

(defun arei-client--print-pending-requests ()
  (interactive)
  (with-current-buffer (arei-connection-buffer)
    (maphash (lambda (key value)
               (message "Key: %s, Value: %s" key value))
             arei-client--pending-requests)))

(defun arei-client-remove-from-session-cache ()
  "Remove current file-name association from session cache.
This function is intended to be used as a value for `kill-buffer-hook'."
  (let ((filename (buffer-file-name (current-buffer))))
    (map-delete arei-client--session-cache filename)))

(defun arei-client-clear-session-cache ()
  "Clear session cache.
This function is intended to be used as a value for `sesman-post-command-hook'."
  ;; NOTE: no equalent in map.el
  (clrhash arei-client--session-cache))

(defun arei-connection-buffer ()
  "Return a connection buffer associated with the current session."
  (let ((filename (buffer-file-name (current-buffer))))
    (if (map-contains-key arei-client--session-cache filename)
        (map-elt arei-client--session-cache filename)
      (let ((buff (cadr (sesman-current-session 'Arei))))
        (map-put! arei-client--session-cache filename buff)))))

(defun arei-connection ()
  "Return a process associated with the current session connection."
  (get-buffer-process (arei-connection-buffer)))

;; TODO: [Nikita Domnitskii, 2024-04-16] move connection related code to
;; arei-connection or something
(defun arei-connected-p ()
  "Return t if Arei is currently connected, nil otherwise."
  (process-live-p (arei-connection)))

(defun arei-send-request (request connection callback &optional session-id)
  "Send REQUEST and assign CALLBACK.
The CALLBACK function will be called when reply is received.

SESSION-ID should be either session-id, t or nil.  If SESSION-ID is t,
use tooling session, nil use no session."
  (unless connection (error "No nREPL connection for current session"))
  (with-current-buffer connection
    (let* ((id (number-to-string (cl-incf arei-client--request-counter))))
      ;; TODO: [Andrew Tropin, 2023-11-20] Ensure that session is created
      ;; at the moment of calling, otherwise put a request into callback.
      (when-let* ((session (if (eq session-id t)
                               (arei-client--get-session-id "tooling")
                             session-id)))
        (arei-nrepl-dict-put request "session" session))
      (arei-nrepl-dict-put request "id" id)
      (puthash id callback arei-client--pending-requests)
      (process-send-string nil (arei-nrepl-bencode request)))))

(defun arei-send-sync-request (request &optional connection session-id)
  "Send request to nREPL server synchronously."
  (let ((time0 (current-time))
        (connection (or connection (arei-connection-buffer)))
        response
        global-status)
    (unless connection (error "No nREPL connection for current session"))
    (arei-send-request
     request
     connection
     (lambda (resp) (setq response resp))
     session-id)
    (while (not (member "done" global-status))
      (setq global-status (arei-nrepl-dict-get response "status"))
      (when (time-less-p arei-client-sync-timeout
                         (time-subtract nil time0))
        (error "Sync nREPL request timed out %s" request))
      (accept-process-output nil 0.01))
    response))

(provide 'arei-client)
;;; arei-client.el ends here
