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


;;;
;;; nREPL Sessions
;;;

(defun arei-client--get-session-id (name)
  "Get session-id from session NAME."
  (with-current-buffer (arei-connection-buffer)
    (gethash name arei-client--nrepl-sessions)))

(defun arei--initialize-nrepl-sessions (connection)
  "Initialize sessions needed for Arei operation."
  (arei--create-nrepl-session
   connection
   "evaluation"
   (lambda ()))
  (arei--create-nrepl-session
   connection
   "tooling"
   (lambda ())))


;;;
;;; Connection
;;;

;; TODO: [Andrew Tropin, 2023-11-20] Add association between session
;; and output buffer for it.  It's needed to support multiple nrepl
;; sessions that can use separate buffers for stdin/stdout instead of
;; using primary connection buffer.  Also, adding
;; `arei-set-default-nrepl-session' may help for eval and switch
;; operations.
(defun arei--new-nrepl-session-handler (session-name &optional callback)
  "Returns callback that is called when new session is created."
  (lambda (response)
    (pcase response
      ((map new-session)
       (when new-session
         (insert
          (propertize
           (format ";;; nREPL session created: %s\n" session-name)
           'face
           '((t (:inherit font-lock-comment-face)))))
         (message "Connected to nREPL server.")
         (when callback (funcall callback))
         (setq-local arei--nrepl-session new-session)
         (puthash session-name new-session arei-client--nrepl-sessions))))))

(defun arei--create-nrepl-session (connection session-name &optional callback)
  "Setups an nrepl session and register it in `arei--nrepl-sessions'."
  (puthash session-name nil arei-client--nrepl-sessions)
  (arei-send-request
   (arei-nrepl-dict "op" "clone")
   connection
   (arei--new-nrepl-session-handler session-name callback)))


(defun arei--sentinel (process message)
  "Called when connection is changed; in out case dropped."
  (with-current-buffer (process-buffer process)
    (sesman-quit))
  (message "nREPL connection closed: %s" message)
  ;; NOTE: sesman-post-command-hook is not run when connection buffer is
  ;; killed, so we run it here just in case
  (run-hooks 'sesman-post-command-hook))

;; TODO: [Andrew Tropin, 2023-10-19] Handle incomplete incomming string.
(defun arei--connection-filter (process string)
  "Decode message(s) from PROCESS contained in STRING and dispatch them."
  (let ((string-q (process-get process :string-q)))
    (queue-enqueue string-q string)
    ;; Start decoding only if the last letter is 'e'
    (when (eq ?e (aref string (1- (length string))))
      (let ((response-q (process-get process :response-q)))
        (arei-nrepl-bdecode string-q response-q)
        (while (queue-head response-q)
          (with-current-buffer (process-buffer process)
            (let ((response (queue-dequeue response-q)))
              ;; (message "response: %s\n" response)
              ;; (with-demoted-errors
              ;;     "Error in one of the `nrepl-response-handler-functions': %s"
              ;;   (run-hook-with-args 'nrepl-response-handler-functions response))
              (arei--dispatch-response response))))))))

(defun arei--dispatch-response (response)
  "Find associated callback for a message by id."
  (pcase response
    ((map id status)
     (when-let* ((callback (gethash id arei-client--pending-requests)))
       (when callback
         (funcall callback response)
         (when (member "done" status)
           (remhash id arei-client--pending-requests)))))))

(defun arei--connect (params)
  "Call callback after connection is established."
  (let* ((host (plist-get params :host))
         (port (number-to-string (plist-get params :port)))
         (host-and-port (concat host ":" port))
         (project (project-current))
         (session-prefix (if project (project-name project) (buffer-name)))
         (sesman-session-name (concat session-prefix ":" host-and-port))
         (buffer-name (concat "*arei: " sesman-session-name "*")))

    ;; Prevent function being called directly, bypassing sesman
    ;; machinery and thus cleanup phaseses.
    (when (get-buffer buffer-name)
      (user-error "Connection buffer already exist."))

    (condition-case err
        (let* ((process (open-network-stream
                         (concat "nrepl-connection-" host-and-port)
                         buffer-name host port))
               (buffer (process-buffer process)))
          (set-process-coding-system process 'utf-8-unix 'utf-8-unix)
          (set-process-filter process 'arei--connection-filter)
          (set-process-sentinel process 'arei--sentinel)
          (process-put process :string-q (queue-create))
          (process-put process :response-q (arei-nrepl-response-queue))

          (arei-client-clear-session-cache)
          (sesman-add-object 'Arei sesman-session-name buffer 'allow-new)

          (with-current-buffer buffer
            (arei-connection-mode)
            (setq arei-client--request-counter 0)
            (setq arei-client--nrepl-sessions (make-hash-table :test 'equal))
            (setq arei-client--pending-requests (make-hash-table :test 'equal))
            ;; Set the current working directory for the connection buffer
            ;; to the project root.
            (when (project-current)
              (setq default-directory (project-root (project-current))))

            (insert
             (propertize
              (format ";;; Connecting to nREPL host on '%s:%s'...\n" host port)
              'face
              '((t (:inherit font-lock-comment-face)))))
            (arei--initialize-nrepl-sessions buffer))
          buffer)
        (error
         (progn
           (kill-buffer buffer-name)
           (message "%s" (error-message-string err)))))))

(defun arei-client--print-pending-requests ()
  (interactive)
  (with-current-buffer (arei-connection-buffer)
    (maphash (lambda (key value)
               (message "Key: %s, Value: %s" key value))
             arei-client--pending-requests)))

;; MAYBE: Rename to switch-to-nrepl-session-buffer to make sure C-c
;; C-z jumps to buffer with needed output.
(defun arei-switch-to-connection-buffer ()
  (interactive)
  (if (arei-connection-buffer)
      (pop-to-buffer (arei-connection-buffer))
    (message "No connection associated with the buffer yet.")))

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
