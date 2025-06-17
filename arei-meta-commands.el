;;; arei-meta-commands.el --- Commands for using meta-commands -*- lexical-binding: t; coding:utf-8 -*-

;;; SPDX-FileCopyrightText: 2025 Libre en Communs <contact@a-lec.org>
;;; SPDX-FileCopyrightText: 2025 Noé Lopez <noelopez@free.fr>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(require 'arei-client)
(require 'arei-evaluation)

(defun arei-choose-meta-command ()
  (let* ((reply (arei-client-send-sync-request
                 (arei-nrepl-dict "op" "ares.guile.meta-commands/list")
                 nil))
         (commands (arei-nrepl-dict-get reply "names")))
    (completing-read "Meta-command: " commands)))

(defun arei-call-meta-command (name)
  (interactive (list (arei-choose-meta-command)))
  (let ((arguments (read-from-minibuffer (concat "," name " "))))
    (arei-client-send-request
     (arei-nrepl-dict "op" "ares.guile.meta-commands/call"
                      "command" name
                      "arguments" arguments)
     (arei--process-user-eval-response-callback (current-buffer))
     (arei--user-evaluation-session-id))))

(provide 'arei-meta-commands)
