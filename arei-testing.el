;;; arei-testing.el --- suitbl Testing -*- lexical-binding: t; -*-

;;; SPDX-FileCopyrightText: 2024, 2025 Andrew Tropin <andrew@trop.in>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(require 'arei-client)
(require 'arei-nrepl)
(require 'transient)

(eval-when-compile (require 'map))
(eval-when-compile (require 'pcase))

(defun arei-testing--callback
    (connection-buffer &optional expression-end)
  "Set up a handler for eval request responses."
  (let ((vals nil))
    (lambda (response)
      ;; (message "%s" response)
      (pcase response
        ((map status value ares.evaluation/stack out err)
         ;; Jump to specific position only when there is something to print
         (when (or out err value)
           (goto-char (point-max)))

         (when (member "need-input" status)
           (arei--send-stdin))
         (when out
           (insert out))
         (when err
           (with-current-buffer connection-buffer
             (setq arei--last-error err))
           (insert (propertize err 'face
                               '((t (:inherit font-lock-warning-face))))))
         (when value
           (unless (= 0 (current-column))
             (insert "\n"))
           (insert (propertize value 'face
                               '((t (:inherit font-lock-string-face)))))
           (insert "\n"))
         (when ares.evaluation/stack
           (arei-show-debugger err ares.evaluation/stack))

         (when (and (member "multiple-values" status)
                    (not (member "done" status)))
           (push value vals))

         (when (member "done" status)
           (with-current-buffer connection-buffer
             (let* ((value (or (and (member "multiple-values" status)
                                    (if vals
                                        (mapconcat
                                         (lambda (v) (concat " => " v))
                                         (reverse vals)
                                         "\n")
                                      " => "))
                               (and value (concat " => " value))))
                    (fmt (if value "%s" " ;; interrupted")))
               (arei-ui-show-result fmt value expression-end))))

         (when (get-buffer-window)
           (set-window-point (get-buffer-window) (buffer-size))))))))

(defun arei-testing-run ()
  (interactive)
  (let* ((request (arei-nrepl-dict
                   "op" "ares.testing/run")))
    (arei-client-send-request
     request
     (arei-testing--callback (current-buffer) (point))
     ;; TODO: [Andrew Tropin, 2025-08-14] Use a separate session for
     ;; testing
     (arei--user-evaluation-session-id))))

(defun arei-testing-load-project-tests ()
  (interactive)
  (let* ((request (arei-nrepl-dict
                   "op" "ares.testing/load-project-tests")))
    (arei-client-send-request
     request
     (arei-testing--callback (current-buffer) (point))
     (arei--user-evaluation-session-id))))

(defvar arei-testing-base-command "make check"
  "Base command for running tests.")

(transient-define-argument arei-testing--parallel ()
  :description "Run in parallel"
  :class 'transient-switch
  :shortarg "-p"
  :key "-p"
  :argument "--parallel")

(transient-define-argument arei-testing--fail-fast ()
  :description "Stop on the first failure"
  :class 'transient-switch
  :shortarg "-f"
  :key "-f"
  :argument "--fail-fast")

(transient-define-suffix arei-testing-rerun-plain ()
  "Rerun last test command without modifications."
  (interactive)
  (compile arei-testing-base-command))

(transient-define-suffix arei-testing-rerun-with-flags ()
  "Rerun tests with current flags."
  (interactive)
  (let ((flags (mapconcat #'identity (transient-args transient-current-command) " ")))
    (compile (concat arei-testing-base-command " " flags))))

(defvar arei-testing--loaded-tests-count 0
  "Number of loaded tests. Should be set by your test framework.")

(defun arei-testing--display-test-count ()
  "Display number of loaded tests."
  (let* ((request (arei-nrepl-dict
                   "op" "ares.testing/get-test-runner-stats"))
         (response (arei-client-send-sync-request
                    request (arei--user-evaluation-session-id)))
         (stats (read (arei-nrepl-dict-get response "value")))
         (loaded-tests-count (alist-get 'loaded-tests-count stats))
         (selected-tests-count (alist-get 'selected-tests-count stats)))
    (format "Hello, I'm a base suitble test runner!

Tests loaded: %d
Tests selected: %d\n"
            loaded-tests-count
            selected-tests-count)))

(transient-define-prefix arei-testing-menu ()
  "Test execution menu."
  :value (list "--parallel" "--fail-fast")
  [:description arei-testing--display-test-count
   ["Switches"
    (arei-testing--parallel)
    (arei-testing--fail-fast)]
   ["Actions"
    ("r" "Rerun plain" arei-testing-rerun-plain)
    ("R" "Rerun with flags" arei-testing-rerun-with-flags)]])

(defvar-keymap arei-testing-map
  "C-t" #'arei-testing-run
  "C-m" #'arei-testing-menu
  "C-p" #'arei-testing-load-project-tests)

(provide 'arei-testing)
;;; arei-testing.el ends here
