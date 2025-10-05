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

(defvar arei-testing-start-symbol "⚙️"
  "The symbol used before test execution.")

(defvar arei-testing-success-symbol "✓"
  "The symbol used on successful test execution.")

(defvar arei-testing-failure-symbol "☠️"
  ;; Sample alterantives: ⚠️
  "The symbol used on faulty test execution.")

(defface arei-testing-success-face
  '((t :inherit diff-refine-added
       :weight bold))
  "Face for `arei-testing-success-symbol'."
  :group 'arei-testing)

(defface arei-testing-failure-face
  '((t :inherit diff-refine-removed
       :weight bold))
  "Face for `arei-testing-failure-symbol'."
  :group 'arei-testing)

(defun arei-testing--handle-run-results (response)
  (pcase response
    ((map status value ares.evaluation/stack out ares.exception)

     (when value
       (unless (= 0 (current-column))
         (insert "\n"))
       (insert (propertize "Tests execution finished" 'face
                           '((t (:inherit font-lock-string-face)))))
       (insert "\n"))

     (when (member "error" status)
       (message "Error during test run: %s" ares.exception))

     (when (and (= 1 (length status)) (member "done" status))
       (let-alist (read value)
         (let ((run-summary
                (format
                 "errors: %s, failures: %s, assertions: %s, tests: %s"
                 .errors .failures .assertions .tests)))
           (if (< 0 (+ .errors .failures))
               (message
                (concat (propertize
                         (concat " " arei-testing-failure-symbol " ")
                         'face 'arei-testing-failure-face)
                        " Failed some tests! %s")
                run-summary)
             (message
              (concat
               (propertize
                (concat " " arei-testing-success-symbol " ")
                'face 'arei-testing-success-face)
               " Passed all tests! %s") run-summary))))))))

(defun arei-testing--eval-callback (connection-buffer handle-response)
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

         (funcall handle-response response)

         (when ares.evaluation/stack
           (arei-show-debugger err ares.evaluation/stack))

         (when (get-buffer-window)
           (set-window-point (get-buffer-window) (buffer-size))))))))

(defun arei-testing-run ()
  (interactive)
  (let* ((request (arei-nrepl-dict
                   "op" "ares.testing/run")))
    (message
     (concat
      (propertize (concat " " arei-testing-start-symbol " ")
                  'face 'diff-function)
      " Little gnomes are executing your tests!"))
    (arei-client-send-request
     request
     (arei-testing--eval-callback
      (current-buffer)
      'arei-testing--handle-run-results)
     ;; TODO: [Andrew Tropin, 2025-08-14] Use a separate session for
     ;; testing
     (arei--user-evaluation-session-id))))

(defun arei-testing-load-project-tests ()
  (interactive)
  (let* ((request (arei-nrepl-dict
                   "op" "ares.testing/load-project-tests")))
    (message "Loading project tests...")
    (arei-client-send-request
     request
     (arei-testing--eval-callback
      (current-buffer)
      (lambda (r)
        (when (member "done" (arei-nrepl-dict-get r "status"))
          (message "Project tests are loaded."))))
     (arei--user-evaluation-session-id))))

(defun arei-testing-load-module-tests ()
  (interactive)
  (let* ((module (arei-current-module))
         (request (arei-nrepl-dict
                   "op" "ares.testing/load-module-tests"
                   "module" module)))
    (unless module
      (user-error "\
There is no Scheme module associated with this buffer.  Please, call \
this function from different buffer."))
    (message "Loading tests for module %s..." module)
    (arei-client-send-request
     request
     (arei-testing--eval-callback
      (current-buffer)
      (lambda (r)
        (when (member "done" (arei-nrepl-dict-get r "status"))
          (message "Tests for module %s are loaded." module))))
     (arei--user-evaluation-session-id))))

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
  "Test runner configuration menu."
  :value (list "--parallel" "--fail-fast")
  [:description arei-testing--display-test-count
   ["Switches"
    (arei-testing--parallel)
    (arei-testing--fail-fast)]
   ["Actions"
    ("r" "Rerun" arei-testing-run)
    ("t" "Rerun" arei-testing-run)]])

(defvar-keymap arei-testing-map
  "C-t" #'arei-testing-run
  "C-n" #'arei-testing-menu
  "C-m" #'arei-testing-load-module-tests
  "C-p" #'arei-testing-load-project-tests)

(provide 'arei-testing)
;;; arei-testing.el ends here
