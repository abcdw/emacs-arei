;;; arei-nrepl.el --- nREPL and bencode related functions -*- lexical-binding:t; coding:utf-8 -*-

;; Copyright © 2012-2013 Tim King, Phil Hagelberg, Bozhidar Batsov
;; Copyright © 2013-2023 Bozhidar Batsov, Artur Malabarba and CIDER contributors
;; Copyright © 2023 Andrew Tropin <andrew@trop.in>

;; Author: Andrew Tropin <andrew@trop.in>
;;
;; URL: https://trop.in/rde
;; Keywords: nrepl

;; This program is free software: you can redistribute it and/or modify
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

;; Provide nREPL and bencode related functions

;;; Code:

(require 'cl-lib)

(defun nrepl-dict (&rest key-vals)
  "Create nREPL dict from KEY-VALS."
  (cons 'dict key-vals))

(defun nrepl-dict-from-hash (hash)
  "Create nREPL dict from HASH."
  (let ((dict (nrepl-dict)))
    (maphash (lambda (k v) (nrepl-dict-put dict k v)) hash)
    dict))

(defun nrepl-dict-p (object)
  "Return t if OBJECT is an nREPL dict."
  (and (listp object)
       (eq (car object) 'dict)))

(defun nrepl-dict-empty-p (dict)
  "Return t if nREPL dict DICT is empty."
  (null (cdr dict)))

(defun nrepl-dict-contains (dict key)
  "Return nil if nREPL dict DICT doesn't contain KEY.
If DICT does contain KEY, then a non-nil value is returned.  Due to the
current implementation, this return value is the tail of DICT's key-list
whose car is KEY.  Comparison is done with `equal'."
  (member key (nrepl-dict-keys dict)))

(defun nrepl-dict-get (dict key &optional default)
  "Get from DICT value associated with KEY, optional DEFAULT if KEY not in DICT.
If dict is nil, return nil.  If DEFAULT not provided, and KEY not in DICT,
return nil.  If DICT is not an nREPL dict object, an error is thrown."
  (when dict
    (if (nrepl-dict-p dict)
        (if (nrepl-dict-contains dict key)
            (lax-plist-get (cdr dict) key)
          default)
      (error "Not an nREPL dict object: %s" dict))))

(defun nrepl-dict-put (dict key value)
  "Associate in DICT, KEY to VALUE.
Return new dict.  Dict is modified by side effects."
  (if (null dict)
      `(dict ,key ,value)
    (if (not (nrepl-dict-p dict))
        (error "Not an nREPL dict object: %s" dict)
      (setcdr dict (lax-plist-put (cdr dict) key value))
      dict)))

(defun nrepl-dict-keys (dict)
  "Return all the keys in the nREPL DICT."
  (if (nrepl-dict-p dict)
      (cl-loop for l on (cdr dict) by #'cddr
               collect (car l))
    (error "Not an nREPL dict")))

(defun nrepl-dict-vals (dict)
  "Return all the values in the nREPL DICT."
  (if (nrepl-dict-p dict)
      (cl-loop for l on (cdr dict) by #'cddr
               collect (cadr l))
    (error "Not an nREPL dict")))

(defun nrepl-dict-map (fn dict)
  "Map FN on nREPL DICT.
FN must accept two arguments key and value."
  (if (nrepl-dict-p dict)
      (cl-loop for l on (cdr dict) by #'cddr
               collect (funcall fn (car l) (cadr l)))
    (error "Not an nREPL dict")))

(defun nrepl-dict-merge (dict1 dict2)
  "Destructively merge DICT2 into DICT1.
Keys in DICT2 override those in DICT1."
  (let ((base (or dict1 '(dict))))
    (nrepl-dict-map (lambda (k v)
                      (nrepl-dict-put base k v))
                    (or dict2 '(dict)))
    base))

(defun nrepl-dict-get-in (dict keys)
  "Return the value in a nested DICT.
KEYS is a list of keys.  Return nil if any of the keys is not present or if
any of the values is nil."
  (let ((out dict))
    (while (and keys out)
      (setq out (nrepl-dict-get out (pop keys))))
    out))

(defun nrepl-dict-flat-map (function dict)
  "Map FUNCTION over DICT and flatten the result.
FUNCTION follows the same restrictions as in `nrepl-dict-map', and it must
also always return a sequence (since the result will be flattened)."
  (when dict
    (apply #'append (nrepl-dict-map function dict))))

(defun nrepl-dict-filter (function dict)
  "For all key-values of DICT, return new dict where FUNCTION returns non-nil.

FUNCTION should be a function taking two arguments, key and value."
  (let ((new-map (nrepl-dict))
        (keys (nrepl-dict-keys dict)))
    (dolist (key keys)
      (let ((val (nrepl-dict-get dict key)))
        (when (funcall function key val)
          (nrepl-dict-put new-map key val))))
    new-map))

(defmacro nrepl-dbind-response (response keys &rest body)
  "Destructure an nREPL RESPONSE dict.
Bind the value of the provided KEYS and execute BODY."
  (declare (debug (form (&rest symbolp) body)))
  `(let ,(cl-loop for key in keys
                  collect `(,key (nrepl-dict-get ,response ,(format "%s" key))))
     ,@body))
(put 'nrepl-dbind-response 'lisp-indent-function 2)


;;;
;;; More specific functions
;;;

(defun nrepl--cons (car list-or-dict)
  "Generic cons of CAR to LIST-OR-DICT."
  (if (eq (car list-or-dict) 'dict)
      (cons 'dict (cons car (cdr list-or-dict)))
    (cons car list-or-dict)))

(defun nrepl--nreverse (list-or-dict)
  "Generic `nreverse' which works on LIST-OR-DICT."
  (if (eq (car list-or-dict) 'dict)
      (cons 'dict (nreverse (cdr list-or-dict)))
    (nreverse list-or-dict)))

(defun nrepl--push (obj stack)
  "Cons OBJ to the top element of the STACK."
  ;; stack is assumed to be a list
  (if (eq (caar stack) 'dict)
      (cons (cons 'dict (cons obj (cdar stack)))
            (cdr stack))
    (cons (if (null stack)
              obj
            (cons obj (car stack)))
          (cdr stack))))
;;;
;;; Bencode
;;;

(cl-defstruct (nrepl-response-queue
               (:include queue)
               (:constructor nil)
               (:constructor nrepl-response-queue (&optional stub)))
  stub)

(put 'nrepl-response-queue 'function-documentation
     "Create queue object used by nREPL to store decoded server responses.
The STUB slot stores a stack of nested, incompletely parsed objects.")

(defun nrepl--bdecode-list (&optional stack)
  "Decode a bencode list or dict starting at point.
STACK is as in `nrepl--bdecode-1'."
  ;; skip leading l or d
  (forward-char 1)
  (let* ((istack (nrepl--bdecode-1 stack))
         (pos0 (point))
         (info (car istack)))
    (while (null info)
      (setq istack (nrepl--bdecode-1 (cdr istack))
            pos0 (point)
            info (car istack)))
    (cond ((eq info :e)
           (cons nil (cdr istack)))
          ((eq info :stub)
           (goto-char pos0)
           istack)
          (t istack))))

(defun nrepl--bdecode-1 (&optional stack)
  "Decode one elementary bencode object starting at point.
Bencoded object is either list, dict, integer or string.  See
http://en.wikipedia.org/wiki/Bencode#Encoding_algorithm for the encoding
rules.

STACK is a list of so far decoded components of the current message.  Car
of STACK is the innermost incompletely decoded object.  The algorithm pops
this list when inner object was completely decoded or grows it by one when
new list or dict was encountered.

The returned value is of the form (INFO . STACK) where INFO is
:stub, nil, :end or :eob and STACK is either an incomplete parsing state as
above (INFO is :stub, nil or :eob) or a list of one component representing
the completely decoded message (INFO is :end).  INFO is nil when an
elementary non-root object was successfully decoded.  INFO is :end when this
object is a root list or dict."
  (cond
   ;; list
   ((eq (char-after) ?l)
    (nrepl--bdecode-list (cons () stack)))
   ;; dict
   ((eq (char-after) ?d)
    (nrepl--bdecode-list (cons '(dict) stack)))
   ;; end of a list or a dict
   ((eq (char-after) ?e)
    (forward-char 1)
    (cons (if (cdr stack) :e :end)
          (nrepl--push (nrepl--nreverse (car stack))
                       (cdr stack))))
   ;; string
   ((looking-at "\\([0-9]+\\):")
    (let ((pos0 (point))
          (beg (goto-char (match-end 0)))
          (end (byte-to-position (+ (position-bytes (point))
                                    (string-to-number (match-string 1))))))
      (if (null end)
          (progn (goto-char pos0)
                 (cons :stub stack))
        (goto-char end)
        ;; normalise any platform-specific newlines
        (let* ((original (buffer-substring-no-properties beg end))
               (result (replace-regexp-in-string "\r\n\\|\n\r\\|\r" "\n" original)))
          (cons nil (nrepl--push result stack))))))
   ;; integer
   ((looking-at "i\\(-?[0-9]+\\)e")
    (goto-char (match-end 0))
    (cons nil (nrepl--push (string-to-number (match-string 1))
                           stack)))
   ;; should happen in tests only as eobp is checked in nrepl-bdecode.
   ((eobp)
    (cons :eob stack))
   ;; truncation in the middle of an integer or in 123: string prefix
   ((looking-at-p "[0-9i]")
    (cons :stub stack))
   ;; else, throw a quiet error
   (t
    (message "Invalid bencode message detected. See the %s buffer for details."
             nrepl-error-buffer-name)
    (nrepl-log-error
     (format "Decoder error at position %d (`%s'):"
             (point) (buffer-substring (point) (min (+ (point) 10) (point-max)))))
    (nrepl-log-error (buffer-string))
    (ding)
    ;; Ensure loop break and clean queues' states in nrepl-bdecode:
    (goto-char (point-max))
    (cons :end nil))))

(defun nrepl--bdecode-message (&optional stack)
  "Decode one full message starting at point.
STACK is as in `nrepl--bdecode-1'.  Return a cons (INFO . STACK)."
  (let* ((istack (nrepl--bdecode-1 stack))
         (info (car istack))
         (stack (cdr istack)))
    (while (or (null info)
               (eq info :e))
      (setq istack (nrepl--bdecode-1 stack)
            info (car istack)
            stack (cdr istack)))
    istack))

(defun nrepl--ensure-fundamental-mode ()
  "Enable `fundamental-mode' if it is not enabled already."
  (when (not (eq 'fundamental-mode major-mode))
    (fundamental-mode)))

(defun nrepl-bdecode (string-q &optional response-q)
  "Decode STRING-Q and place the results into RESPONSE-Q.
STRING-Q is either a queue of strings or a string.  RESPONSE-Q is a queue of
server requests (nREPL dicts).  STRING-Q and RESPONSE-Q are modified by side
effects.

Return a cons (STRING-Q . RESPONSE-Q) where STRING-Q is the original queue
containing the remainder of the input strings which could not be
decoded.  RESPONSE-Q is the original queue with successfully decoded messages
enqueued and with slot STUB containing a nested stack of an incompletely
decoded message or nil if the strings were completely decoded."
  (with-current-buffer (get-buffer-create " *nrepl-decoding*")
    ;; Don't needlessly call `fundamental-mode', to prevent needlessly firing
    ;; hooks. This fixes an issue with evil-mode where the cursor loses its
    ;; correct color.
    (nrepl--ensure-fundamental-mode)
    (erase-buffer)
    (if (queue-p string-q)
        (while (queue-head string-q)
          (insert (queue-dequeue string-q)))
      (insert string-q)
      (setq string-q (queue-create)))
    (goto-char 1)
    (unless response-q
      (setq response-q (nrepl-response-queue)))
    (let ((istack (nrepl--bdecode-message
                   (nrepl-response-queue-stub response-q))))
      (while (and (eq (car istack) :end)
                  (not (eobp)))
        (queue-enqueue response-q (cadr istack))
        (setq istack (nrepl--bdecode-message)))
      (unless (eobp)
        (queue-enqueue string-q (buffer-substring (point) (point-max))))
      (if (not (eq (car istack) :end))
          (setf (nrepl-response-queue-stub response-q) (cdr istack))
        (queue-enqueue response-q (cadr istack))
        (setf (nrepl-response-queue-stub response-q) nil))
      (erase-buffer)
      (cons string-q response-q))))

(defun nrepl-bencode (object)
  "Encode OBJECT with bencode.
Integers, lists and nrepl-dicts are treated according to bencode
specification.  Everything else is encoded as string."
  (cond
   ((integerp object) (format "i%de" object))
   ((nrepl-dict-p object) (format "d%se" (mapconcat #'nrepl-bencode (cdr object) "")))
   ((listp object) (format "l%se" (mapconcat #'nrepl-bencode object "")))
   (t (format "%s:%s" (string-bytes object) object))))

(provide 'arei-nrepl)
