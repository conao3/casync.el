;;; casync.el --- Elisp port of clojure.core.async  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Naoya Yamashita

;; Author: Naoya Yamashita <conao3@gmail.com>
;; Version: 0.0.1
;; Keywords: convenience
;; Package-Requires: ((emacs "27.1") (iter2 "2.3") (async "1.9.9"))
;; URL: https://github.com/conao3/casync.el

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Elisp port of clojure.core.async.


;;; Code:

(require 'cl-lib)

(defgroup casync nil
  "Elisp port of clojure.core.async."
  :group 'convenience
  :link '(url-link :tag "Github" "https://github.com/conao3/casync.el"))


(cl-defstruct (casync--abuf (:constructor nil) (:copier nil)))

(cl-defstruct (casync--fixed-buffer
               (:include casync--abuf)
               (:constructor casync--make-fixed-buffer (n))
               (:copier nil))
  (items nil)
  n)

(cl-defstruct (casync--dropping-buffer
               (:include casync--abuf)
               (:constructor casync--make-dropping-buffer (n))
               (:copier nil))
  (items nil)
  n)

(cl-defstruct (casync--sliding-buffer
               (:include casync--abuf)
               (:constructor casync--make-sliding-buffer (n))
               (:copier nil))
  (items nil)
  n)

(cl-defstruct (casync--promise-buffer
               (:include casync--abuf)
               (:constructor casync--make-promise-buffer ())
               (:copier nil))
  (value nil)
  (set nil))

(defun casync-buffer (n)
  "Return a fixed buffer that holds up to N items."
  (casync--make-fixed-buffer n))

(defun casync-dropping-buffer (n)
  "Return a dropping buffer of size N that drops new items when full."
  (casync--make-dropping-buffer n))

(defun casync-sliding-buffer (n)
  "Return a sliding buffer of size N that drops old items when full."
  (casync--make-sliding-buffer n))

(defun casync-promise-buffer ()
  "Return a promise buffer holding a single value permanently."
  (casync--make-promise-buffer))

(cl-defgeneric casync-unblocking-buffer-p (buffer)
  "Return non-nil when adding to BUFFER never blocks.")

(cl-defmethod casync-unblocking-buffer-p ((_buffer casync--fixed-buffer)) nil)
(cl-defmethod casync-unblocking-buffer-p ((_buffer casync--dropping-buffer)) t)
(cl-defmethod casync-unblocking-buffer-p ((_buffer casync--sliding-buffer)) t)
(cl-defmethod casync-unblocking-buffer-p ((_buffer casync--promise-buffer)) t)

(cl-defgeneric casync-buffer-count (buffer)
  "Return the number of items currently in BUFFER.")

(cl-defmethod casync-buffer-count ((buffer casync--fixed-buffer))
  (length (casync--fixed-buffer-items buffer)))

(cl-defmethod casync-buffer-count ((buffer casync--dropping-buffer))
  (length (casync--dropping-buffer-items buffer)))

(cl-defmethod casync-buffer-count ((buffer casync--sliding-buffer))
  (length (casync--sliding-buffer-items buffer)))

(cl-defmethod casync-buffer-count ((buffer casync--promise-buffer))
  (if (casync--promise-buffer-set buffer) 1 0))

(cl-defgeneric casync-buffer-full-p (buffer)
  "Return non-nil when BUFFER cannot accept more items without blocking.")

(cl-defmethod casync-buffer-full-p ((buffer casync--fixed-buffer))
  (>= (length (casync--fixed-buffer-items buffer))
      (casync--fixed-buffer-n buffer)))

(cl-defmethod casync-buffer-full-p ((_buffer casync--dropping-buffer)) nil)
(cl-defmethod casync-buffer-full-p ((_buffer casync--sliding-buffer)) nil)
(cl-defmethod casync-buffer-full-p ((_buffer casync--promise-buffer)) nil)

(cl-defgeneric casync-buffer-add! (buffer item)
  "Add ITEM to BUFFER and return BUFFER.")

(cl-defmethod casync-buffer-add! ((buffer casync--fixed-buffer) item)
  (cl-assert (not (casync-buffer-full-p buffer)) t "casync-buffer-add!: buffer full")
  (setf (casync--fixed-buffer-items buffer)
        (append (casync--fixed-buffer-items buffer) (list item)))
  buffer)

(cl-defmethod casync-buffer-add! ((buffer casync--dropping-buffer) item)
  (when (< (length (casync--dropping-buffer-items buffer))
           (casync--dropping-buffer-n buffer))
    (setf (casync--dropping-buffer-items buffer)
          (append (casync--dropping-buffer-items buffer) (list item))))
  buffer)

(cl-defmethod casync-buffer-add! ((buffer casync--sliding-buffer) item)
  (setf (casync--sliding-buffer-items buffer)
        (append (casync--sliding-buffer-items buffer) (list item)))
  (when (> (length (casync--sliding-buffer-items buffer))
           (casync--sliding-buffer-n buffer))
    (setf (casync--sliding-buffer-items buffer)
          (cdr (casync--sliding-buffer-items buffer))))
  buffer)

(cl-defmethod casync-buffer-add! ((buffer casync--promise-buffer) item)
  (unless (casync--promise-buffer-set buffer)
    (setf (casync--promise-buffer-value buffer) item
          (casync--promise-buffer-set buffer) t))
  buffer)

(cl-defgeneric casync-buffer-remove! (buffer)
  "Remove and return the next item from BUFFER.")

(cl-defmethod casync-buffer-remove! ((buffer casync--fixed-buffer))
  (when (null (casync--fixed-buffer-items buffer))
    (error "casync-buffer-remove!: buffer empty"))
  (prog1 (car (casync--fixed-buffer-items buffer))
    (setf (casync--fixed-buffer-items buffer)
          (cdr (casync--fixed-buffer-items buffer)))))

(cl-defmethod casync-buffer-remove! ((buffer casync--dropping-buffer))
  (when (null (casync--dropping-buffer-items buffer))
    (error "casync-buffer-remove!: buffer empty"))
  (prog1 (car (casync--dropping-buffer-items buffer))
    (setf (casync--dropping-buffer-items buffer)
          (cdr (casync--dropping-buffer-items buffer)))))

(cl-defmethod casync-buffer-remove! ((buffer casync--sliding-buffer))
  (when (null (casync--sliding-buffer-items buffer))
    (error "casync-buffer-remove!: buffer empty"))
  (prog1 (car (casync--sliding-buffer-items buffer))
    (setf (casync--sliding-buffer-items buffer)
          (cdr (casync--sliding-buffer-items buffer)))))

(cl-defmethod casync-buffer-remove! ((buffer casync--promise-buffer))
  (casync--promise-buffer-value buffer))

(cl-defgeneric casync-buffer-close! (buffer)
  "Notify BUFFER that its channel is closing.  Return nil.")

(cl-defmethod casync-buffer-close! ((_buffer casync--abuf)) nil)

(provide 'casync)

;;; casync.el ends here
