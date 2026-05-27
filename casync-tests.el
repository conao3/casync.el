;;; casync-tests.el --- Test definitions for casync  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Naoya Yamashita

;; Author: Naoya Yamashita <conao3@gmail.com>
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

;; Test definitions for `casync'.


;;; Code:

(require 'cort)
(require 'casync)


(cort-deftest casync-buffer/fixed
  '((:equal
     (let ((b (casync-buffer 2)) (log '()))
       (push (casync-buffer-count b) log)
       (casync-buffer-add! b :1)
       (push (casync-buffer-count b) log)
       (casync-buffer-add! b :2)
       (push (casync-buffer-count b) log)
       (push (casync-buffer-remove! b) log)
       (push (casync-buffer-full-p b) log)
       (push (casync-buffer-count b) log)
       (push (casync-buffer-remove! b) log)
       (push (casync-buffer-count b) log)
       (nreverse log))
     '(0 1 2 :1 nil 1 :2 0))
    (:eq (casync-unblocking-buffer-p (casync-buffer 1)) nil)))

(cort-deftest casync-buffer/fixed-remove-empty-errors
  '((:cort-error error
     (casync-buffer-remove! (casync-buffer 2)))))

(cort-deftest casync-buffer/dropping
  '((:equal
     (let ((b (casync-dropping-buffer 2)) (log '()))
       (push (casync-buffer-count b) log)
       (casync-buffer-add! b :1)
       (push (casync-buffer-count b) log)
       (casync-buffer-add! b :2)
       (push (casync-buffer-count b) log)
       (push (casync-buffer-full-p b) log)
       (casync-buffer-add! b :3)
       (push (casync-buffer-count b) log)
       (push (casync-buffer-remove! b) log)
       (push (casync-buffer-count b) log)
       (push (casync-buffer-remove! b) log)
       (push (casync-buffer-count b) log)
       (nreverse log))
     '(0 1 2 nil 2 :1 1 :2 0))
    (:eq (casync-unblocking-buffer-p (casync-dropping-buffer 1)) t)))

(cort-deftest casync-buffer/sliding
  '((:equal
     (let ((b (casync-sliding-buffer 2)) (log '()))
       (push (casync-buffer-count b) log)
       (casync-buffer-add! b :1)
       (push (casync-buffer-count b) log)
       (casync-buffer-add! b :2)
       (push (casync-buffer-count b) log)
       (push (casync-buffer-full-p b) log)
       (casync-buffer-add! b :3)
       (push (casync-buffer-count b) log)
       (push (casync-buffer-remove! b) log)
       (push (casync-buffer-count b) log)
       (push (casync-buffer-remove! b) log)
       (push (casync-buffer-count b) log)
       (nreverse log))
     '(0 1 2 nil 2 :2 1 :3 0))
    (:eq (casync-unblocking-buffer-p (casync-sliding-buffer 1)) t)))

(cort-deftest casync-buffer/promise
  '((:equal
     (let ((b (casync-promise-buffer)) (log '()))
       (push (casync-buffer-count b) log)
       (casync-buffer-add! b :1)
       (push (casync-buffer-count b) log)
       (casync-buffer-add! b :2)
       (push (casync-buffer-count b) log)
       (push (casync-buffer-full-p b) log)
       (casync-buffer-add! b :3)
       (push (casync-buffer-count b) log)
       (push (casync-buffer-remove! b) log)
       (push (casync-buffer-count b) log)
       (push (casync-buffer-remove! b) log)
       (push (casync-buffer-close! b) log)
       (push (casync-buffer-remove! b) log)
       (nreverse log))
     '(0 1 1 nil 1 :1 1 :1 nil :1))
    (:eq (casync-unblocking-buffer-p (casync-promise-buffer)) t)))


;; (provide 'casync-tests)

;;; casync-tests.el ends here
