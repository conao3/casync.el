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


(defmacro casync-test-fresh (&rest body)
  "Evaluate BODY with a fresh scheduler state."
  `(let ((casync--run-queue nil)
         (casync--run-queue-tail nil)
         (casync--external-pending 0)
         (casync--timer nil))
     ,@body))


(cort-deftest casync-scheduler/fifo-order
  '((:equal
     (casync-test-fresh
      (let ((log '()))
        (casync--dispatch (lambda () (push 1 log)))
        (casync--dispatch (lambda () (push 2 log)))
        (casync--dispatch (lambda () (push 3 log)))
        (casync--run-pending)
        (nreverse log)))
     '(1 2 3))))

(cort-deftest casync-scheduler/trampoline-nested-dispatch
  '((:equal
     (casync-test-fresh
      (let ((log '()))
        (casync--dispatch
         (lambda ()
           (push 1 log)
           (casync--dispatch (lambda () (push 2 log)))))
        (casync--run-pending)
        (nreverse log)))
     '(1 2))))

(cort-deftest casync-scheduler/error-routed-to-handler
  '((:eq
     (casync-test-fresh
      (let ((captured nil))
        (let ((casync-error-handler (lambda (e) (setq captured e))))
          (casync--dispatch (lambda () (error "boom")))
          (casync--run-pending))
        (car captured)))
     'error)))

(cort-deftest casync-scheduler/pump-until-done
  '((:eq
     (casync-test-fresh
      (let ((done nil))
        (casync--dispatch (lambda () (setq done t)))
        (casync--pump (lambda () done))
        done))
     t)))

(cort-deftest casync-scheduler/pump-deadlock-errors
  '((:cort-error error
     (casync-test-fresh (casync--pump (lambda () nil))))))

(cort-deftest casync-scheduler/blocking-inside-scheduler-errors
  '((:eq
     (casync-test-fresh
      (let ((captured nil))
        (casync--dispatch
         (lambda ()
           (condition-case _e
               (casync--pump (lambda () t))
             (error (setq captured t)))))
        (casync--run-pending)
        captured))
     t)))


(cort-deftest casync-channel/buffered-put-take
  '((:= (casync-test-fresh
         (let ((c (casync-chan 1)))
           (casync->!! c 42)
           (casync-<!! c)))
        42)))

(cort-deftest casync-channel/take-parks-then-put
  '((:eq (casync-test-fresh
          (let ((c (casync-chan)) (result :none))
            (casync-take! c (lambda (v) (setq result v)))
            (casync->!! c :test-val)
            result))
         :test-val)))

(cort-deftest casync-channel/put-parks-then-take
  '((:eq (casync-test-fresh
          (let ((c (casync-chan)))
            (casync-put! c :v (lambda (_r) nil))
            (casync-<!! c)))
         :v)))

(cort-deftest casync-channel/take-parks-then-buffered-put
  '((:eq (casync-test-fresh
          (let ((c (casync-chan 1)) (result :none))
            (casync-take! c (lambda (v) (setq result v)))
            (casync->!! c :buffered)
            result))
         :buffered)))

(cort-deftest casync-channel/offer-poll
  '((:equal (casync-test-fresh
             (let ((c (casync-chan 2)) (log '()))
               (push (casync-offer! c 1) log)
               (push (casync-offer! c 2) log)
               (push (casync-offer! c 3) log)
               (push (casync-<!! c) log)
               (push (casync-poll! c) log)
               (push (casync-poll! c) log)
               (nreverse log)))
            '(t t nil 1 2 nil))
    (:equal (casync-test-fresh
             (let ((c (casync-chan)) (log '()))
               (push (casync-offer! c 1) log)
               (push (casync-poll! c) log)
               (nreverse log)))
            '(nil nil))))

(cort-deftest casync-channel/put-nil-rejected
  '((:cort-error error
     (casync-test-fresh (casync->!! (casync-chan 1) nil)))))

(cort-deftest casync-channel/pending-put-limit
  '((:cort-error error
     (casync-test-fresh
      (let ((c (casync-chan)))
        (dotimes (_ 1024) (casync-put! c 1 #'ignore))
        (casync-put! c 42 #'ignore))))
    (:= (casync-test-fresh
         (let ((c (casync-chan)))
           (dotimes (x 1024) (casync-put! c x #'ignore))
           (ignore-errors (casync-put! c 9999 #'ignore))
           (casync-<!! c)))
        0)))

(cort-deftest casync-channel/close-drains-buffer-then-nil
  '((:equal (casync-test-fresh
             (let ((c (casync-chan 2)) (log '()))
               (casync->!! c :a)
               (casync->!! c :b)
               (casync-close! c)
               (push (casync-<!! c) log)
               (push (casync-<!! c) log)
               (push (casync-<!! c) log)
               (push (casync-<!! c) log)
               (nreverse log)))
            '(:a :b nil nil))))

(cort-deftest casync-channel/put-on-closed-returns-false
  '((:eq (casync-test-fresh
          (let ((c (casync-chan)))
            (casync-close! c)
            (casync->!! c :x)))
         nil)))

(cort-deftest casync-channel/close-releases-pending-takers
  '((:eq (casync-test-fresh
          (let ((c (casync-chan)) (result :none))
            (casync-take! c (lambda (v) (setq result v)))
            (casync-close! c)
            (casync--pump (lambda () (not (eq result :none))))
            result))
         nil)))


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
