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
         (casync--timer nil)
         (casync--timeouts nil)
         (casync--timeout-timer nil))
     ,@body))

(defmacro casync-deftest-thunk (name op expect &rest body)
  "Define cort test NAME comparing the result of BODY to EXPECT with OP.
BODY runs inside a lexically-compiled function so that `casync-go'
expands correctly, and with a fresh scheduler state."
  (declare (indent 3))
  (let ((fn (intern (concat "casync-test--" (symbol-name name)))))
    `(progn
       (defun ,fn () (casync-test-fresh ,@body))
       (cort-deftest ,name '((,op (,fn) ,expect))))))


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


(casync-deftest-thunk casync-go/returns-value := 10
  (let ((c (casync-chan 1)))
    (casync->!! c 10)
    (casync-<!! (casync-go (casync-<! c)))))

(casync-deftest-thunk casync-go/writes-work := 11
  (casync-<!! (casync-go
               (let ((c (casync-chan 1)))
                 (casync->! c 11)
                 (casync-<! c)))))

(casync-deftest-thunk casync-go/nil-result :eq nil
  (casync-<!! (casync-go nil)))

(casync-deftest-thunk casync-go/take-in-let-binding := 42
  (let ((c (casync-chan 1)))
    (casync->!! c 42)
    (casync-<!! (casync-go (let ((x (casync-<! c))) x)))))

(casync-deftest-thunk casync-go/park-across-if :eq :yes
  (let ((c (casync-chan 1)))
    (casync->!! c t)
    (casync-<!! (casync-go (if (casync-<! c) :yes :no)))))

(casync-deftest-thunk casync-go/park-across-while :equal '(1 2 3)
  (let ((c (casync-chan 5)))
    (dolist (x '(1 2 3)) (casync->!! c x))
    (casync-close! c)
    (casync-<!! (casync-go
                 (let ((acc nil) (v nil))
                   (while (setq v (casync-<! c))
                     (push v acc))
                   (nreverse acc))))))

(casync-deftest-thunk casync-go/park-across-condition-case := 42
  (let ((c (casync-chan 1)))
    (casync->!! c 42)
    (casync-<!! (casync-go
                 (condition-case _e (casync-<! c) (error :err))))))

(casync-deftest-thunk casync-go/nests := 23
  (casync-<!! (casync-<!! (casync-go (casync-go 23)))))

(casync-deftest-thunk casync-go/body-error-routed-to-handler :eq 'error
  (let ((captured nil))
    (let ((casync-error-handler (lambda (e) (setq captured e))))
      (casync-go (error "boom"))
      (casync--pump (lambda () captured)))
    (car captured)))

(casync-deftest-thunk casync-go/body-error-closes-result :eq nil
  (let ((casync-error-handler #'ignore))
    (casync-<!! (casync-go (error "boom")))))

(casync-deftest-thunk casync-go/blocking-take-inside-go-errors :eq 'error
  (let ((captured nil))
    (let ((casync-error-handler (lambda (e) (setq captured (car e)))))
      (casync-go (casync-<!! (casync-chan)))
      (casync--pump (lambda () captured)))
    captured))

(casync-deftest-thunk casync-go/nested-lambda-park-errors :eq t
  (let ((captured nil))
    (let ((casync-error-handler (lambda (_e) (setq captured t))))
      (casync-go (mapcar (lambda (_x) (casync-<! (casync-chan))) '(1)))
      (casync--pump (lambda () captured)))
    captured))

(casync-deftest-thunk casync-promise-chan/fulfills-pending-takers :equal '(:val :val)
  (let ((c (casync-promise-chan)) (r1 :none) (r2 :none))
    (casync-take! c (lambda (v) (setq r1 v)))
    (casync-take! c (lambda (v) (setq r2 v)))
    (casync->!! c :val)
    (list r1 r2)))

(casync-deftest-thunk casync-promise-chan/later-puts-dropped :eq :val
  (let ((c (casync-promise-chan)))
    (casync->!! c :val)
    (casync->!! c :LOST)
    (casync-<!! c)))

(casync-deftest-thunk casync-promise-chan/takes-keep-returning-value :equal '(:val :val :val)
  (let ((c (casync-promise-chan)))
    (casync->!! c :val)
    (list (casync-<!! c) (casync-<!! c) (casync-<!! c))))

(casync-deftest-thunk casync-promise-chan/close-after-value-keeps-value :equal '(:val :val)
  (let ((c (casync-promise-chan)))
    (casync->!! c :val)
    (casync-close! c)
    (list (casync-<!! c) (casync-<!! c))))

(casync-deftest-thunk casync-promise-chan/close-before-value-pending-takers-nil :equal '(nil nil)
  (let ((c (casync-promise-chan)) (r1 :none) (r2 :none))
    (casync-take! c (lambda (v) (setq r1 v)))
    (casync-take! c (lambda (v) (setq r2 v)))
    (casync-close! c)
    (casync--pump (lambda () (and (not (eq r1 :none)) (not (eq r2 :none)))))
    (list r1 r2)))

(casync-deftest-thunk casync-promise-chan/close-before-value-takes-nil :eq nil
  (let ((c (casync-promise-chan)))
    (casync-close! c)
    (casync-<!! c)))


(defun casync-test--ident (v)
  "Return a fresh channel already holding value V."
  (let ((c (casync-chan 1)))
    (casync-put! c v #'ignore)
    c))

(casync-deftest-thunk casync-alts/take := 42
  (let ((c (casync-chan 1)))
    (casync->!! c 42)
    (nth 0 (casync-<!! (casync-go (casync-alts! (list c)))))))

(casync-deftest-thunk casync-alts/returns-port :eq t
  (let ((c (casync-chan 1)))
    (casync->!! c 42)
    (let ((res (casync-<!! (casync-go (casync-alts! (list c))))))
      (eq (nth 1 res) c))))

(casync-deftest-thunk casync-alts/default :equal '(42 :default)
  (casync-<!! (casync-go (casync-alts! (list (casync-chan 1)) :default 42))))

(casync-deftest-thunk casync-alts/random-checks-all :eq t
  (let ((seen (casync-<!! (casync-go
                           (let ((acc nil) (i 0))
                             (while (< i 30)
                               (let ((r (casync-alts!
                                         (list (casync-test--ident :one)
                                               (casync-test--ident :two)
                                               (casync-test--ident :three)))))
                                 (push (nth 0 r) acc))
                               (setq i (1+ i)))
                             acc)))))
    (and (memq :one seen) (memq :two seen) (memq :three seen) t)))

(casync-deftest-thunk casync-alts/put-on-closed-no-error :eq :ok
  (let ((open (casync-chan)) (closed (casync-chan)))
    (casync-close! closed)
    (dotimes (_ 1500)
      (casync-alts!! (list open (list closed t)) :priority t))
    :ok))

(cort-deftest casync-alts/put-nil-invalid
  '((:cort-error error
     (casync-test-fresh
      (casync-alts!! (list (casync-chan) (list (casync-chan) nil)))))))

(casync-deftest-thunk casync-alt/take-clause :equal '(42 :foo)
  (let ((c (casync-chan 1)))
    (casync->!! c 42)
    (casync-<!! (casync-go (casync-alt! c ((v) (list v :foo)))))))

(casync-deftest-thunk casync-alt/default := 42
  (casync-<!! (casync-go (casync-alt! (casync-chan) ((v) :failed) :default 42))))


(casync-deftest-thunk casync-timeout/returns-nil :eq nil
  (casync-<!! (casync-timeout 20)))

(casync-deftest-thunk casync-timeout/interval :eq t
  (let ((start (float-time)))
    (casync-<!! (casync-timeout 100))
    (>= (- (float-time) start) 0.08)))

(casync-deftest-thunk casync-timeout/dedup :eq t
  (let ((casync-timeout-resolution 0.1))
    (let ((a (casync-timeout 1000)))
      (eq a (casync-timeout 950)))))

(casync-deftest-thunk casync-timeout/ordering :equal '(3 1 2 0)
  (let ((order nil)
        (t0 (casync-timeout 400))
        (t1 (casync-timeout 300))
        (t2 (casync-timeout 350))
        (t3 (casync-timeout 250)))
    (casync-go (casync-<! t0) (push 0 order))
    (casync-go (casync-<! t1) (push 1 order))
    (casync-go (casync-<! t2) (push 2 order))
    (casync-go (casync-<! t3) (push 3 order))
    (casync--pump (lambda () (= (length order) 4)))
    (nreverse order)))


(cort-deftest casync-go/park-outside-go-errors
  '((:cort-error error
     (casync-<! (casync-chan)))))

(cort-deftest casync-go/park-in-cleanup-rejected
  '((:cort-error error
     (eval '(casync-go
             (unwind-protect (casync-<! (casync-chan))
               (casync-<! (casync-chan))))
           t))))


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
