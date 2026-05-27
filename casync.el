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
(require 'generator)
(require 'iter2)

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


(defvar casync-error-handler #'casync--default-error-handler
  "Function called with one argument when an unhandled error escapes.
The argument is the error object as produced by `condition-case'.")

(defvar casync--run-queue nil
  "FIFO list of pending thunks; the head is the next to run.")

(defvar casync--run-queue-tail nil
  "Last cons of `casync--run-queue' for constant-time enqueue.")

(defvar casync--draining nil
  "Non-nil while `casync--run-pending' is draining the run queue.")

(defvar casync--in-go nil
  "Non-nil while executing inside a go block body.")

(defvar casync--timer nil
  "Pending timer that will drain the run queue, or nil.")

(defvar casync--external-pending 0
  "Count of outstanding timer or process operations the pump must await.")

(defun casync--default-error-handler (err)
  "Report ERR by logging it."
  (message "casync: unhandled error: %S" err))

(defun casync--enqueue (thunk)
  "Append THUNK to the run queue."
  (let ((cell (list thunk)))
    (if casync--run-queue-tail
        (setcdr casync--run-queue-tail cell)
      (setq casync--run-queue cell))
    (setq casync--run-queue-tail cell)))

(defun casync--dequeue ()
  "Remove and return the next thunk from the run queue, or nil."
  (when casync--run-queue
    (prog1 (car casync--run-queue)
      (setq casync--run-queue (cdr casync--run-queue))
      (unless casync--run-queue
        (setq casync--run-queue-tail nil)))))

(defun casync--ensure-timer ()
  "Schedule a drain of the run queue unless one is already pending."
  (unless (or casync--draining casync--timer)
    (setq casync--timer (run-at-time 0 nil #'casync--drain-timer))))

(defun casync--drain-timer ()
  "Timer callback that drains the run queue."
  (setq casync--timer nil)
  (casync--run-pending))

(defun casync--dispatch (thunk)
  "Schedule THUNK to run on the scheduler."
  (casync--enqueue thunk)
  (casync--ensure-timer))

(defun casync--run-one (thunk)
  "Run THUNK, routing any error to `casync-error-handler'."
  (condition-case err
      (funcall thunk)
    (error (funcall casync-error-handler err))))

(defun casync--run-pending ()
  "Drain the run queue, running each pending thunk in order."
  (let ((casync--draining t)
        (thunk nil))
    (while (setq thunk (casync--dequeue))
      (casync--run-one thunk))))

(defun casync--pump (donep &optional timeout)
  "Drive the scheduler until DONEP returns non-nil.
TIMEOUT, when non-nil, is the maximum number of seconds to wait.
Signal an error when called from within the scheduler or a go block,
when TIMEOUT elapses, or when no progress can be made."
  (when casync--draining
    (error "casync: blocking operation called inside scheduler"))
  (when casync--in-go
    (error "casync: blocking operation called inside go block"))
  (let ((deadline (and timeout (+ (float-time) timeout))))
    (catch 'casync--pump-done
      (while t
        (casync--run-pending)
        (when (funcall donep)
          (throw 'casync--pump-done t))
        (when (and deadline (> (float-time) deadline))
          (error "casync: pump timeout"))
        (if (> casync--external-pending 0)
            (accept-process-output nil 0.05)
          (error "casync: deadlock, no pending work"))))))


(defconst casync--max-queue-size 1024
  "Maximum number of pending puts or takes a channel may hold.")

(cl-defstruct (casync--handler
               (:constructor casync--make-handler)
               (:copier nil))
  flag
  fn
  (blockable t))

(defun casync--handler-active-p (handler)
  "Return non-nil when HANDLER has not yet committed."
  (car (casync--handler-flag handler)))

(defun casync--handler-commit (handler)
  "Mark HANDLER committed and return its callback function."
  (setcar (casync--handler-flag handler) nil)
  (casync--handler-fn handler))

(cl-defstruct (casync-channel
               (:constructor casync--make-channel)
               (:copier nil))
  buf
  (takes nil)
  (puts nil)
  (closed nil))

(defun casync-chan (&optional buffer)
  "Return a new channel.
BUFFER is nil for an unbuffered channel, a positive integer for a
fixed buffer of that size, or a buffer object."
  (casync--make-channel
   :buf (cond
         ((null buffer) nil)
         ((integerp buffer)
          (if (> buffer 0)
              (casync-buffer buffer)
            (error "casync-chan: buffer size must be positive")))
         ((casync--abuf-p buffer) buffer)
         (t (error "casync-chan: invalid buffer %S" buffer)))))

(defun casync-promise-chan ()
  "Return a promise channel delivering at most one value to all takers."
  (casync--make-channel :buf (casync-promise-buffer)))

(defun casync--box (val)
  "Wrap VAL as a completed-operation box."
  (cons 'casync--box val))

(defun casync--unbox (box)
  "Return the value wrapped in BOX."
  (cdr box))

(defun casync--channel-poll-active-take (ch)
  "Pop and return the first active pending taker of CH, or nil."
  (let ((result nil))
    (while (and (not result) (casync-channel-takes ch))
      (let ((handler (pop (casync-channel-takes ch))))
        (when (casync--handler-active-p handler)
          (setq result handler))))
    result))

(defun casync--channel-poll-active-put (ch)
  "Pop and return the first active pending put (VAL . HANDLER) of CH, or nil."
  (let ((result nil))
    (while (and (not result) (casync-channel-puts ch))
      (let ((put (pop (casync-channel-puts ch))))
        (when (casync--handler-active-p (cdr put))
          (setq result put))))
    result))

(defun casync--channel-flush-buffer-to-takers (ch)
  "Deliver buffered values of CH to any waiting takers."
  (let ((buf (casync-channel-buf ch)))
    (while (and buf
                (> (casync-buffer-count buf) 0)
                (casync-channel-takes ch))
      (let ((taker (casync--channel-poll-active-take ch)))
        (when taker
          (let ((take-cb (casync--handler-commit taker))
                (val (casync-buffer-remove! buf)))
            (casync--dispatch (lambda () (funcall take-cb val)))))))))

(defun casync--channel-fill-buffer-from-puts (ch)
  "Move pending puts of CH into its buffer while room remains."
  (let ((buf (casync-channel-buf ch)))
    (while (and buf
                (not (casync-buffer-full-p buf))
                (casync-channel-puts ch))
      (let ((put (casync--channel-poll-active-put ch)))
        (when put
          (let ((put-cb (casync--handler-commit (cdr put))))
            (casync-buffer-add! buf (car put))
            (casync--dispatch (lambda () (funcall put-cb t)))))))))

(defun casync--channel-take! (ch handler enqueue)
  "Attempt a take on CH for HANDLER.
Return a box when the take completes, or nil when it parks.
When ENQUEUE is non-nil and the take cannot complete, park HANDLER."
  (when (casync--handler-active-p handler)
    (let ((buf (casync-channel-buf ch)))
      (cond
       ((and buf (> (casync-buffer-count buf) 0))
        (casync--handler-commit handler)
        (let ((val (casync-buffer-remove! buf)))
          (casync--channel-fill-buffer-from-puts ch)
          (casync--box val)))
       (t
        (let ((put (casync--channel-poll-active-put ch)))
          (cond
           (put
            (let ((put-cb (casync--handler-commit (cdr put))))
              (casync--handler-commit handler)
              (casync--dispatch (lambda () (funcall put-cb t)))
              (casync--box (car put))))
           ((casync-channel-closed ch)
            (casync--handler-commit handler)
            (casync--box nil))
           (enqueue
            (when (casync--handler-blockable handler)
              (when (>= (length (casync-channel-takes ch)) casync--max-queue-size)
                (error "casync: too many pending takes"))
              (setf (casync-channel-takes ch)
                    (nconc (casync-channel-takes ch) (list handler))))
            nil)
           (t nil))))))))

(defun casync--channel-put! (ch val handler enqueue)
  "Attempt to put VAL on CH for HANDLER.
Return a box when the put completes, or nil when it parks.
When ENQUEUE is non-nil and the put cannot complete, park HANDLER."
  (when (null val)
    (error "casync: cannot put nil on a channel"))
  (when (casync--handler-active-p handler)
    (cond
     ((casync-channel-closed ch)
      (casync--handler-commit handler)
      (casync--box nil))
     (t
      (let ((buf (casync-channel-buf ch)))
        (cond
         ((and buf (not (casync-buffer-full-p buf)))
          (casync--handler-commit handler)
          (casync-buffer-add! buf val)
          (casync--channel-flush-buffer-to-takers ch)
          (casync--box t))
         (t
          (let ((taker (casync--channel-poll-active-take ch)))
            (cond
             (taker
              (let ((take-cb (casync--handler-commit taker)))
                (casync--handler-commit handler)
                (casync--dispatch (lambda () (funcall take-cb val)))
                (casync--box t)))
             (enqueue
              (when (casync--handler-blockable handler)
                (when (>= (length (casync-channel-puts ch)) casync--max-queue-size)
                  (error "casync: too many pending puts"))
                (setf (casync-channel-puts ch)
                      (nconc (casync-channel-puts ch) (list (cons val handler)))))
              nil)
             (t nil))))))))))

(defun casync-take! (ch fn &optional on-caller)
  "Take a value from CH and call FN with it.
When ON-CALLER is non-nil and a value is immediately available, call
FN synchronously; otherwise FN runs on the scheduler."
  (let* ((handler (casync--make-handler :flag (list t) :fn fn))
         (box (casync--channel-take! ch handler t)))
    (when box
      (let ((val (casync--unbox box)))
        (if on-caller
            (funcall fn val)
          (casync--dispatch (lambda () (funcall fn val))))))
    nil))

(defun casync-put! (ch val fn &optional on-caller)
  "Put VAL on CH and call FN with t on success or nil when CH is closed.
When ON-CALLER is non-nil and the put completes immediately, call FN
synchronously; otherwise FN runs on the scheduler."
  (let* ((handler (casync--make-handler :flag (list t) :fn fn))
         (box (casync--channel-put! ch val handler t)))
    (when box
      (let ((ret (casync--unbox box)))
        (if on-caller
            (funcall fn ret)
          (casync--dispatch (lambda () (funcall fn ret))))))
    nil))

(defun casync-offer! (ch val)
  "Put VAL on CH without blocking.
Return t when the value was accepted, nil otherwise."
  (let* ((handler (casync--make-handler :flag (list t) :fn #'ignore :blockable nil))
         (box (casync--channel-put! ch val handler nil)))
    (and box (casync--unbox box))))

(defun casync-poll! (ch)
  "Take a value from CH without blocking.
Return the value, or nil when none is immediately available."
  (let* ((handler (casync--make-handler :flag (list t) :fn #'ignore :blockable nil))
         (box (casync--channel-take! ch handler nil)))
    (and box (casync--unbox box))))

(defun casync-close! (ch)
  "Close CH.  Pending takers receive buffered values then nil.  Return nil."
  (unless (casync-channel-closed ch)
    (setf (casync-channel-closed ch) t)
    (let ((buf (casync-channel-buf ch))
          (taker nil))
      (while (setq taker (casync--channel-poll-active-take ch))
        (let ((take-cb (casync--handler-commit taker))
              (val (when (and buf (> (casync-buffer-count buf) 0))
                     (casync-buffer-remove! buf))))
          (casync--dispatch (lambda () (funcall take-cb val)))))
      (when buf (casync-buffer-close! buf))))
  nil)

(defun casync-<!! (ch)
  "Take a value from CH, pumping the scheduler until one arrives."
  (let ((got nil) (result nil))
    (casync-take! ch (lambda (v) (setq result v got t)))
    (casync--pump (lambda () got))
    result))

(defun casync->!! (ch val)
  "Put VAL on CH, pumping the scheduler until the put completes.
Return t on success or nil when CH is closed."
  (let ((got nil) (result nil))
    (casync-put! ch val (lambda (r) (setq result r got t)))
    (casync--pump (lambda () got))
    result))


(defmacro casync-<! (ch)
  "Park the current go block until a value is taken from CH.
Valid only inside the body of `casync-go'."
  `(iter-yield (list :take ,ch)))

(defmacro casync->! (ch val)
  "Park the current go block until VAL is put on CH.
Valid only inside the body of `casync-go'."
  `(iter-yield (list :put ,ch ,val)))

(defmacro casync-go (&rest body)
  "Run BODY asynchronously as a go block.
Return a channel that yields BODY's value and then closes."
  (declare (indent 0))
  `(casync--run-go (iter2-lambda () ,@body)))

(defun casync--run-go (machine)
  "Start go MACHINE and return its result channel."
  (let ((it (funcall machine))
        (result-ch (casync-chan 1)))
    (casync--dispatch (lambda () (casync--go-resume it result-ch nil)))
    result-ch))

(defun casync--deliver-go-result (result-ch value)
  "Deliver VALUE on RESULT-CH and close it."
  (when (not (null value))
    (casync-put! result-ch value #'ignore))
  (casync-close! result-ch))

(defun casync--go-resume (it result-ch value)
  "Resume go iterator IT with VALUE, dispatching its next park request."
  (let ((request
         (condition-case err
             (let ((casync--in-go t))
               (cons 'request (iter2-next it value)))
           (iter-end-of-sequence
            (casync--deliver-go-result result-ch (cdr err))
            nil)
           (error
            (casync-close! result-ch)
            (funcall casync-error-handler err)
            nil))))
    (when request
      (casync--go-dispatch-request it result-ch (cdr request)))))

(defun casync--go-dispatch-request (it result-ch request)
  "Perform park REQUEST for go iterator IT bound to RESULT-CH."
  (pcase request
    (`(:take ,ch)
     (casync-take! ch (lambda (v) (casync--go-resume it result-ch v))))
    (`(:put ,ch ,val)
     (casync-put! ch val (lambda (ret) (casync--go-resume it result-ch ret))))
    (_ (error "casync: unknown park request %S" request))))

(provide 'casync)

;;; casync.el ends here
