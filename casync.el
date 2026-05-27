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

(eval-and-compile
  (require 'cl-lib)
  (require 'generator)
  (require 'iter2))

(defgroup casync nil
  "Elisp port of clojure.core.async."
  :group 'convenience
  :link '(url-link :tag "Github" "https://github.com/conao3/casync.el"))


(defsubst casync--get (object key)
  "Return the value stored under KEY in tagged-plist OBJECT."
  (plist-get (cdr object) key))

(defun casync--set (object key value)
  "Store VALUE under KEY in tagged-plist OBJECT and return OBJECT."
  (setcdr object (plist-put (cdr object) key value))
  object)


(defun casync-buffer (n)
  "Return a fixed buffer that holds up to N items."
  (list 'casync--fixed :items nil :n n))

(defun casync-dropping-buffer (n)
  "Return a dropping buffer of size N that drops new items when full."
  (list 'casync--dropping :items nil :n n))

(defun casync-sliding-buffer (n)
  "Return a sliding buffer of size N that drops old items when full."
  (list 'casync--sliding :items nil :n n))

(defun casync-promise-buffer ()
  "Return a promise buffer holding a single value permanently."
  (list 'casync--promise :value nil :set nil))

(defun casync--buffer-p (object)
  "Return non-nil when OBJECT is a casync buffer."
  (memq (car-safe object)
        '(casync--fixed casync--dropping casync--sliding casync--promise)))

(defun casync-unblocking-buffer-p (buffer)
  "Return non-nil when adding to BUFFER never blocks."
  (and (memq (car buffer) '(casync--dropping casync--sliding casync--promise)) t))

(defun casync-buffer-count (buffer)
  "Return the number of items currently in BUFFER."
  (if (eq (car buffer) 'casync--promise)
      (if (casync--get buffer :set) 1 0)
    (length (casync--get buffer :items))))

(defun casync-buffer-full-p (buffer)
  "Return non-nil when BUFFER cannot accept more items without blocking."
  (and (eq (car buffer) 'casync--fixed)
       (>= (length (casync--get buffer :items))
           (casync--get buffer :n))))

(defun casync-buffer-add! (buffer item)
  "Add ITEM to BUFFER and return BUFFER."
  (pcase (car buffer)
    ('casync--fixed
     (when (casync-buffer-full-p buffer)
       (error "casync-buffer-add!: buffer full"))
     (casync--set buffer :items (append (casync--get buffer :items) (list item))))
    ('casync--dropping
     (when (< (length (casync--get buffer :items)) (casync--get buffer :n))
       (casync--set buffer :items (append (casync--get buffer :items) (list item)))))
    ('casync--sliding
     (casync--set buffer :items (append (casync--get buffer :items) (list item)))
     (when (> (length (casync--get buffer :items)) (casync--get buffer :n))
       (casync--set buffer :items (cdr (casync--get buffer :items)))))
    ('casync--promise
     (unless (casync--get buffer :set)
       (casync--set buffer :value item)
       (casync--set buffer :set t))))
  buffer)

(defun casync-buffer-remove! (buffer)
  "Remove and return the next item from BUFFER."
  (if (eq (car buffer) 'casync--promise)
      (casync--get buffer :value)
    (let ((items (casync--get buffer :items)))
      (when (null items)
        (error "casync-buffer-remove!: buffer empty"))
      (casync--set buffer :items (cdr items))
      (car items))))

(defun casync-buffer-close! (_buffer)
  "Notify a buffer that its channel is closing.  Return nil."
  nil)


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

(defun casync--make-handler (flag fn blockable)
  "Return a handler sharing commit cell FLAG, callback FN, blockable BLOCKABLE."
  (list 'casync--handler :flag flag :fn fn :blockable blockable))

(defun casync--handler-active-p (handler)
  "Return non-nil when HANDLER has not yet committed."
  (car (casync--get handler :flag)))

(defun casync--handler-commit (handler)
  "Mark HANDLER committed and return its callback function."
  (setcar (casync--get handler :flag) nil)
  (casync--get handler :fn))

(defun casync--handler-blockable (handler)
  "Return non-nil when HANDLER may park."
  (casync--get handler :blockable))

(defun casync-chan (&optional buffer)
  "Return a new channel.
BUFFER is nil for an unbuffered channel, a positive integer for a
fixed buffer of that size, or a buffer object."
  (list 'casync--channel
        :buf (cond
              ((null buffer) nil)
              ((integerp buffer)
               (if (> buffer 0)
                   (casync-buffer buffer)
                 (error "casync-chan: buffer size must be positive")))
              ((casync--buffer-p buffer) buffer)
              (t (error "casync-chan: invalid buffer %S" buffer)))
        :takes nil
        :puts nil
        :closed nil))

(defun casync-channel-p (object)
  "Return non-nil when OBJECT is a channel."
  (eq (car-safe object) 'casync--channel))

(defun casync-promise-chan ()
  "Return a promise channel delivering at most one value to all takers."
  (list 'casync--channel
        :buf (casync-promise-buffer)
        :takes nil
        :puts nil
        :closed nil))

(defun casync--box (val)
  "Wrap VAL as a completed-operation box."
  (cons 'casync--box val))

(defun casync--unbox (box)
  "Return the value wrapped in BOX."
  (cdr box))

(defun casync--channel-poll-active-take (ch)
  "Pop and return the first active pending taker of CH, or nil."
  (let ((result nil)
        (takes (casync--get ch :takes)))
    (while (and (not result) takes)
      (let ((handler (car takes)))
        (setq takes (cdr takes))
        (when (casync--handler-active-p handler)
          (setq result handler))))
    (casync--set ch :takes takes)
    result))

(defun casync--channel-poll-active-put (ch)
  "Pop and return the first active pending put (VAL . HANDLER) of CH, or nil."
  (let ((result nil)
        (puts (casync--get ch :puts)))
    (while (and (not result) puts)
      (let ((put (car puts)))
        (setq puts (cdr puts))
        (when (casync--handler-active-p (cdr put))
          (setq result put))))
    (casync--set ch :puts puts)
    result))

(defun casync--channel-flush-buffer-to-takers (ch)
  "Deliver buffered values of CH to any waiting takers."
  (let ((buf (casync--get ch :buf)))
    (while (and buf
                (> (casync-buffer-count buf) 0)
                (casync--get ch :takes))
      (let ((taker (casync--channel-poll-active-take ch)))
        (when taker
          (let ((take-cb (casync--handler-commit taker))
                (val (casync-buffer-remove! buf)))
            (casync--dispatch (lambda () (funcall take-cb val)))))))))

(defun casync--channel-fill-buffer-from-puts (ch)
  "Move pending puts of CH into its buffer while room remains."
  (let ((buf (casync--get ch :buf)))
    (while (and buf
                (not (casync-buffer-full-p buf))
                (casync--get ch :puts))
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
    (let ((buf (casync--get ch :buf)))
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
           ((casync--get ch :closed)
            (casync--handler-commit handler)
            (casync--box nil))
           (enqueue
            (when (casync--handler-blockable handler)
              (casync--set ch :takes
                           (cl-delete-if-not #'casync--handler-active-p
                                             (casync--get ch :takes)))
              (when (>= (length (casync--get ch :takes)) casync--max-queue-size)
                (error "casync: too many pending takes"))
              (casync--set ch :takes
                           (nconc (casync--get ch :takes) (list handler))))
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
     ((casync--get ch :closed)
      (casync--handler-commit handler)
      (casync--box nil))
     (t
      (let ((buf (casync--get ch :buf)))
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
                (casync--set ch :puts
                             (cl-delete-if-not
                              (lambda (p) (casync--handler-active-p (cdr p)))
                              (casync--get ch :puts)))
                (when (>= (length (casync--get ch :puts)) casync--max-queue-size)
                  (error "casync: too many pending puts"))
                (casync--set ch :puts
                             (nconc (casync--get ch :puts) (list (cons val handler)))))
              nil)
             (t nil))))))))))

(defun casync-take! (ch fn &optional on-caller)
  "Take a value from CH and call FN with it.
When ON-CALLER is non-nil and a value is immediately available, call
FN synchronously; otherwise FN runs on the scheduler."
  (let* ((handler (casync--make-handler (list t) fn t))
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
  (let* ((handler (casync--make-handler (list t) fn t))
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
  (let* ((handler (casync--make-handler (list t) #'ignore nil))
         (box (casync--channel-put! ch val handler nil)))
    (and box (casync--unbox box))))

(defun casync-poll! (ch)
  "Take a value from CH without blocking.
Return the value, or nil when none is immediately available."
  (let* ((handler (casync--make-handler (list t) #'ignore nil))
         (box (casync--channel-take! ch handler nil)))
    (and box (casync--unbox box))))

(defun casync-close! (ch)
  "Close CH.  Pending takers receive buffered values then nil.  Return nil."
  (unless (casync--get ch :closed)
    (casync--set ch :closed t)
    (let ((buf (casync--get ch :buf))
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


(eval-and-compile

(defmacro casync-<! (ch)
  "Park the current go block until a value is taken from CH.
Valid only inside the body of `casync-go'."
  `(iter-yield (list :take ,ch)))

(defmacro casync->! (ch val)
  "Park the current go block until VAL is put on CH.
Valid only inside the body of `casync-go'."
  `(iter-yield (list :put ,ch ,val)))

(defmacro casync-alts! (ops &rest opts)
  "Park the current go block until one of OPS completes.
OPS evaluates to a list whose elements are channels (take operations)
or (CHANNEL VALUE) lists (put operations).  OPTS may include
`:default VALUE' and `:priority t'.  Return (VALUE PORT).
Valid only inside the body of `casync-go'."
  `(iter-yield (list :alts ,ops (list ,@opts))))

(defmacro casync-recur (&rest vals)
  "Recur to the enclosing `casync-go-loop' with VALS as the new bindings."
  `(throw 'casync--go-loop-recur (list ,@vals)))

(defmacro casync-go-loop (bindings &rest body)
  "Like `casync-go' wrapping a loop over BINDINGS.
Inside BODY, call `casync-recur' in tail position to loop with new
binding values; any other value finishes the loop and is delivered."
  (declare (indent 1))
  (let ((vars (mapcar (lambda (b) (if (consp b) (car b) b)) bindings))
        (next (gensym "casync--loop-next-"))
        (ret (gensym "casync--loop-ret-"))
        (again (gensym "casync--loop-again-"))
        (idx -1))
    `(casync-go
      (let* (,@bindings (,ret nil) (,again t))
        (while ,again
          (setq ,again nil)
          (let ((,next (catch 'casync--go-loop-recur
                         (setq ,ret (progn ,@body))
                         'casync--go-loop-done)))
            (unless (eq ,next 'casync--go-loop-done)
              ,@(mapcar (lambda (v)
                          (setq idx (1+ idx))
                          `(setq ,v (nth ,idx ,next)))
                        vars)
              (setq ,again t))))
        ,ret))))

(defmacro casync-go (&rest body)
  "Run BODY asynchronously as a go block.
Return a channel that yields BODY's value and then closes."
  (declare (indent 0))
  `(casync--run-go (iter2-lambda () ,@body)))

)

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
    (`(:alts ,ops ,opts)
     (casync--alts ops opts (lambda (result) (casync--go-resume it result-ch result))))
    (_ (error "casync: unknown park request %S" request))))


(defun casync--shuffle (list)
  "Return a new list with the elements of LIST in random order."
  (let ((vec (vconcat list))
        (n (length list)))
    (dotimes (i n)
      (let ((j (+ i (random (- n i)))))
        (cl-rotatef (aref vec i) (aref vec j))))
    (append vec nil)))

(defun casync--alts (ops opts fret)
  "Try alt operations OPS with OPTS, delivering (VALUE PORT) to FRET.
OPS is a list of channels (take) or (CHANNEL VALUE) lists (put).
OPTS may include `:default VALUE' and `:priority t'."
  (let* ((has-default (plist-member opts :default))
         (priority (plist-get opts :priority))
         (flag (list t))
         (n (length ops))
         (order (if priority
                    (number-sequence 0 (1- n))
                  (casync--shuffle (number-sequence 0 (1- n)))))
         (completed nil))
    (catch 'casync--alts-done
      (dolist (i order)
        (let ((op (nth i ops)))
          (if (casync-channel-p op)
              (let* ((ch op)
                     (handler (casync--make-handler
                               flag
                               (lambda (v) (funcall fret (list v ch)))
                               (not has-default)))
                     (box (casync--channel-take! ch handler t)))
                (when box
                  (setq completed (list (casync--unbox box) ch))
                  (throw 'casync--alts-done nil)))
            (let* ((ch (car op))
                   (val (cadr op))
                   (handler (casync--make-handler
                             flag
                             (lambda (ret) (funcall fret (list ret ch)))
                             (not has-default)))
                   (box (casync--channel-put! ch val handler t)))
              (when box
                (setq completed (list (casync--unbox box) ch))
                (throw 'casync--alts-done nil)))))))
    (cond
     (completed
      (casync--dispatch (lambda () (funcall fret completed))))
     (has-default
      (when (car flag)
        (setcar flag nil)
        (let ((default (plist-get opts :default)))
          (casync--dispatch (lambda () (funcall fret (list default :default)))))))
     (t nil))))

(defun casync-alts!! (ops &rest opts)
  "Try alt operations OPS with OPTS, pumping until one completes.
Return (VALUE PORT)."
  (let ((got nil) (result nil))
    (casync--alts ops opts (lambda (r) (setq result r got t)))
    (casync--pump (lambda () got))
    result))

(defun casync--build-alt (alts-fn clauses)
  "Build an alt form invoking ALTS-FN over CLAUSES.
Each clause is a port followed by an action (BINDINGS BODY...), or
the pair `:default DEFAULT'."
  (let ((val-sym (gensym "casync--alt-val-"))
        (port-sym (gensym "casync--alt-port-"))
        (res-sym (gensym "casync--alt-res-"))
        (bindings nil)
        (ports nil)
        (conds nil)
        (has-default nil)
        (default-form nil)
        (rest clauses))
    (while rest
      (let ((head (pop rest))
            (action (pop rest)))
        (if (eq head :default)
            (setq has-default t
                  default-form action)
          (let ((psym (gensym "casync--alt-p-")))
            (push (list psym head) bindings)
            (push psym ports)
            (let ((vvar (nth 0 (car action)))
                  (cvar (nth 1 (car action)))
                  (body (cdr action)))
              (push `((eq ,port-sym ,psym)
                      (let (,@(when vvar `((,vvar ,val-sym)))
                            ,@(when cvar `((,cvar ,port-sym))))
                        ,@body))
                    conds))))))
    `(let* (,@(nreverse bindings)
            (,res-sym (,alts-fn (list ,@(nreverse ports))
                                ,@(when has-default (list :default default-form))))
            (,val-sym (nth 0 ,res-sym))
            (,port-sym (nth 1 ,res-sym)))
       (cond ,@(nreverse conds)
             ,@(when has-default
                 `(((eq ,port-sym :default) ,val-sym)))))))

(defmacro casync-alt! (&rest clauses)
  "Select among CLAUSES inside a go block.
Each clause is PORT followed by (BINDINGS BODY...), or `:default EXPR'."
  (casync--build-alt 'casync-alts! clauses))

(defmacro casync-alt!! (&rest clauses)
  "Select among CLAUSES, pumping the scheduler until one completes.
Each clause is PORT followed by (BINDINGS BODY...), or `:default EXPR'."
  (casync--build-alt 'casync-alts!! clauses))


(defvar casync-timeout-resolution 0.01
  "Granularity in seconds for coalescing timeout channels.")

(defvar casync--timeouts nil
  "List of (DEADLINE . CHANNEL) pending timeouts, DEADLINE a `float-time'.")

(defvar casync--timeout-timer nil
  "The single Emacs timer scheduled for the soonest pending timeout.")

(defun casync--timeout-reuse (deadline)
  "Return a pending timeout channel due within resolution of DEADLINE."
  (let ((found nil))
    (dolist (entry casync--timeouts found)
      (when (and (not found)
                 (>= (car entry) deadline)
                 (<= (- (car entry) deadline) casync-timeout-resolution))
        (setq found (cdr entry))))))

(defun casync--timeout-reschedule ()
  "Schedule the single timeout timer for the soonest pending deadline."
  (when casync--timeout-timer
    (cancel-timer casync--timeout-timer)
    (setq casync--timeout-timer nil))
  (when casync--timeouts
    (let ((soonest (apply #'min (mapcar #'car casync--timeouts))))
      (setq casync--timeout-timer
            (run-at-time (max 0 (- soonest (float-time))) nil
                         #'casync--timeout-fire)))))

(defun casync--timeout-fire ()
  "Close all timeout channels whose deadline has arrived, in deadline order."
  (setq casync--timeout-timer nil)
  (let ((now (float-time))
        (due nil))
    (setq casync--timeouts
          (cl-remove-if (lambda (entry)
                          (when (<= (car entry) (+ now casync-timeout-resolution))
                            (push entry due)
                            t))
                        casync--timeouts))
    (dolist (entry (sort due (lambda (a b) (< (car a) (car b)))))
      (setq casync--external-pending (max 0 (1- casync--external-pending)))
      (casync-close! (cdr entry)))
    (casync--timeout-reschedule)))

(defun casync-timeout (msecs)
  "Return a channel that closes after MSECS milliseconds."
  (let ((deadline (+ (float-time) (/ msecs 1000.0))))
    (or (casync--timeout-reuse deadline)
        (let ((ch (casync-chan)))
          (push (cons deadline ch) casync--timeouts)
          (setq casync--external-pending (1+ casync--external-pending))
          (casync--timeout-reschedule)
          ch))))


(defun casync-reduced (value)
  "Wrap VALUE to signal early termination of `casync-reduce'."
  (list 'casync--reduced :value value))

(defun casync-reduced-p (object)
  "Return non-nil when OBJECT marks an early reduction result."
  (eq (car-safe object) 'casync--reduced))

(defun casync--reduced-value (object)
  "Return the value wrapped by `casync-reduced' OBJECT."
  (casync--get object :value))

(cl-defun casync-onto-chan! (ch coll &optional (close t))
  "Put each element of COLL on CH, then close CH unless CLOSE is nil.
Return a channel that closes when the transfer finishes."
  (casync-go
    (dolist (x coll)
      (casync->! ch x))
    (when close
      (casync-close! ch))))

(defun casync-to-chan! (coll &optional buffer)
  "Return a channel that delivers each element of COLL and then closes.
BUFFER is passed to `casync-chan'."
  (let ((ch (casync-chan buffer)))
    (casync-onto-chan! ch coll t)
    ch))

(cl-defun casync-pipe (from to &optional (close t))
  "Copy values from channel FROM to channel TO.
Close TO when FROM closes unless CLOSE is nil.  Return TO."
  (casync-go-loop ()
    (let ((v (casync-<! from)))
      (if (null v)
          (when close (casync-close! to))
        (casync->! to v)
        (casync-recur))))
  to)

(defun casync-reduce (f init ch)
  "Reduce values taken from CH with F starting at INIT.
Return a channel delivering the final result.  A `casync-reduced'
result stops the reduction early."
  (casync-go-loop ((acc init))
    (let ((v (casync-<! ch)))
      (if (null v)
          acc
        (let ((acc2 (funcall f acc v)))
          (if (casync-reduced-p acc2)
              (casync--reduced-value acc2)
            (casync-recur acc2)))))))

(defun casync-into (coll ch)
  "Collect all values from CH into COLL and return a channel with the result."
  (casync-reduce (lambda (acc x) (append acc (list x))) coll ch))

(cl-defun casync-take (n ch &optional buffer)
  "Return a channel of the first N values from CH, then closed.
BUFFER is passed to `casync-chan'."
  (let ((out (casync-chan buffer)))
    (casync-go-loop ((i 0))
      (if (>= i n)
          (casync-close! out)
        (let ((v (casync-<! ch)))
          (if (null v)
              (casync-close! out)
            (casync->! out v)
            (casync-recur (1+ i))))))
    out))

(cl-defun casync-merge (chans &optional buffer)
  "Return a channel that receives values from all CHANS until each closes.
BUFFER is passed to `casync-chan'."
  (let ((out (casync-chan buffer)))
    (casync-go-loop ((cs chans))
      (if (null cs)
          (casync-close! out)
        (let* ((res (casync-alts! cs))
               (v (nth 0 res))
               (ch (nth 1 res)))
          (if (null v)
              (casync-recur (cl-remove ch cs :test #'eq))
            (casync->! out v)
            (casync-recur cs)))))
    out))

(cl-defun casync-split (pred ch &optional true-buffer false-buffer)
  "Split CH by PRED into two channels (TRUE-CH FALSE-CH).
TRUE-BUFFER and FALSE-BUFFER are passed to `casync-chan'."
  (let ((tc (casync-chan true-buffer))
        (fc (casync-chan false-buffer)))
    (casync-go-loop ()
      (let ((v (casync-<! ch)))
        (if (null v)
            (progn (casync-close! tc) (casync-close! fc))
          (casync->! (if (funcall pred v) tc fc) v)
          (casync-recur))))
    (list tc fc)))

(cl-defun casync-unique (ch &optional buffer)
  "Return a channel of values from CH with consecutive duplicates removed.
BUFFER is passed to `casync-chan'."
  (let ((out (casync-chan buffer)))
    (casync-go-loop ((last 'casync--none))
      (let ((v (casync-<! ch)))
        (if (null v)
            (casync-close! out)
          (if (equal v last)
              (casync-recur last)
            (casync->! out v)
            (casync-recur v)))))
    out))

(cl-defun casync-partition (n ch &optional buffer)
  "Return a channel of consecutive groups of N values from CH.
A trailing partial group is delivered.  BUFFER is passed to `casync-chan'."
  (let ((out (casync-chan buffer)))
    (casync-go-loop ((batch nil) (cnt 0))
      (let ((v (casync-<! ch)))
        (if (null v)
            (progn
              (when (> cnt 0) (casync->! out (nreverse batch)))
              (casync-close! out))
          (let ((b (cons v batch))
                (c (1+ cnt)))
            (if (>= c n)
                (progn (casync->! out (nreverse b)) (casync-recur nil 0))
              (casync-recur b c))))))
    out))

(cl-defun casync-partition-by (f ch &optional buffer)
  "Return a channel of groups of values from CH split when (F value) changes.
BUFFER is passed to `casync-chan'."
  (let ((out (casync-chan buffer)))
    (casync-go-loop ((batch nil) (last 'casync--none))
      (let ((v (casync-<! ch)))
        (if (null v)
            (progn
              (when batch (casync->! out (nreverse batch)))
              (casync-close! out))
          (let ((fv (funcall f v)))
            (if (or (eq last 'casync--none) (equal fv last))
                (casync-recur (cons v batch) fv)
              (casync->! out (nreverse batch))
              (casync-recur (list v) fv))))))
    out))

(defun casync-mult (ch)
  "Return a mult that broadcasts every value of CH to its taps."
  (let ((m (list 'casync--mult :ch ch :taps nil)))
    (casync-go-loop ()
      (let ((v (casync-<! ch)))
        (if (null v)
            (dolist (tap (casync--get m :taps))
              (casync-close! (car tap)))
          (dolist (tap (copy-sequence (casync--get m :taps)))
            (casync->! (car tap) v))
          (casync-recur))))
    m))

(cl-defun casync-tap (m ch &optional (close t))
  "Add CH as a tap of mult M.  Close CH when M's source closes unless CLOSE."
  (casync--set m :taps (cons (cons ch close) (casync--get m :taps)))
  ch)

(defun casync-untap (m ch)
  "Remove CH from the taps of mult M.  Return M."
  (casync--set m :taps
               (cl-remove ch (casync--get m :taps) :key #'car :test #'eq))
  m)

(defun casync-mix (out)
  "Return a mix that forwards values from its admixed inputs to OUT."
  (let ((m (list 'casync--mix
                 :out out
                 :ins nil
                 :change (casync-chan (casync-sliding-buffer 1)))))
    (casync-go-loop ()
      (let ((ins (casync--get m :ins)))
        (if (null ins)
            (progn (casync-<! (casync--get m :change)) (casync-recur))
          (let* ((res (casync-alts! (cons (casync--get m :change) ins)))
                 (v (nth 0 res))
                 (ch (nth 1 res)))
            (cond
             ((eq ch (casync--get m :change)) (casync-recur))
             ((null v)
              (casync--set m :ins (cl-remove ch (casync--get m :ins) :test #'eq))
              (casync-recur))
             (t (casync->! (casync--get m :out) v) (casync-recur)))))))
    m))

(defun casync-admix (m ch)
  "Add input channel CH to mix M.  Return M."
  (casync--set m :ins (cons ch (casync--get m :ins)))
  (casync-put! (casync--get m :change) t #'ignore)
  m)

(defun casync-unmix (m ch)
  "Remove input channel CH from mix M.  Return M."
  (casync--set m :ins (cl-remove ch (casync--get m :ins) :test #'eq))
  (casync-put! (casync--get m :change) t #'ignore)
  m)

(defun casync-pub (ch topic-fn)
  "Return a pub that routes values of CH by TOPIC-FN to subscribers."
  (let ((p (list 'casync--pub
                 :ch ch
                 :topic-fn topic-fn
                 :mults (make-hash-table :test 'equal))))
    (casync-go-loop ()
      (let ((v (casync-<! ch)))
        (if (null v)
            (maphash (lambda (_topic entry) (casync-close! (car entry)))
                     (casync--get p :mults))
          (let ((entry (gethash (funcall topic-fn v) (casync--get p :mults))))
            (when entry
              (casync->! (car entry) v))
            (casync-recur)))))
    p))

(cl-defun casync-sub (p topic ch &optional (close t))
  "Subscribe CH to TOPIC of pub P.  Close CH on source close unless CLOSE."
  (let ((entry (gethash topic (casync--get p :mults))))
    (unless entry
      (let ((src (casync-chan)))
        (setq entry (cons src (casync-mult src)))
        (puthash topic entry (casync--get p :mults))))
    (casync-tap (cdr entry) ch close)
    ch))

(provide 'casync)

;;; casync.el ends here
