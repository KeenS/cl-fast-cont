(in-package :cl-user)
(defpackage cl-fast-cont.transform
  (:use :cl)
  (:export #:to-ssa))
(in-package :cl-fast-cont.transform)


;; (expr) -> (let ((tmp1 expr1)...) tmpn)
(defun to-ssa (expr)
  (destructuring-bind (sym &rest binds) (to-gensym expr)
    `(let ,binds
       ,sym)))


;; (expr) -> (gensym . ((gensym expr) ...))
(defun to-gensym (expr)
  ;; FIXME: macro
  ;; FIXME: special forms
  (cond
    ((atom expr) (to-gensym-atom expr))
    (t (to-gensym-cons expr))))


;;; atom
;; (expr) -> (gensym . ((gensym expr) ...))
(defun to-gensym-atom (expr)
  (let ((tmp (gensym (concatenate 'string (princ-to-string expr) "@"))))
   (cons tmp (list (list tmp expr)))))

;; (expr) -> (gensym . ((gensym expr) ...))
(defun to-gensym-cons (expr)
  (let* ((name (car expr))
         (sym (gensym (concatenate 'string (princ-to-string name) (princ-to-string '-result@)))))
    (case name
      ((block)                (to-gensym-block sym expr))
      ((catch)                (to-gensym-catch sym expr))
      ((eval-when)            (to-gensym-eval-when sym expr))
      ((flet)                 (to-gensym-flet sym expr))
      ((function)             (to-gensym-function sym expr))
      ((go)                   (to-gensym-go sym expr))
      ((if)                   (to-gensym-if sym expr))
      ((labels)               (to-gensym-labels sym expr))
      ((let)                  (to-gensym-let sym expr))
      ((let*)                 (to-gensym-let* sym expr))
      ((load-time-value)      (to-gensym-load-time-value sym expr))
      ((locally)              (to-gensym-locally sym expr))
      ((macrolet)             (to-gensym-macrolet sym expr))
      ((multiple-value-call)  (to-gensym-multiple-value-call sym expr))
      ((multiple-value-prog1) (to-gensym-multiple-value-prog1 sym expr))
      ((progn)                (to-gensym-progn sym expr))
      ((progv)                (to-gensym-progv sym expr))
      ((quote)                (to-gensym-quote sym expr))
      ((return-from)          (to-gensym-return-from sym expr))
      ((setq)                 (to-gensym-setq sym expr))
      ((symbol-macrolet)      (to-gensym-symbol-macrolet sym expr))
      ((tagbody)              (to-gensym-tagbody sym expr))
      ((the)                  (to-gensym-the sym expr))
      ((throw)                (to-gensym-throw sym expr))
      ((unwind-protect)       (to-gensym-unwind-protect sym expr))
      #+openmcl
      ((ccl:compiler-let)     (to-gensym-ccl:compiler-let sym expr))
      ((values)               (to-gensym-values sym expr))
      ((values-list)          (to-gensym-values-list sym expr))
      (t                      (to-gensym-funcall sym expr)))))

(define-condition unimplemented ()
  ())

;; (gensym expr ((gensym_ expr_) ...)) -> (gensym . ((gensym_ expr_) ... (gensym expr)))
(defun make-result (sym expr env)
  ;; appending tail. too bad. FIXME.
  (cons sym (append env (list (list sym expr)))))

;;; BLOCK
(defun to-gensym-block (sym expr)
  (declare (ignore sym expr))
  (error 'unimplemented))
(defun to-gensym-catch (sym expr)
  (declare (ignore sym expr))
  (error 'unimplemented))
(defun to-gensym-eval-when (sym expr)
  (declare (ignore sym expr))
  (error 'unimplemented))
(defun to-gensym-flet (sym expr)
  (declare (ignore sym expr))
  (error 'unimplemented))

;;; FUNC
(defun to-gensym-function (sym expr)
  "transformation of `FUNCTION'.
See http://www.lispworks.com/documentation/lw70/CLHS/Body/s_fn.htm"
  (make-result sym expr ()))

;;; GO
(defun to-gensym-go (sym expr)
  "transformation of `GO'.
See http://www.lispworks.com/documentation/lw70/CLHS/Body/s_go.htm#go"
  (make-result sym expr ()))

(defun to-gensym-if (sym expr)
  (declare (ignore sym expr))
  (error 'unimplemented))
(defun to-gensym-labels (sym expr)
  (declare (ignore sym expr))
  (error 'unimplemented))
(defun to-gensym-let (sym expr)
  (declare (ignore sym expr))
  (error 'unimplemented))
(defun to-gensym-let* (sym expr)
  (declare (ignore sym expr))
  (error 'unimplemented))
(defun to-gensym-load-time-value (sym expr)
  (declare (ignore sym expr))
  (error 'unimplemented))
(defun to-gensym-locally (sym expr)
  (declare (ignore sym expr))
  (error 'unimplemented))
(defun to-gensym-macrolet (sym expr)
  (declare (ignore sym expr))
  (error 'unimplemented))
(defun to-gensym-multiple-value-call (sym expr)
  (declare (ignore sym expr))
  (error 'unimplemented))
(defun to-gensym-multiple-value-prog1 (sym expr)
  (declare (ignore sym expr))
  (error 'unimplemented))
(defun to-gensym-progn (sym expr)
  (declare (ignore sym expr))
  (error 'unimplemented))
(defun to-gensym-progv (sym expr)
  (declare (ignore sym expr))
  (error 'unimplemented))

;;; QUOTE
;; (expr) -> (gensym . ((gensym expr) ...))
(defun to-gensym-quote (sym expr)
  "transformation of `QUOTE'.
See http://www.lispworks.com/documentation/lw70/CLHS/Body/s_quote.htm#quote"
  (make-result sym expr ()))

(defun to-gensym-return-from (sym expr)
  (declare (ignore sym expr))
  (error 'unimplemented))
(defun to-gensym-setq (sym expr)
  (declare (ignore sym expr))
  (error 'unimplemented))
(defun to-gensym-symbol-macrolet (sym expr)
  (declare (ignore sym expr))
  (error 'unimplemented))
(defun to-gensym-tagbody (sym expr)
  (declare (ignore sym expr))
  (error 'unimplemented))
(defun to-gensym-the (sym expr)
  (declare (ignore sym expr))
  (error 'unimplemented))
(defun to-gensym-throw (sym expr)
  (declare (ignore sym expr))
  (error 'unimplemented))
(defun to-gensym-unwind-protect (sym expr)
  (declare (ignore sym expr))
  (error 'unimplemented))
#+openmcl
(defun to-gensym-ccl:compiler-let (sym expr)
  (error 'unimplemented))
(defun to-gensym-values (sym expr)
  (declare (ignore sym expr))
  (error 'unimplemented))
(defun to-gensym-values-list (sym expr)
  (declare (ignore sym expr))
  (error 'unimplemented))


;;; funtion call
;; (expr) -> (gensym . ((gensym expr) ...))
(defun to-gensym-funcall (sym expr)
  (destructuring-bind (name &rest args) expr
    ;; What's wanted: collect sym and pile up env
    (let* ((args (mapcar #'to-gensym args))
           (syms (mapcar #'car args))
           (envs (mapcar #'cdr args))
           (expr (cons name syms)))
      (make-result sym expr (apply #'append envs)))))
