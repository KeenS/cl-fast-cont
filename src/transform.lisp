(in-package :cl-user)
(defpackage cl-fast-cont.transform
  (:use :cl)
  (:export #:transform #:to-ssa))
(in-package :cl-fast-cont.transform)


(defmacro transform (expr &environment env)
  (to-ssa expr env))

;; (expr) -> (let ((tmp1 expr1) ... (tmpn exprn)) tmpn)
(defun to-ssa (expr env)
  (destructuring-bind (sym &rest binds) (to-gensym expr env)
    `(let ,binds
       ,sym)))


;; (expr env) -> (gensym . (... (gensym expr)))
(defun to-gensym (expr env)
  ;; FIXME: macro
  ;; FIXME: special forms
  (cond
    ((atom expr) (to-gensym-atom expr env))
    (t (to-gensym-cons expr env))))


;;; atom
;; (expr env) -> (gensym . (... (gensym expr)))
(defun to-gensym-atom (expr env)
  (declare (ignore env))
  (let ((tmp (gensym (concatenate 'string (princ-to-string expr) "@"))))
   (cons tmp (list (list tmp expr)))))

;; (expr env) -> (gensym . (... (gensym expr)))
(defun to-gensym-cons (expr env)
  (let* ((name (car expr))
         (sym (gensym (concatenate 'string (princ-to-string name) (princ-to-string '-result@)))))
    (case name
      ((block)                (to-gensym-block sym expr env))
      ((catch)                (to-gensym-catch sym expr env))
      ((eval-when)            (to-gensym-eval-when sym expr env))
      ((flet)                 (to-gensym-flet sym expr env))
      ((function)             (to-gensym-function sym expr env))
      ((go)                   (to-gensym-go sym expr env))
      ((if)                   (to-gensym-if sym expr env))
      ((labels)               (to-gensym-labels sym expr env))
      ((let)                  (to-gensym-let sym expr env))
      ((let*)                 (to-gensym-let* sym expr env))
      ((load-time-value)      (to-gensym-load-time-value sym expr env))
      ((locally)              (to-gensym-locally sym expr env))
      ((macrolet)             (to-gensym-macrolet sym expr env))
      ((multiple-value-call)  (to-gensym-multiple-value-call sym expr env))
      ((multiple-value-prog1) (to-gensym-multiple-value-prog1 sym expr env))
      ((progn)                (to-gensym-progn sym expr env))
      ((progv)                (to-gensym-progv sym expr env))
      ((quote)                (to-gensym-quote sym expr env))
      ((return-from)          (to-gensym-return-from sym expr env))
      ((setq)                 (to-gensym-setq sym expr env))
      ((symbol-macrolet)      (to-gensym-symbol-macrolet sym expr env))
      ((tagbody)              (to-gensym-tagbody sym expr env))
      ((the)                  (to-gensym-the sym expr env))
      ((throw)                (to-gensym-throw sym expr env))
      ((unwind-protect)       (to-gensym-unwind-protect sym expr env))
      #+openmcl
      ((ccl:compiler-let)     (to-gensym-ccl:compiler-let sym expr env))
      ((values)               (to-gensym-values sym expr env))
      ((values-list)          (to-gensym-values-list sym expr env))
      (t (multiple-value-bind (expantion expanded-p) (macroexpand-1 expr env)
           (if expanded-p
               (to-gensym expantion env)
               (to-gensym-funcall sym expantion env)))))))

(define-condition unimplemented ()
  ())

;; (gensym expr ((gensym_ expr_) ...)) -> (gensym . ((gensym_ expr_) ... (gensym expr)))
(defun make-result (sym expr vars)
  ;; appending tail. too bad. FIXME.
  (cons sym (append vars (list (list sym expr)))))

;;; BLOCK
(defun to-gensym-block (sym expr env)
  "transformation of `BLOCK`.
See http://www.lispworks.com/documentation/lw70/CLHS/Body/s_block.htm#block"
  (destructuring-bind (name blk-name &rest body) expr
    (let* ((body (to-ssa (cons 'progn body) env))
           (expr (cons name (cons blk-name body))))
      (make-result sym expr ()))))

(defun to-gensym-catch (sym expr env)
  (declare (ignore sym expr env))
  (error 'unimplemented))
(defun to-gensym-eval-when (sym expr env)
  (declare (ignore sym expr env))
  (error 'unimplemented))
(defun to-gensym-flet (sym expr env)
  (declare (ignore sym expr env))
  (error 'unimplemented))

;;; FUNCTION
(defun to-gensym-function (sym expr env)
  "transformation of `FUNCTION'.
See http://www.lispworks.com/documentation/lw70/CLHS/Body/s_fn.htm"
  (declare (ignore env))
  (make-result sym expr ()))

;;; GO
(defun to-gensym-go (sym expr env)
  "transformation of `GO'.
See http://www.lispworks.com/documentation/lw70/CLHS/Body/s_go.htm#go"
  (declare (ignore env))
  (make-result sym expr ()))

(defun to-gensym-if (sym expr env)
  (declare (ignore sym expr env))
  (error 'unimplemented))
(defun to-gensym-labels (sym expr env)
  (declare (ignore sym expr env))
  (error 'unimplemented))
(defun to-gensym-let (sym expr env)
  (declare (ignore sym expr env))
  (error 'unimplemented))
(defun to-gensym-let* (sym expr env)
  (declare (ignore sym expr env))
  (error 'unimplemented))
(defun to-gensym-load-time-value (sym expr env)
  (declare (ignore sym expr env))
  (error 'unimplemented))
(defun to-gensym-locally (sym expr env)
  (declare (ignore sym expr env))
  (error 'unimplemented))
(defun to-gensym-macrolet (sym expr env)
  (declare (ignore sym expr env))
  (error 'unimplemented))
(defun to-gensym-multiple-value-call (sym expr env)
  (declare (ignore sym expr env))
  (error 'unimplemented))
(defun to-gensym-multiple-value-prog1 (sym expr env)
  (declare (ignore sym expr env))
  (error 'unimplemented))
(defun to-gensym-progn (sym expr env)
  (declare (ignore sym expr env))
  (error 'unimplemented))
(defun to-gensym-progv (sym expr env)
  (declare (ignore sym expr env))
  (error 'unimplemented))

;;; QUOTE
(defun to-gensym-quote (sym expr env)
  "transformation of `QUOTE'.
See http://www.lispworks.com/documentation/lw70/CLHS/Body/s_quote.htm#quote"
  (declare (ignore env))
  (make-result sym expr ()))

;;; RETURN-FROM
(defun to-gensym-return-from (sym expr env)
  "transformation of `RETURN-FROM'.
See http://www.lispworks.com/documentation/lw70/CLHS/Body/s_ret_fr.htm#return-from"
  (destructuring-bind (name blk-name &optional arg) expr
    (if arg
        (destructuring-bind (gensym . vars) (to-gensym arg env)
          (make-result sym (list name blk-name gensym) vars))
        (make-result sym expr ()))))

;;; SETQ
(defun to-gensym-setq (sym expr env)
  "transformation of `SETQ'.
See http://www.lispworks.com/documentation/lw70/CLHS/Body/s_setq.htm#setq"
  (destructuring-bind (name &rest pairs) expr
    (let* ((vars (loop :for (_  expr_ . __) :on pairs :by #'cddr
                    :collect (to-gensym expr_ env)))
           (bindings (loop
                        :for (binding . _) :on pairs :by #'cddr
                        :for (sym_ .  __) :in vars
                        :append (list binding sym_))))
      (make-result sym (cons name bindings) (apply #'append (mapcar #'cadr vars))))))


(defun to-gensym-symbol-macrolet (sym expr env)
  (declare (ignore sym expr env))
  (error 'unimplemented))
(defun to-gensym-tagbody (sym expr env)
  (declare (ignore sym expr env))
  (error 'unimplemented))
(defun to-gensym-the (sym expr env)
  (declare (ignore sym expr env))
  (error 'unimplemented))
(defun to-gensym-throw (sym expr env)
  (declare (ignore sym expr env))
  (error 'unimplemented))
(defun to-gensym-unwind-protect (sym expr env)
  (declare (ignore sym expr env))
  (error 'unimplemented))
#+openmcl
(defun to-gensym-ccl:compiler-let (sym expr env)
  (error 'unimplemented))
(defun to-gensym-values (sym expr env)
  (declare (ignore sym expr env))
  (error 'unimplemented))
(defun to-gensym-values-list (sym expr env)
  (declare (ignore sym expr env))
  (error 'unimplemented))


;;; funtion call
;; (expr) -> (gensym . ((gensym expr) ...))
(defun to-gensym-funcall (sym expr env)
  (destructuring-bind (name &rest args) expr
    (let* ((args (mapcar (lambda (arg) (to-gensym arg env)) args))
           (syms (mapcar #'car args))
           (vars (mapcar #'cdr args))
           (expr (cons name syms)))
      (make-result sym expr (apply #'append vars)))))
