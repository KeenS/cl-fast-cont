(in-package :cl-user)
(defpackage cl-fast-cont.transform
  (:use :cl))
(in-package :cl-fast-cont.transform)

;;; pass 1
;;; walk down AST and expand macros
;;; and find call/cc(shift), then mark the expr and parents as 'want-cont'


(defstruct (marker (:constructor make-marker (want-cont-p expr)))
  want-cont-p
  expr)

;;; (expr env) -> marker
(defun mark-want-cont (expr env)
  (cond
    ((atom expr) (mark-want-cont-atom expr env))
    (t           (mark-want-cont-cons expr env))))

(defun mark-want-cont-atom (expr env)
  (make-marker nil expr))

(defun mark-want-cont-cons (expr env)
  (let* ((name (car expr)))
    (case name
      ((block)                (mark-want-cont-block expr env))
      ((catch)                (mark-want-cont-catch expr env))
      ((eval-when)            (mark-want-cont-eval-when expr env))
      ((flet)                 (mark-want-cont-flet expr env))
      ((function)             (mark-want-cont-function expr env))
      ((go)                   (mark-want-cont-go expr env))
      ((if)                   (mark-want-cont-if expr env))
      ((labels)               (mark-want-cont-labels expr env))
      ((let)                  (mark-want-cont-let expr env))
      ((let*)                 (mark-want-cont-let* expr env))
      ((load-time-value)      (mark-want-cont-load-time-value expr env))
      ((locally)              (mark-want-cont-locally expr env))
      ((macrolet)             (mark-want-cont-macrolet expr env))
      ((multiple-value-call)  (mark-want-cont-multiple-value-call expr env))
      ((multiple-value-prog1) (mark-want-cont-multiple-value-prog1 expr env))
      ((progn)                (mark-want-cont-progn expr env))
      ((progv)                (mark-want-cont-progv expr env))
      ((quote)                (mark-want-cont-quote expr env))
      ((return-from)          (mark-want-cont-return-from expr env))
      ((setq)                 (mark-want-cont-setq expr env))
      ((symbol-macrolet)      (mark-want-cont-symbol-macrolet expr env))
      ((tagbody)              (mark-want-cont-tagbody expr env))
      ((the)                  (mark-want-cont-the expr env))
      ((throw)                (mark-want-cont-throw expr env))
      ((unwind-protect)       (mark-want-cont-unwind-protect expr env))
      #+openmcl
      ((ccl:compiler-let)     (mark-want-cont-ccl:compiler-let env))
      ;; ((values)               (mark-want-cont-values env))
      ;; ((values-list)          (mark-want-cont-values-list env))
      (t (multiple-value-bind (expantion expanded-p) (macroexpand-1 expr env)
           (if expanded-p
               (mark-want-cont expantion env)
               (mark-want-cont-funcall expantion env)))))))

(defun mark-want-cont-block (expr env)
  (destructuring-bind (name tag &rest body) expr
    (let* ((body (mapcar (lambda (e) (mark-want-cont e env)) body))
           (want-cont-p (some #'marker-want-cont-p body))
           (expr (cons name (cons tag body))))
      (make-marker want-cont-p expr))))

(setf (symbol-function 'mark-want-cont-catch) mark-want-cont-block)

(defun mark-want-cont-eval-when (expr env)
  (destructuring-bind (name situations &rest body) expr
    (let* ((body (mapcar (lambda (e) (mark-want-cont e env)) body))
           (want-cont-p (some #'marker-want-cont-p body))
           (expr (cons name (cons situations body))))
      (make-marker want-cont-p expr))))

(defun mark-want-cont-flet (expr env)
  (destructuring-bind (name bindings body) expr
    (let* ((bindings (error "not implemented"))
           ;; FIXME: handle declarations
           (body (mapcar (lambda (e) (mark-want-cont e env)) body))
           (want-cont-p (or (some #'marker-want-cont-p bindings)
                            (some #'marker-want-cont-p body)))
           (expr (cons name (cons bindings body))))
      (make-marker want-cont-p expr))))

(defun mark-want-cont-function (expr env)
  (error "not implemented"))

(defun mark-want-cont-go (expr env)
  (destructuring-bind (name e) expr
    (let* ((e (mark-want-cont e env))
           (want-cont-p (marker-want-cont-p e))
           (expr (list name e)))
      (make-marker want-cont-p expr))))

(defun mark-want-cont-if (expr env)
  (destructuring-bind (name cond then &optional else) expr
    (let* ((cond (mark-want-cont cond env))
           (then (mark-want-cont then env))
           (else (and else (mark-want-cont else env)))
           (want-cont-p (or
                         (some #'marker-want-cont-p cond)
                         (some #'marker-want-cont-p then)
                         (some #'marker-want-cont-p else)))
           (expr (list name cond then else)))
      (make-marker want-cont-p expr))))


(setf (symbol-function 'mark-want-cont-labels) #'mark-want-cont-flet)

(defun mark-want-cont-let (expr env)
  (destructuring-bind (name bindings &rest body) expr
    (let* ((bindings (error "not implemented"))
           ;; FIXME: handle declarations
           (body (mapcar (lambda (e) (mark-want-cont e env)) body))
           (want-cont-p (or (some #'marker-want-cont-p bindings)
                            (some #'marker-want-cont-p body)))
           (expr (cons name (cons bindings body))))
      (make-marker want-cont-p expr))))

(setf (symbol-function 'mark-want-cont-let*) #'mark-want-cont-let)

(defun mark-want-cont-load-time-value (expr env)
  (destructuring-bind (name form &optional read-only-p) expr
    (let* ((form (mark-want-cont form env))
           (want-cont-p (marker-want-cont-p form))
           (expr (list name form read-only-p)))
      (make-marker want-cont-p expr))))

(defun mark-want-cont-loacally (expr env)
  (error "not implemented"))

(defun mark-want-cont-macrolet (expr env)
  (destructuring-bind (name bindings &rest body) expr
           ;; FIXME: handle declarations
    (let* ((body (mapcar (lambda (e) (mark-want-cont e env)) body))
           (want-cont-p (some #'marker-want-cont-p body))
           (expr (cons name (cons bindings body))))
      (make-marker want-cont-p expr))))

(defun mark-want-cont-multiple-value-call (expr env)
  (destructuring-bind (name fun &rest args) expr
    (let* ((fun (mark-want-cont fun env))
           (args (mapcar (lambda (e) (mark-want-cont e env)) args))
           (want-cont-p (or (marker-want-cont-p fun)
                            (some #'marker-want-cont-p args)))
           (expr (cons name (cons fun args))))
      (make-marker want-cont-p expr))))

(defun mark-want-cont-multiple-value-prog1 (expr env)
  (destructuring-bind (name &rest body) expr
    (let* ((body (mapcar (lambda (e) (mark-want-cont e env)) body))
           (want-cont-p (some #'marker-want-cont-p body))
           (expr (cons name body)))
      (make-marker want-cont-p expr))))

(setf (symbol-function 'mark-want-cont-progn) #'mark-want-cont-multiple-value-prog1)

(defun mark-want-cont-progv (expr env)
  (destructuring-bind (name symbols values &rest body) expr
      (let* ((symbols (mark-want-cont symbols env))
             (values (mark-want-cont values env))
             (body (mapcar (lambda (e) (mark-want-cont e env)) body))
             (want-cont-p (or
                           (marker-want-cont-p symbols)
                           (marker-want-cont-p values)
                           (some #'marker-want-cont-p body)))
             (expr (cons name (cons symbols (cons values body)))))
        (make-marker want-cont-p expr))))


(defun mark-want-cont-quote (expr env)
  (declare (ignore env))
  (make-marker nil expr))

(defun mark-want-cont-return-from (expr env)
  (destructuring-bind (name form) expr
    (let* ((form (mark-want-cont form env))
           (want-cont-p (marker-want-cont-p form))
           (expr (list name form)))
      (make-marker want-cont-p expr))))

(defun mark-want-cont-setq (expr env)
  (destructuring-bind (name &rest asgns) expr
    (let* ((asgns (loop :for (var val . _) :on asgns :by #'cddr
                     :append (list
                              (make-marker nil var)
                              (mark-want-cont val env))))
           (want-cont-p (loop :for val :in (cdr asgns) :by #'cddr
                           :thereis (marker-want-cont-p val)))
           (expr (cons name asgns)))
      (make-marker want-cont-p expr))))

(defun mark-want-cont-symbol-macrolet (expr env)
  (destructuring-bind (name bindings &rest body) expr
    (let ((bindings (error "not implemented"))
          ;; FIXME: handle declarations
          (body (mapcar (lambda (e) (mark-want-cont e env)) body))
          (want-cont-p (some #'marker-want-cont-p body))
          (expr (cons name (cons bindings body))))
      (make-marker want-cont-p body))))


(defun mark-want-cont-tagbody (expr env)
  (error "not implemented"))

(defun mark-want-cont-the (expr env)
  (destructuring-bind (name type form) expr
    (let* ((form (mark-want-cont form env))
           (want-cont-p (marker-want-cont-p form))
           (expr (list name type form)))
      (make-marker want-cont-p expr))))

(defun mark-want-cont-throw (expr env)
  (destructuring-bind (name tag form) expr
    (let* ((tag (mark-want-cont tag env))
           (form (mark-want-cont form env))
           (want-cont-p nil)
           (expr (list name tag form)))
      (make-marker want-cont-p expr))))

(defun mark-want-cont-unwind-protect (expr env)
  (error "not implemented")
  (destructuring-bind (name protected &rest cleanups) expr
    (let* ((protected (mark-want-cont protected env))
           (cleanups (mapcar (lambda (e) (mark-want-cont e env)) cleanups))
           (want-cont-p)))))

;;; pass 2
;;; walk down AST and when find a 'want-cont' marked expr,
;;; get the current continuation and transform the expr to CPS
(defun get-current-continuation (expr &optional cont))

(defun to-cps (expr cont))
