(in-package :cl-user)
(defpackage cl-fast-cont.transform
  (:use :cl))
(in-package :cl-fast-cont.transform)

;;; pass 1
;;; walk down AST and expand macros
;;; and find call/cc(shift), then mark the expr and parents as 'want-cont'


(defstruct (marker (:constructor make-marker% (expr)))
  expr)

(defun make-marker (want-cont-p expr)
  (if want-cont-p
      (make-marker% expr)
      expr))

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
  (destructuring-bind (name bindings &rest body) expr
    (let* ((bindings (loop :for (name bindings &rest body) :in bindings :collect
                          (let* ((body (mapcar (lambda (e) (mark-want-cont e env)) body))
                                 (want-cont-p (some #'marker-want-cont-p body))
                                 (expr (cons name (cons bindings body))))
                            (make-marker want-cont-p expr))))
           ;; FIXME: handle declarations
           (body (mapcar (lambda (e) (mark-want-cont e env)) body))
           (want-cont-p (some #'marker-want-cont-p body))
           (expr (cons name (cons bindings body))))
      (make-marker want-cont-p expr))))

(defun mark-want-cont-function (expr env)
  ;; FIXME: set want-cont attribute to the symbol
  (destructuring-bind (name body) expr
    (if (symbolp body)
        (make-marker nil expr)
        (destructuring-bind (name params body) body
          (let* ((body (mapcar (lambda (e) (mark-want-cont e env)) body))
                 (want-cont-p (some #'marker-want-cont-p body))
                 (expr (cons name (cons params body))))
            (make-marker want-cont-p expr))))))

(defun mark-want-cont-go (expr env)
  (destructuring-bind (name e) expr
    (let* ((e (mark-want-cont e env))
           (want-cont-p (marker-want-cont-p e))
           (expr (list name e)))
      (make-marker want-cont-p expr))))

(defun mark-want-cont-if (expr env)
  (let ((none (gensym "none")))
   (destructuring-bind (name cond then &optional (else none)) expr
     (let* ((cond (mark-want-cont cond env))
            (then (mark-want-cont then env))
            (else (and else (mark-want-cont else env)))
            (want-cont-p (or
                          (some #'marker-want-cont-p cond)
                          (some #'marker-want-cont-p then)
                          (some #'marker-want-cont-p else)))
            (expr (if (eql none else)
                      (list name cond then)
                      (list name cond then else))))
       (make-marker want-cont-p expr)))))


(setf (symbol-function 'mark-want-cont-labels) #'mark-want-cont-flet)

(defun mark-want-cont-let (expr env)
  (destructuring-bind (name bindings &rest body) expr
    (let* ((bindings (loop :for cell :in bindings) :collect
                     (if (consp cell)
                         ;; TODO: is this env correct?
                         (list (car cell) (mark-want-cont (cadr cell) env))
                         var))
           ;; FIXME: handle declarations
           (body (mapcar (lambda (e) (mark-want-cont e env)) body))
           (want-cont-p (or (some #'marker-want-cont-p bindings)
                            (some #'marker-want-cont-p body)))
           (expr (cons name (cons bindings body))))
      (make-marker want-cont-p expr))))

(setf (symbol-function 'mark-want-cont-let*) #'mark-want-cont-let)

(defun mark-want-cont-load-time-value (expr env)
  (let ((none (gensym "none")))
   (destructuring-bind (name form &optional (read-only-p none)) expr
     (let* ((form (mark-want-cont form env))
            (want-cont-p (marker-want-cont-p form))
            (expr (if (eql none read-only-p)
                      (list name form)
                      (list name form read-only-p))))
       (make-marker want-cont-p expr)))))

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
  (destructuring-bind (name protected &rest cleanups) expr
    (let* ((protected (mark-want-cont protected env))
           (cleanups (mapcar (lambda (e) (mark-want-cont e env)) cleanups))
           (want-cont-p (or (marker-want-cont-p protected)
                            (some #'marker-want-cont-p cleanups)))
           (expr (cons name (cons protected cleanups))))
      (make-marker want-cont-p expr))))

(defun mark-want-cont-funcall (expr env)
  (destructuring-bind (f &rest args) expr
    (let* ((f (error "not implemented"))
           (args (mapcar (lambda (e) (mark-want-cont e env)) args))
           (want-cont-p (or (marker-want-cont-p f)
                            (some #'marker-want-cont-p args)))
           (expr (cons f args)))
      (make-marker want-cont-p expr))))

;;; pass 2
;;; walk down AST and when find a 'want-cont' marked expr,
;;; get the current continuation and transform the expr to CPS
(defun get-current-continuation (expr &optional cont)
  (error "not implemented"))

(defun to-cps (marker cont)
  (if (marker-p expr)
      (with-slots (expr) marker
        (cond
          ;; unlikely reachable
          ((atom expr)       (to-cps-atom expr cont))
          (t                 (to-cps-cons expr cont))))
      expr))

(defun to-cps-atom (expr cont)
  (declare (ignore cont))
  expr)

(defun to-cps-cons (expr cont)
  (let* ((name (car expr)))
    (case name
      ((block)                (to-cps-block expr cont))
      ((catch)                (to-cps-catch expr cont))
      ((eval-when)            (to-cps-eval-when expr cont))
      ((flet)                 (to-cps-flet expr cont))
      ((function)             (to-cps-function expr cont))
      ((go)                   (to-cps-go expr cont))
      ((if)                   (to-cps-if expr cont))
      ((labels)               (to-cps-labels expr cont))
      ((let)                  (to-cps-let expr cont))
      ((let*)                 (to-cps-let* expr cont))
      ((load-time-value)      (to-cps-load-time-value expr cont))
      ((locally)              (to-cps-locally expr cont))
      ((macrolet)             (to-cps-macrolet expr cont))
      ((multiple-value-call)  (to-cps-multiple-value-call expr cont))
      ((multiple-value-prog1) (to-cps-multiple-value-prog1 expr cont))
      ((progn)                (to-cps-progn expr cont))
      ((progv)                (to-cps-progv expr cont))
      ((quote)                (to-cps-quote expr cont))
      ((return-from)          (to-cps-return-from expr cont))
      ((setq)                 (to-cps-setq expr cont))
      ((symbol-macrolet)      (to-cps-symbol-macrolet expr cont))
      ((tagbody)              (to-cps-tagbody expr cont))
      ((the)                  (to-cps-the expr cont))
      ((throw)                (to-cps-throw expr cont))
      ((unwind-protect)       (to-cps-unwind-protect expr cont))
      #+openmcl
      ((ccl:compiler-let)     (to-cps-ccl:compiler-let cont))
      ;; ((values)               (to-cps-values cont))
      ;; ((values-list)          (to-cps-values-list cont))

      ;; FIXME
      (t (multiple-value-bind (expantion expanded-p) (macroexpand-1 expr cont)
           (if expanded-p
               (to-cps expantion cont)
               (to-cps-funcall expantion cont)))))))

(defun to-cps-markers (markers cont)
  (if (null markers)
      ()
      (let ((marker (car markers))
            (marker (cdr markers))
            (v (gensym "v")))
        (if (want-cont-p marker)
            (list (to-cps marker
                          `(lambda (,v)
                             (declare (ignore ,v))
                             ,@(to-cps-markers markers cont))))
            (cons marker (to-cps-markers markers cont))))))

(defun to-cps-block (expr cont)
  (destructuring-bind (name tag &rest body) expr
    `(,name ,tag ,@(to-cps-markers body cont))))

(setf (symbol-function 'to-cps-catch) to-cps-block)

(defun to-cps-eval-when (expr cont)
  (destructuring-bind (name situations &rest body) marker
    `(,name ,situations ,@(to-cps-markers body cont))))


(defun to-cps-lambda-like (expr)
  (destructuring-bind (name params &rest body) expr
    (let ((c (gensym "cont")))
      `(,name (,cons c params) ,@(to-cps-markers body c)))))

(defun to-cps-flet (expr cont)
  (destructuring-bind (name bindings &rest body) expr
    (let* ((bindings (loop :for binding :in bindings :collect
                          (if (marker-p binding)
                              (to-cps-lambda-like (expr-of binding))
                              binding)))
           (body (to-cps-markes body cont)))
      `(,name ,bindings ,@body))))

(defun to-cps-function (expr cont)
  (destructuring-bind (name body) expr
    (with-slots (want-cont-p expr) body
     (if (symbolp expr)
         ;; when its a symbol, it shouldn't want cont
         `(name ,expr)
         (to-cps-lambda-like body)))))

(defun to-cps-go (expr env)
  (error "not implemented"))

(defun to-cps-if (expr cont)
  (let ((none (gensym "none")))
   (destructuring-bind (name cond then &optional (else none)) expr
     (let ((then (to-cps then cont))
           (else (and (not (eql none else)) (to-cps else cont)))
           (make-if (lambda (v) (if (eql none else)
                                (list name v then)
                                (list name v then else))))
           (v (gensym "v")))
       (with-slots (want-cont-p expr) cond
         (if want-cont-p
             (to-cps cond `(lambda (,v) ,(funcall #'make-if v)))
             (funcall #'make-if expr)))))))


(setf (symbol-function 'to-cps-labels) #'to-cps-flet)

(defun to-cps-let (expr env)
  (error "not implemented"))

(defun to-cps-let* (expr cont)
  (labels ((to-cps-bare-let* (name bindings parsed body)
             (if (null bindings)
                 `(,name ,(nreverse parsed) ,@(to-cps-markers body cont))
                 (let ((binding (car bindings))
                       (bindings (cdr bindings)))
                   (if (and (consp bindig) (marker-p (cdr binding)))
                       (let ((sym (car binding))
                             (expr (expr-of(cdr binding))))
                         `(name ,(nreverse parsed)
                               ,(to-cps expr `(lambda (,sym)
                                                ,@(to-cps-bare-let* name bindings () body)))))
                       (to-cps-bare-let* name bindings (cons binding parsed) body))))))
    (if (marker-p marker)
        (destructuring-bind (name bindings &rest body) (expr-of marker)
          (to-cps-bare-let* name bindings () body))
        marker)))

(defun to-cps-load-time-value (expr cont)
  (let ((none (gensym "none")))
   (destructuring-bind (name form &optional (read-only-p none)) expr
     (let* ((form (to-cps form cont)))
       (if (eql none read-only-p)
           (list name form)
           (list name form read-only-p))))))

;; FIXME: all of below are not implemented

(defun to-cps-loacally (expr env)
  (error "not implemented"))

(defun to-cps-macrolet (expr env)
  (destructuring-bind (name bindings &rest body) expr
           ;; FIXME: handle declarations
    (let* ((body (mapcar (lambda (e) (to-cps e env)) body))
           (want-cont-p (some #'marker-want-cont-p body))
           (expr (cons name (cons bindings body))))
      (make-marker want-cont-p expr))))

(defun to-cps-multiple-value-call (expr env)
  (destructuring-bind (name fun &rest args) expr
    (let* ((fun (to-cps fun env))
           (args (mapcar (lambda (e) (to-cps e env)) args))
           (want-cont-p (or (marker-want-cont-p fun)
                            (some #'marker-want-cont-p args)))
           (expr (cons name (cons fun args))))
      (make-marker want-cont-p expr))))

(defun to-cps-multiple-value-prog1 (expr env)
  (destructuring-bind (name &rest body) expr
    (let* ((body (mapcar (lambda (e) (to-cps e env)) body))
           (want-cont-p (some #'marker-want-cont-p body))
           (expr (cons name body)))
      (make-marker want-cont-p expr))))

(setf (symbol-function 'to-cps-progn) #'to-cps-multiple-value-prog1)

(defun to-cps-progv (expr env)
  (destructuring-bind (name symbols values &rest body) expr
      (let* ((symbols (to-cps symbols env))
             (values (to-cps values env))
             (body (mapcar (lambda (e) (to-cps e env)) body))
             (want-cont-p (or
                           (marker-want-cont-p symbols)
                           (marker-want-cont-p values)
                           (some #'marker-want-cont-p body)))
             (expr (cons name (cons symbols (cons values body)))))
        (make-marker want-cont-p expr))))


(defun to-cps-quote (expr env)
  (declare (ignore env))
  (make-marker nil expr))

(defun to-cps-return-from (expr env)
  (destructuring-bind (name form) expr
    (let* ((form (to-cps form env))
           (want-cont-p (marker-want-cont-p form))
           (expr (list name form)))
      (make-marker want-cont-p expr))))

(defun to-cps-setq (expr env)
  (destructuring-bind (name &rest asgns) expr
    (let* ((asgns (loop :for (var val . _) :on asgns :by #'cddr
                     :append (list
                              (make-marker nil var)
                              (to-cps val env))))
           (want-cont-p (loop :for val :in (cdr asgns) :by #'cddr
                           :thereis (marker-want-cont-p val)))
           (expr (cons name asgns)))
      (make-marker want-cont-p expr))))

(defun to-cps-symbol-macrolet (expr env)
  (destructuring-bind (name bindings &rest body) expr
    (let ((bindings (error "not implemented"))
          ;; FIXME: handle declarations
          (body (mapcar (lambda (e) (to-cps e env)) body))
          (want-cont-p (some #'marker-want-cont-p body))
          (expr (cons name (cons bindings body))))
      (make-marker want-cont-p body))))


(defun to-cps-tagbody (expr env)
  (error "not implemented"))

(defun to-cps-the (expr env)
  (destructuring-bind (name type form) expr
    (let* ((form (to-cps form env))
           (want-cont-p (marker-want-cont-p form))
           (expr (list name type form)))
      (make-marker want-cont-p expr))))

(defun to-cps-throw (expr env)
  (destructuring-bind (name tag form) expr
    (let* ((tag (to-cps tag env))
           (form (to-cps form env))
           (want-cont-p nil)
           (expr (list name tag form)))
      (make-marker want-cont-p expr))))

(defun to-cps-unwind-protect (expr env)
  (destructuring-bind (name protected &rest cleanups) expr
    (let* ((protected (to-cps protected env))
           (cleanups (mapcar (lambda (e) (to-cps e env)) cleanups))
           (want-cont-p (or (marker-want-cont-p protected)
                            (some #'marker-want-cont-p cleanups)))
           (expr (cons name (cons protected cleanups))))
      (make-marker want-cont-p expr))))

(defun to-cps-funcall (expr env)
  (destructuring-bind (f &rest args) expr
    (let* ((f (error "not implemented"))
           (args (mapcar (lambda (e) (to-cps e env)) args))
           (want-cont-p (or (marker-want-cont-p f)
                            (some #'marker-want-cont-p args)))
           (expr (cons f args)))
      (make-marker want-cont-p expr))))
