;; run like: "emacs ~/.config/emacs/init.el --no-site-file --batch -l ~/.config/emacs/scripts/use-package-extract.el -f print-packages 2>&1"

(defun upe-list-until (predicate list)
  (cond ((null list) nil)
        ((funcall predicate (car list)) nil)
        (t (cons (car list) (upe-list-until predicate (cdr list))))))

(defun upe-findcdr (keyword list)
  (cond ((null list) nil)
        ((eq (car list) keyword) list)
        (t (upe-findcdr keyword (cdr list)))))

(defun upe-get-use-package-progn (body keyword)
  (upe-list-until #'keywordp (cdr (upe-findcdr keyword body))))

(defun upe-handle-use-package (use-package-expression)
  (let* ((name (cadr use-package-expression))
         (body (cddr use-package-expression))
         (ensure (car (upe-get-use-package-progn body :ensure)))
         (install-package (cond ((keywordp ensure) name)
                                ((eq t ensure)     name)
                                (t                 ensure)))
         (init-progn (upe-get-use-package-progn body :init))
         (config-progn (upe-get-use-package-progn body :config))
         (preface-progn (upe-get-use-package-progn body :preface)))
    (append (if install-package (list install-package) '())
            (upe-walk init-progn)
            (upe-walk config-progn)
            (upe-walk preface-progn))))

(defun upe-walk (tree)
  (cond ((atom tree)                  nil)
        ((and (eq (car tree) 'when)
              (eq (cadr tree) nil))   nil) ; ignore parts commented out with (when nil ...)
        ((eq (car tree) 'use-package) (upe-handle-use-package tree))
        (t                            (append (upe-walk (car tree))
                                              (upe-walk (cdr tree))))))

(defun read-current-buffer ()
  "Read the current buffer and return its contents as a list of Lisp objects."
  (let (result)
    (while (< (point) (point-max))
      (add-to-list 'result (ignore-errors (read (current-buffer)))))
    result))

(defun print-packages ()
  (dolist (element (upe-walk (read-current-buffer)))
    (message (symbol-name element))))
