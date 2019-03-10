;; -*- lexical-binding: t -*-

;; to list packages:    "emacs ~/.config/emacs/init.el --no-site-file --batch -l ~/.config/emacs/scripts/use-package-extract.el -f print-packages 2>&1"
;; to install packages: "emacs ~/.config/emacs/init.el --no-site-file --batch -l ~/.config/emacs/scripts/use-package-extract.el -f install-packages 2>&1"

(defun relative-path (path)
  (expand-file-name path (file-name-directory load-file-name)))

(load (relative-path "nix-emacs-with-use-package-pkgs/use-package-name-extract.el") nil t)

(let ((package-settings-path (relative-path "../package-settings.el")))
  (defun install-packages ()
    (defvar user-init-file)
    (defvar custom-file)
    ;; trick package-install into writing into the customization file
    (let ((user-init-file "~/.emacs")
          (custom-file "~/.emacs-custom"))
      (load package-settings-path)
      (package-install 'use-package)    ; we will need use-package, which is not configured using use-package
      (dolist (package-name (upe-walk (read-current-buffer)))
        (package-install package-name nil)))))
