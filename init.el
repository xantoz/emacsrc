(require 'cl)                           ; I like extra bloat!
;; only really works during load-time
(defun relative-path (path)
  (expand-file-name path (file-name-directory load-file-name)))

(when (>= emacs-major-version 24)
  (load (relative-path "package-settings.el")))

(unless (package-installed-p 'use-package)
  (progn
    (package-refresh-contents)
    (package-install 'use-package)))
(require 'use-package)
(setq use-package-verbose t)

;; wrap use-package so that it ignores :ensure
(defun up-parameter-skip-to-keyword (list)
  (cond ((endp list) nil)
        ((keywordp (car list)) list)
        (t (up-parameter-skip-to-keyword (cdr list)))))

(defun up-parameter-remove (list key)
  (cond ((endp list) nil)
        ((eq (car list) key) (up-parameter-skip-to-keyword (cdr list)))
        (t (cons (car list) (up-parameter-remove (cdr list) key)))))

(setf (symbol-function '%old-use-package) (symbol-function 'use-package))
(defmacro use-package (&rest args)
  (let ((no-ensure (up-parameter-remove args :ensure)))
    `(%old-use-package ,@no-ensure)))

;; (autoload 'ghc-init "ghc" nil t)
;; (autoload 'ghc-debug "ghc" nil t)
;; (add-hook 'haskell-mode-hook (lambda () (ghc-init)))

;(setq max-specpdl-size 13000)           ; 10 times larger than default (1300)
(setq max-specpdl-size 30000)
;(setq max-lisp-eval-depth 10000)        ; 16.7 times larger than default (600)
(setq max-lisp-eval-depth 20000)

(unless (fboundp 'system-name)
  (defun system-name ()
    system-name))

;; Constants to tell which machine we are running on so we can conditionally include code and stuffs
(defconst i-am-csc-ubuntu (and (eq system-type 'gnu/linux) (string= "csc" (second (split-string (system-name) "\\.")))))
(defconst i-am-monad (string-prefix-p "monad" (system-name)))
(defconst i-am-lain (string-prefix-p "lain" (system-name)))
(defconst i-am-sakuya (string-prefix-p "sakuya" (system-name)))
(defconst i-am-colgate (string-prefix-p "colgate" (system-name)))
(defconst i-am-udongein (string-prefix-p "udongein" (system-name)))
(defconst i-am-suiseiseki (string-prefix-p "suiseiseki.nu" (system-name)))
(defconst i-am-souseiseki (string-prefix-p "Souseiseki" (system-name)))
(defconst i-am-patchouli  (string-prefix-p "patchouli" (system-name)))
(defconst i-am-kombu      (string-prefix-p "kombu" (system-name)))
(defconst i-am-cirno      (string-prefix-p "cirno" (system-name)))
(defconst i-am-nazrin     (string-prefix-p "nazrin" (system-name)))
(defconst i-am-usbee      (string-prefix-p "usbee" (system-name)))
(defconst i-am-nanopi-alpine (string-prefix-p "nanopi-alpine" (system-name)))

(defconst i-am-headless-server (or i-am-suiseiseki i-am-souseiseki i-am-sakuya i-am-patchouli i-am-kombu i-am-nanopi-alpine))
(defconst i-have-battery (or i-am-colgate i-am-nazrin i-am-cirno))

;; (setq load-path (remove-if (lambda (x) (string-match "auctex" x)) load-path))

(if (featurep 'tex-site) (unload-feature 'tex-site t))

;;;; DIRED
;; Have some nice extra dired features like dired-do-find-marked-files (on F)
;; also keybinds
(defun my-dired-copy-filename-as-kill (&optional arg)
  (interactive "P")
  (if arg
      (if (zerop (prefix-numeric-value arg))
          (dired-copy-filename-as-kill)
          (dired-copy-filename-as-kill arg))
      (dired-copy-filename-as-kill 0)))

(add-hook 'dired-load-hook
          #'(lambda ()
              (load "dired-x")
              (setq dired-guess-shell-alist-user '(("\\.eps\\'" "atril &") ("\\.pdf\\'" "atril &") ("\\.csv\\'" "csv-pretty") ("\\.csv.gz\\'" "zcat * | csv-pretty")))
              (setq dired-dwim-target t)))
(add-hook 'dired-mode-hook
          #'(lambda ()
              (local-set-key "w" 'my-dired-copy-filename-as-kill)))
;;;; END DIRED

;; Add extra things to load path if they exist (mostly obsolete now
;; with packages, but I still have some modules installed this way on
;; some machines)
(defun maybe-add-to-load-path (&rest paths)
  (dolist (ele paths)
    (when (file-directory-p ele)
      (add-to-list 'load-path (directory-file-name ele)))))

(maybe-add-to-load-path
 (relative-path "elisp/")
 (relative-path "vendor-elisp/")
 (relative-path "submodule-elisp/nix-update-el/")
 (relative-path "submodule-elisp/bitbake-el/")
 "~/.elisp/")

(use-package json :ensure t :defer t) ; seems to be used by nix-mode in part, but it's not properly pulled in...
(use-package nix-mode
  :ensure t
  :defer t
  :config (setq nix-indent-function #'smie-indent-line))

(use-package ratpoison
  :defer t
  :commands ratpoisonrc-mode
  :mode (".ratpoisonrc" . ratpoisonrc-mode))

(use-package etags-table
  :defer 1
  :config (setq etags-table-search-up-depth 99))

(use-package mmm-mode :ensure t :defer t) ; needed by bitbake-el
(use-package bitbake
  :defer 1
  :init (progn
          (add-hook 'mmm-mode-hook (lambda () (whitespace-mode 0)))
          (add-hook 'mmm-bitbake-mode-hook (lambda () (whitespace-mode 0)))
          (add-hook 'mmm-python-mode-submode-hook (lambda () (whitespace-mode 0)))
          (add-hook 'mmm-shell-script-mode-submode-hook (lambda () (whitespace-mode 0)))
          (add-hook 'mmm-sh-mode-submode-hook (lambda () (whitespace-mode 0)))))

(when (or i-am-colgate i-am-usbee i-am-nazrin)
  (maybe-add-to-load-path (relative-path "emacs-libvterm/"))
  (use-package vterm
    :commands vterm vterm-other-window
    :config (progn
              (set-face-foreground 'vterm-color-default "white")
              (set-face-background 'vterm-color-default "black")
              (set-face-foreground 'term-color-blue "#5c5cff")
              ;; seems like cua-mode is interfering with vterms remaping of yank and yank-pop
              (define-key vterm-mode-map (kbd "C-y") #'vterm-yank)
              (define-key vterm-mode-map (kbd "M-y") #'vterm-yank-pop))))

;; ;; load jflex-mode now (it becomes happier that way) if it is in load path
;; (require 'jflex-mode nil t)

;; ;; Pulsing on goto-line among others
;; (when (require 'pulse nil t)
;;   (pulse-toggle-integration-advice 1))


(setq inhibit-startup-screen t)

(when (featurep 'x)
  (tool-bar-mode -1)
  ;; (scroll-bar-mode -1)
  )

(use-package ag
  :defer t
  :ensure t
  :config (use-package wgrep-ag :ensure t)
  :init
  (defun ag-case (&rest args)
    "ag, but with case sensitivity"
    (interactive (advice-eval-interactive-spec
                  (cadr (interactive-form #'ag))))
    (let ((ag-arguments (cons "--case-sensitive" (remove "--smart-case" ag-arguments))))
      (apply #'ag args))))

;; Mute these
(dolist (i '(XF86AudioMute
             XF86MonBrightnessDown
             XF86MonBrightnessUp
             XF86AudioRaiseVolume
             XF86AudioLowerVolume
             XF86WebCam
             XF86Eject))
  (global-set-key (make-vector 1 i) 'ignore))

(use-package crosshairs
  :defer t
  :bind (("<f9>" . crosshairs-flash)
         ("C-<f9>" . crosshairs-mode)))

;; C-x C-t clashes badly with ratpoison defaults. C-x t seems to be free, so use that instead
(global-set-key (kbd "C-x t") 'transpose-lines)

;; find-file-in-project
;; TODO: bindings for maybe:
;;         find-directory-in-project
;;         find-file-in-current-directory
;;         Some of the diff-related commands?
;;      learn to use:
;;         ffip-create-project-file
(use-package find-file-in-project
  :ensure t
  :defer t
  :bind ("C-x C-M-f" . find-file-in-project))

;; pop mark
(global-set-key (kbd "C-,") (lambda () (interactive) (set-mark-command "")))

(when (require 'helm nil t)
  (global-set-key (kbd "C-?") 'helm-semantic-or-imenu))

(when (require 'imenu-anywhere nil t)
  (cond ((or (featurep 'helm) (require 'helm nil t))
         (global-set-key (kbd "C-.") 'helm-imenu-anywhere)
         (global-set-key (kbd "C-M-.") 'imenu-anywhere))
        ((or (featurep 'ivy) (require 'ivy nil t))
         (global-set-key (kbd "C-.") 'ivy-imenu-anywhere)
         (global-set-key (kbd "C-M-.") 'imenu-anywhere))
        (t (global-set-key (kbd "C-.") 'imenu-anywhere))))

(defun fix-python-imenu ()
  "Hack to manually set imenu-create-index-function to python-imenu-create-index"
  (interactive)
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (eq major-mode 'python-mode)
        (set (make-local-variable 'imenu-create-index-function) 'python-imenu-create-index)))))

(add-hook 'python-mode-hook
          (lambda()
            (setq imenu-create-index-function 'python-imenu-create-index)))

;; Enable CUA selection mode without the C-z/C-x/C-c/C-v bindings.
;; Use case: rectangular regions using C-RET
(when (> emacs-major-version 21)
  (cua-selection-mode t))

(setq display-buffer-reuse-frames t)
(when nil
  (setq display-buffer-alist
        '((".*"
           (display-buffer-reuse-window)
           (reusable-frames . visible)))))

;; Improve behavior of some C-x 5 ... commands so they do not open a new frame when there is one already
(setq display-buffer--other-frame-action
      '((display-buffer-reuse-window
         display-buffer-use-some-frame
         display-buffer-pop-up-frame)
        (reusable-frames . visible)
        ;; (frame-predicate . (lambda (frame) (eq t (frame-visible-p frame))))
        (inhibit-same-window . t)))

;; ediff setup
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(setq ediff-split-window-function (if (> (frame-width) 150)
                                      'split-window-horizontally
                                    'split-window-vertically))

;; Have to fix xref--pop-to-location behavior for frames
(use-package xref
  :defer t
  :config
  (defun xref--pop-to-location (item &optional action)
    "Go to the location of ITEM and display the buffer.
ACTION controls how the buffer is displayed:
  nil      -- switch-to-buffer
  `window' -- pop-to-buffer (other window)
  `frame'  -- pop-to-buffer (other frame)
If SELECT is non-nil, select the target window."
    (let* ((marker (save-excursion
                     (xref-location-marker (xref-item-location item))))
           (buf (marker-buffer marker)))
      (cl-ecase action
        ((nil)  (switch-to-buffer buf))
        (window (pop-to-buffer buf t))
        (frame  (pop-to-buffer buf display-buffer--other-frame-action))
        ;; (frame  (switch-to-buffer-other-frame buf))
        )
      (xref--goto-char marker))
    (let ((xref--current-item item))
      (run-hooks 'xref-after-jump-hook))))

(setq require-final-newline t)		;; will make the last line end in a carriage return
(fset 'yes-or-no-p 'y-or-n-p)		;; will allow you to type just "y" instead of "yes" when you exit.
(setq next-line-add-newlines nil)	;; will disallow creation of new lines when you press the "arrow-down key" at end of the buffer.

(setq display-time-day-and-date t) (display-time)  ;; will make the display of date and time persistent.
(require 'paren) (show-paren-mode t)               ;; will highlight matching parentheses next to cursor.
(setq-default indent-tabs-mode nil)                ;; will introduce spaces instead of tabs by default.

(transient-mark-mode t)              ;; will highlight region between point and mark.
(setq query-replace-highlight t)     ;; will highlight during query.
(setq search-highlight t)            ;; highlight incremental search
(setq default-major-mode 'text-mode) ;; will make text-mode default.

; Moving cursor down at bottom scrolls only a single line, not half page
(setq scroll-step 1)
(setq scroll-conservatively 5)
(global-set-key [delete] 'delete-char)

(setq mouse-drag-copy-region nil)  ; stops selection with a mouse being immediately injected to the kill ring
;(setq x-select-enable-primary nil)  ; stops killing/yanking interacting with primary X11 selection
(setq x-select-enable-primary t)  ; make killing/yanking interact with primary X11 selection
(setq x-select-enable-clipboard t)  ; makes killing/yanking interact with clipboard X11 selection   ;; these will probably be already set to these values, leave them that way if so!

; (setf interprogram-cut-function 'x-select-text)
; (setf interprogram-paste-function 'x-cut-buffer-or-selection-value)

; this doesn't always quite work right at time of writing, but when it does, it makes
; "highlight/middlebutton" style (X11 primary selection based) copy-paste work as expected
; if you're used to other modern apps (that is to say, the mere act of highlighting doesn't
; overwrite the clipboard or alter the kill ring, but you can paste in merely highlighted
; text with the mouse if you want to)
(setq select-active-regions t) ;  active region sets primary X11 selection
(global-set-key [mouse-2] 'mouse-yank-primary)  ; make mouse middle-click only paste from primary X11 selection, not clipboard and kill ring.

;; with this, doing an M-y will also affect the X11 clipboard, making emacs act as a sort of clipboard history, at
;; least of text you've pasted into it in the first place.
; (setq yank-pop-change-selection t)  ; makes rotating the kill ring change the X11 clipboard.

;;;; C-mode
(setq kill-whole-line t)     ;; will make "Ctrl-k" kills an entire line if the cursor is at the beginning of line -- very useful.
(setq c-hungry-delete-key t) ;; will delete "hungrily" in C mode! Use it to see what it does -- very useful.
(setq c-auto-newline 1)      ;; will let emacs put in a "carriage-return" for you automatically after left curly braces, right curly braces, and semi-colons in "C mode"

(add-hook 'c-mode-common-hook
          #'(lambda ()
              (turn-on-auto-fill)
              (setq fill-column 80)
              (setq comment-column 60)
              ;; (modify-syntax-entry ?_ "w")       ; now '_' is not considered a word-delimiter
              (if (or (> emacs-major-version 23) (and (= emacs-major-version 23) (>= emacs-minor-version 2))) ; For studlyCaps, use (c-subword-mode 1) in your mode setup: makes some movement and text commands recognize case-change as a word boundary
                  (subword-mode 1)
                  (c-subword-mode 1))
              ;; (c-set-style "ellemtel")           ; set indentation style
              (c-set-style "linux")           ; set indentation style
              (c-set-offset 'inextern-lang 0) ; don't indent inside extern "C" blocks
              (setq c-doc-comment-style '(gtkdoc javadoc autodoc))
              (setq c-basic-offset 4)
              (local-set-key (kbd "C-c C-h") 'c-toggle-hungry-state)
              (local-set-key [(control tab)]     ; move to next tempo mark
                             'tempo-forward-mark)
              (linum-mode)))

(add-hook 'python-mode-hook
          #'(lambda ()
              (linum-mode)))

; Make Emacs use "newline-and-indent" when you hit the Enter key so
; that you don't need to keep using TAB to align yourself when coding.
(global-set-key "\C-m" 'newline-and-indent)

(use-package xcscope
  :defer t
  :ensure t
  :init
  (add-hook 'c-mode-hook (function cscope-minor-mode))
  (add-hook 'c++-mode-hook (function cscope-minor-mode))
  (add-hook 'dired-mode-hook (function cscope-minor-mode))
  (add-hook 'python-mode-hook (function cscope-minor-mode))
  (add-hook 'web-mode-hook (function cscope-minor-mode)))

;; give helm-cscope-mode some bindings so it's actually useful
(when (require 'helm-cscope nil t)
  (progn
    (let ((map helm-cscope-mode-map))
      (define-key map "\C-css" 'helm-cscope-find-this-symbol)
      (define-key map "\C-csd" 'helm-cscope-find-global-definition)
      (define-key map "\C-csg" 'helm-cscope-find-global-definition)
      ;(define-key map "\C-csG" 'cscope-find-global-definition-no-prompting)
      (define-key map "\C-cs=" 'helm-cscope-find-assignments-to-this-symbol)
      (define-key map "\C-csc" 'helm-cscope-find-calling-this-funtcion) ;lol, misspelled in the lib
      (define-key map "\C-csC" 'helm-cscope-find-called-function)
      (define-key map "\C-cst" 'helm-cscope-find-this-text-string)
      (define-key map "\C-cse" 'helm-cscope-find-egrep-pattern)
      (define-key map "\C-csf" 'helm-cscope-find-this-symbol)
      (define-key map "\C-csi" 'helm-cscope-find-files-including-file)
      ;; --- (The '---' indicates that this line corresponds to a menu separator.)
      ;; (define-key map "\C-csb" 'cscope-display-buffer)
      ;; (define-key map "\C-csB" 'cscope-display-buffer-toggle)
      (define-key map "\C-csn" 'cscope-history-forward-line-current-result)
      (define-key map "\C-csN" 'cscope-history-forward-file-current-result)
      (define-key map "\C-csp" 'cscope-history-backward-line-current-result)
      (define-key map "\C-csP" 'cscope-history-backward-file-current-result)
      (define-key map "\C-csu" 'cscope-pop-mark)
      ;; ---
      (define-key map "\C-csa" 'cscope-set-initial-directory)
      (define-key map "\C-csA" 'cscope-unset-initial-directory)
      ;; ---
      (define-key map "\C-csL" 'cscope-create-list-of-files-to-index)
      (define-key map "\C-csI" 'cscope-index-files)
      (define-key map "\C-csE" 'cscope-edit-list-of-files-to-index)
      (define-key map "\C-csW" 'cscope-tell-user-about-directory)
      (define-key map "\C-csS" 'cscope-tell-user-about-directory)
      (define-key map "\C-csT" 'cscope-tell-user-about-directory)
      (define-key map "\C-csD" 'cscope-dired-directory))))

;; fix silly emacs defaults
(setq sentence-end-double-space nil)

;;;; shell-mode Improvements
(add-hook 'comint-output-filter-functions 'comint-watch-for-password-prompt)  ;; will disalllow passwords to be shown in clear text (this is useful, for example, if you use the shell and then, login/telnet/ftp/scp etc. to other machines).

;;;; Handle backups and autosaves stuff sanelier
(setq backup-by-copying t               ; don't clobber symlinks
      backup-directory-alist '(("." . "~/.emacs.d/backups/")) ; don't litter my fs tree
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)       ; use versioned backups
(setq tramp-backup-directory-alist backup-directory-alist)
;; (setq auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-saves/" t)))

;; auto-saves goes in same directory for extra ugliness (esp. since
;; backups don't know). But this hinders tramp from being stupid
;; wrt. /su::/somesupersecretfile and I want my .emacs to be sanely
;; usable on multi-user systems on where I'm the admin.
(setq auto-save-file-name-transforms nil)

;;;; w3m settings
(eval-after-load 'w3m
  '(progn
     (if (featurep 'x)
         (setq w3m-default-display-inline-images t
               w3m-use-favicon                   t)
       (setq w3m-default-display-inline-images     nil
             w3m-show-graphic-icons-in-header-line nil
             w3m-show-graphic-icons-in-mode-line   nil
             w3m-use-favicon                       nil))
     (setq w3m-default-save-directory "~/Downloads"
           w3m-use-cookies            t)))

;;;; default browser
(defun find-browser ()
  "Finds browser to use, from an internal list. Earlier takes precedence.
Graphical browsers only."
  (let ((browsers (list "webmacs" "firefox" "conkeror" "chromium" "midori" "surf")))
    (reduce (lambda (a b) (or a b))     ;or ain't a function, can't use it without thunk
            (mapcar #'executable-find browsers))))

(cond ((and (featurep 'x)
            (find-browser))
       (setq browse-url-browser-function 'browse-url-generic
             browse-url-generic-program (find-browser)))
      ((require 'w3m nil t)
       (setq browse-url-browser-function 'w3m-browse-url)
       (autoload 'w3m-browse-url "w3m" "Ask a WWW browser to show a URL." t)))

;;;; etags customization
;; swap these two buttons, I want this to behave like SLIME does
(define-key esc-map "*" 'tags-loop-continue)
(define-key esc-map "," 'pop-tag-mark)

;;;; Making C-x k end an emacsclient session
(when (>= emacs-major-version 23)
  (add-hook 'server-switch-hook
            (lambda ()
              (when (current-local-map)
                (use-local-map (copy-keymap (current-local-map))))
              (local-set-key (kbd "C-x k") 'server-edit))))

;; lazily load ~/quicklisp/slime-helper.el, if it exists
;; cannot use use-package, because slime-helper.el is not a proper require:eable package
(let ((quicklisp-slime-helper-path (expand-file-name "~/quicklisp/slime-helper.el")))
  (when (file-regular-p quicklisp-slime-helper-path)
    (autoload 'slime                quicklisp-slime-helper-path "" t   nil)
    (autoload 'slime-mode           quicklisp-slime-helper-path "" t   nil)
    (autoload 'slime-lisp-mode-hook quicklisp-slime-helper-path "" nil nil)
    (add-hook 'lisp-mode-hook 'slime-lisp-mode-hook)
    (eval-after-load quicklisp-slime-helper-path
      (lambda () (setq inferior-lisp-program "sbcl")))))

;; (defun slem ()
;;   (interactive)
;;   ;; (fmakunbound 'slime)
;;   (load (expand-file-name "~/quicklisp/slime-helper.el"))
;;   (set-language-environment "UTF-8")
;;   (setq slime-net-coding-system 'utf-8-unix)
;;   (setq inferior-lisp-program (or (executable-find "sbcl")
;;                                   (executable-find "clisp"))) ; clisp -K full ?
;;   (slime-setup)
;;   (slime))


;; Mouse Drag
;; (global-set-key [down-mouse-2] 'mouse-drag-throw)
(global-set-key [down-mouse-2] 'mouse-drag-drag)
(setq mouse-throw-with-scroll-bar t)

(when (require 'fill-column-indicator nil t)
  ;; (setq fci-rule-character 9553)        ;double bar
  (setq fci-rule-character 9474)        ;single bar
  )

;;EMMS setup
(when (require 'emms nil t)
  (require 'emms-setup)
  (emms-standard)
  (emms-default-players)

  ;; (require 'emms-player-simple)
  ;; (define-emms-simple-player apeplayer '(file)
  ;;   (regexp-opt '( ".ape" ".tta" ))
  ;;   "mplayer" "-slave" "-quiet" "-really-quiet" )
  ;; (add-to-list 'emms-player-list 'emms-player-apeplayer)

  (require 'emms-info-libtag)
  (setq emms-info-functions '(emms-info-libtag))

  ;;Play a whole directory tree
  (global-set-key (kbd "C-c C-l") 'emms-play-directory-tree)
  ;;Just play a single file
  (global-set-key (kbd "C-c l") 'emms-play-file)

  ;;Set a pause key
  (global-set-key (kbd "C-c p") 'emms-pause)

  ;;Set a key for previous track
  (global-set-key (kbd "C-c C-p") 'emms-previous)

  ;;Set a key to browse the music
  (global-set-key (kbd "C-c b") 'open-music-dir-in-dired)

  ;;And a key to add the music while browsing
  (global-set-key (kbd "C-c q") 'emms-add-dired)

  ;;Set a next key
  (global-set-key (kbd "C-c n") 'emms-next)

  ;;Add a whole tree to the playlist
  (global-set-key (kbd "C-c C-a") 'emms-add-directory-tree)
  ;;Add a single file
  (global-set-key (kbd "C-c a") 'emms-add-file)

  ;;Show the playlist in the current frame
  (global-set-key (kbd "C-c C-s") 'emms-playlist-mode-go)

  ;; lite mer regexp at lilla mplayer
  (emms-player-set emms-player-mplayer 'regex "\\`\\(http\\|mms\\)://\\|\\.\\([Oo][Gg][Gg]\\|[Mm][Pp]3\\|[Ww][Aa][Vv]\\|[Mm][Pp][Gg]\\|[Mm][Pp][Ee][Gg]\\|[Ww][Mm][Vv]\\|[Ww][Mm][Aa]\\|[Mm][Oo][Vv]\\|[Aa][Vv][Ii]\\|[Dd][Ii][Vv][Xx]\\|[Oo][Gg][Mm]\\|[Oo][Gg][Vv]\\|[Aa][Ss][Ff]\\|[Mm][Kk][Vv]\\|[Rr][Mm]\\|[Rr][Mm][Vv][Bb]\\|[Mm][Pp]4\\|[Ff][Ll][Aa][Cc]\\|[Vv][Oo][Bb]\\|[Mm]4[Aa]\\|[Aa][Pp][Ee]\\|[Tt][Tt][Aa]\\|[Aa][Aa][Cc]\\)\\'"))

;;;; Find function
(find-function-setup-keys)

;;;; iswitchb-mode  (I don't care that it is deprecated. I prefer this over ido-mode!)
(iswitchb-mode 1)
(setq iswitchb-default-method
      (if i-am-headless-server
          'always-frame
          'samewindow))

(global-set-key (kbd "C-x M-b") #'ivy-switch-buffer)
(global-set-key (kbd "C-x C-M-b") #'ivy-switch-buffer)

;;;; image-dired
(when (featurep 'x)
  (eval-after-load 'image-dired
    '(let ((use-/dev/shm/-? (or i-am-udongein i-am-lain i-am-monad)))
       (setq image-dired-cmd-pngcrush-program         (executable-find "pngcrush")
             image-dired-cmd-pngnq-program            (executable-find "pngnq")
             image-dired-gallery-image-root-url       "Pictures"
             image-dired-gallery-thumb-image-root-url "thumbnails"
             image-dired-external-viewer              (cond ((executable-find "feh-bg")
                                                             (concat (executable-find "feh-bg") " --scale-down"))
                                                            ((executable-find "feh"))
                                                            ((executable-find "display")))
             image-dired-main-image-directory         "~/Pictures/"
             image-dired-temp-image-file              (cond (use-/dev/shm/-? "/dev/shm/image-dired_temp")
                                                            (t               "/tmp/image-dired_temp"))
             image-dired-temp-rotate-image-file       (cond (use-/dev/shm/-? "/dev/shm/image-dired_temp_rotate")
                                                            (t               "/tmp/image-dired_temp_rotate"))
             image-dired-thumbnail-storage            'standard)
       ;; Make dired use image-dired-dired-next-line and image-dired-dired-previous-line for extra niceness
       (add-hook 'dired-mode-hook
                 (lambda ()
                   (local-set-key "n"    'image-dired-dired-next-line)
                   (local-set-key "p"    'image-dired-dired-previous-line)
                   (local-set-key " "    'image-dired-dired-next-line)
                   (local-set-key "\C-n" 'image-dired-dired-next-line)
                   (local-set-key "\C-p" 'image-dired-dired-previous-line)
                   (local-set-key [down] 'image-dired-dired-next-line)
                   (local-set-key [up]   'image-dired-dired-previous-line))))))

;;;; battery
(when (and (> emacs-major-version 21) (require 'battery nil t))
  (when battery-status-function         ; if battery-status-function isn't nil we most likely have a battery
    (setq battery-mode-line-format "[%b%p%%,%t]")
    (display-battery-mode 1)))

;;;; wdired
(setq wdired-allow-to-change-permissions t)

;;;; JDE
(eval-after-load 'jde
  '(setq ;; jde-enable-abbrev-mode t
         jde-enable-abbrev-mode nil
         jde-mode-abbreviations '(("disp" . "System.out.println") ("ca" . "catch (Exception e) {") ("cl" . "class") ("co" . "const") ("ma" . "public static void main(String[] args)") ("pr" . "private") ("pro" . "protected") ("pu" . "public") ("st" . "static") ("sy" . "synchronized"))))



;;;; ERC
(eval-after-load 'erc
  '(progn
     (unless (and (require 'erc-highlight-nicknames nil t)
                  (add-to-list 'erc-modules 'highlight-nicknames)
                  ;; (erc-update-modules)
                  )
       (princ "No, I no get highlighted nicknames!!!!!!!!111oneoneeleventwelve"))
     (require 'erc-dcc)
     (require 'erc-log)
     ;; (require 'erc-truncate)
     (defvar erc-insert-post-hook)
     (add-hook 'erc-insert-post-hook
               'erc-truncate-buffer)
     (setq erc-log-channels-directory "~/erclogs/"
           erc-save-buffer-on-part nil
           erc-save-queries-on-quit nil
           erc-log-write-after-send t
           erc-log-write-after-insert t
           erc-truncate-buffer-on-save t
           erc-nick "Xantoz")
     (erc-log-enable)))

;;;; PROLOG
;; The suffix ".pl" is traditional, but conflicts with the more
;; popular language Perl. Some people choose to use ".pro" as a suffix
;; for Prolog files. In that case the next line is better than the one
;; above.
(setq auto-mode-alist (append '(("\\.pro$" . prolog-mode))
                              auto-mode-alist))

;;;; Makefile.<something> should be opened in makefile mode
(setq auto-mode-alist (acons "[Mm]akefile\\..*$" #'makefile-gmake-mode
                             auto-mode-alist))

;;;; debian postinst file should open with sh-mode
(push '("postinst" . sh-mode) auto-mode-alist)

(eval-after-load 'prolog
  '(progn
     (defun set-prolog-system (system)
       (interactive "Ssystem: ")
       (setq prolog-system system)
       (setq prolog-program-name (case prolog-system
                                   (gnu "gprolog")
                                   (swi "swipl")
                                   (sicstus (cond ((or i-am-monad i-am-udongein) "/usr/local/sicstus4.1.2/bin/sicstus")
                                                  (i-am-colgate                  "/usr/local/sicstus4.2.3/bin/sicstus")
                                                  (i-am-csc-ubuntu               "/opt/sicstus/4.2.0/bin/sicstus")
                                                  (t                             "sicstus")))))
       (when (eq prolog-system 'gnu)
         (setq prolog-consult-string "[%f].")))
     (setq prolog-indent-width 4)
     (set-prolog-system (if (and (not i-am-csc-ubuntu) (eq system-type 'gnu/linux)) 'swi 'sicstus))))

;;;; HASKELL
;(add-hook 'haskell-mode-hook 'turn-on-haskell-ghci)
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)

;;;; Octave
(add-hook 'octave-mode-hook #'(lambda () (setq comment-start "% "))) ; for MATLAB compatibility

;;;; Rudel
(defun load-rudel ()
  (interactive)
  (require 'rudel-mode)
  (require 'rudel-obby)
  (global-rudel-minor-mode))

(defun add-to-initial-and-default-frame-alist (&rest alist)
  (dolist (acons alist)
    (add-to-list 'initial-frame-alist acons)
    (add-to-list 'default-frame-alist acons)))

;;;; Font settings (applicable on some systems only)
(when (> emacs-major-version 21)
  (let ((font (cond
               ;; (i-am-colgate  "-misc-fixed-medium-r-normal--13-120-75-75-c-80-iso8859-1")
               ;; (i-am-colgate  "-*-fixed-normal-normal-normal-*-13-*-*-*-*-*-*-*")
               ;; (i-am-colgate "-*-courier-medium-r-*-*-12-*-*-*-*-*-*-*")
               (i-am-colgate "-misc-fixed-medium-r-semicondensed--13-120-75-75-c-60-iso10646-1")
               ;; (i-am-colgate "-adobe-courier-medium-r-*-*-12-*-*-*-*-*-*-*")
               (i-am-udongein "-*-courier-medium-r-*-*-12-*-*-*-*-*-*-*")
               (i-am-cirno "-1ASC-Liberation Mono-normal-normal-normal-*-20-*-*-*-m-0-iso10646-1")
               (i-am-nazrin "-PfEd-DejaVu Sans Mono-normal-normal-normal-*-11-*-*-*-m-0-iso10646-1"))))
    (when font
      (add-to-initial-and-default-frame-alist (cons 'font font)))))
;;;; END font settings

;;;; "Night mode" (really sort of my default "theme" with a key to toggle to default black on white for bright days)
(when (> emacs-major-version 21)
  (add-to-initial-and-default-frame-alist
   '(background-color . "gray15")
   '(foreground-color . "wheat")
   '(cursor-color . "wheat")
   '(night-mode-on . t)))

(when (featurep 'x)
  (defun night-mode ()
    (interactive)
    (set-background-color "gray15")
    (set-foreground-color "wheat")
    (set-cursor-color "wheat")
    (message "Night mode on")
    (set-frame-parameter nil 'night-mode-on t))

  (defun day-mode ()
    (interactive)
    (set-background-color "white")
    (set-foreground-color "black")
    (set-cursor-color "black")
    (message "Night mode off")
    (set-frame-parameter nil 'night-mode-on nil))

  (defun toggle-night-mode ()
    (interactive)
    (if (frame-parameter nil 'night-mode-on)
        (day-mode)
      (night-mode)))

  (global-set-key [f12] 'toggle-night-mode)
  (global-set-key [SunF37] 'toggle-night-mode))
;;;; END "Night mode"

;;;; BEGIN my hacky functions here
(defun lunix-mode ()
  (interactive)
  (c-set-style "linux")
  (setq indent-tabs-mode t))

(defun dired-mark-images ()
  (interactive)
  (dired-mark-files-regexp (image-file-name-regexp)))

(defun image-dired-dired-mark-untagged-files ()
  (interactive)
  (dired-unmark-all-marks)
  (dired-mark-images)
  (dired-map-over-marks
   (unless (equal (image-dired-list-tags (dired-get-filename))
                  '(""))
     (dired-unmark nil))
   nil
   t))

(defun %time (fn)
  (let* ((t0 (current-time)) result)
    (setq result (funcall fn))
    (print (time-to-seconds (time-subtract (current-time) t0)))
    result))

(defmacro time (form)
  `(%time (lambda () ,form)))

(defun latex-insert-figure (rawr)
  (interactive "Mname: ")
  (insert "\\begin{figure}[H]\n")
  (insert "  \\includegraphics{" rawr ".eps}\n")
  (insert "  \\caption{" rawr "}\n")
  (insert "\\end{figure}\n"))

(defun xterm ()
  "Launches an xterm in the current directory"
  (interactive)
  (start-process "xterm" "nil" "xterm" "-ls"))

(defun decode-hex-string (hex-string)
  (apply #'concat
     (loop for i from 0 to (- (/ (length hex-string) 2) 1)
           for hex-byte = (substring hex-string (* 2 i) (* 2 (+ i 1)))
           collect (format "%c" (string-to-number hex-byte 16)))))
;;;; END

;;;; this is really crap over from the minijava project
(defun woop (a)
  (interactive "Mwoop: ")
  (insert "case sym." (format "%s" a) ": ")
  (insert "return \"" (format "%s" a) "\";\n"))

(defun woopity ()
  (dolist (a '(GREATERTHAN TRUELITERAL BLOCKCLOSE BITWISEXOR GREATERTHANEQUAL LONGLITERAL BLOCKOPEN RIGHTPAREN BITWISEAND NOTEQUAL INDEXCLOSE INT PRINTLN LEFTPAREN LESSTHANEQUAL STATIC SEMICOLON NOT AND FALSELITERAL LESSTHAN OR COMMA CLASS DIV BITWISEOR ASSIGN IF THIS DOT EOF BOOLEAN RETURN EQUAL NEW error MUL ADD VOID INTLITERAL INDEXOPEN ELSE WHILE PUBLIC EXTENDS LONG STRING SUB LENGTH IDENTIFIER))
    (woop a)))
;;;; end crap


(require 'uniquify)
(setq
  uniquify-buffer-name-style 'post-forward
  uniquify-separator ":")

(defun insert-mode-comment-alt (mode)
  (interactive "Mmode: ")
  (insert (concat comment-start "-*- Mode: " mode " -*-")))

(defun insert-mode-comment (arg)
  (interactive "P")
  (insert (concat comment-start
                  "-*- Mode: "
                  (if arg
                      (read-from-minibuffer "mode: ")
                      mode-name)
                  " -*-")))

(defun insert-printf-thing (string)
  (interactive "Mstring: ")
  (indent-for-tab-command)
  (insert "printf(\"" string " %zd\\n\", " string ");\n"))


;; Do stuff to the frame font
(defun frame-font-grow (n)
  (interactive "P")
  (unless n (setf n 1))
  (let ((fonty (font-spec :name (frame-parameter nil 'font))))
    (font-put fonty :size (+ n (font-get fonty :size)))
    (set-frame-font (font-xlfd-name fonty) t)
    (message "Font size: %d" (font-get fonty :size))))

(defun frame-font-shrink (n)
  (interactive "P")
  (unless n (setf n 1))
  (frame-font-grow (- n)))

(defun set-frame-font-size (size)
  (interactive "nSize: ")
  (let ((fonty (font-spec :name (frame-parameter nil 'font))))
    (font-put fonty :size size)
    (set-frame-font (font-xlfd-name fonty) t)
    (message "Font size: %d" (font-get fonty :size))))

(global-set-key (kbd "C-x M-0") 'set-frame-font-size)
(global-set-key (kbd "C-x M--") 'frame-font-shrink)
(global-set-key (kbd "C-x M-+") 'frame-font-grow)
(global-set-key (kbd "C-x M-=") 'frame-font-grow)

;;;; "stolen" hacky functions here
(defun bf-pretty-print-xml-region (begin end)
  "Pretty format XML markup in region. You need to have nxml-mode
http://www.emacswiki.org/cgi-bin/wiki/NxmlMode installed to do
this.  The function inserts linebreaks to separate tags that have
nothing but whitespace between them.  It then indents the markup
by using nxml's indentation rules."
  (interactive "r")
  (save-excursion
      (nxml-mode)
      (goto-char begin)
      (while (search-forward-regexp "\>[ \\t]*\<" nil t)
        (backward-char) (insert "\n"))
      (indent-region begin end))
    (message "Ah, much better!"))

(defvar default-alpha '(95 95))
(setq day-mode-alpha '(80 80))
(add-to-list 'default-frame-alist `(alpha ,@default-alpha))
(defun toggle-transparency ()
  (interactive)
  (if (/=
       (car (frame-parameter nil 'alpha))
       100)
      (set-frame-parameter nil 'alpha '(100 100))
      (set-frame-parameter nil 'alpha (if (frame-parameter 'nil 'night-mode-on)
                                          default-alpha
                                          day-mode-alpha)))) ;TODO: fix this toggling to become better at understanding night-mode-toggling and all that
(global-set-key [f11] 'toggle-transparency)

(defun count-words (&optional begin end)
  "count words between BEGIN and END (region); if no region defined, count words in buffer.
Non-interactive calls disregard region entirely, but allow for optional begin/end parameters"
  (interactive "r")
  (let ((b (if (if (called-interactively-p 'any) (and mark-active begin) begin)
               begin
             (point-min)))
        (e (if (if (called-interactively-p 'any) (and mark-active end) end)
               end
             (point-max))))
    (message "Word count: %s" (how-many "\\w+" b e))))

(defun count-words-tex ()
  (interactive)
  (shell-command-on-region (point-min) (point-max) "detex | wc -w"))

(defun count-words-dwim (&optional begin end)
  "count words between BEGIN and END (region); if no region defined, count words in buffer.
DO WHAT I MEAN
TODO: Should i count-words-tex for regions somehow too?"
  (interactive "r")
  (cond ((and (not mark-active) (find major-mode '(latex-mode tex-mode))) (count-words-tex))
        (t (call-interactively 'count-words))))

;; Commented out because it collides with one prefix for hi-lock-mode
;(global-set-key (kbd "\C-x w") 'count-words-dwim)

(defun scratch-lisp-file ()
  "Insert a template (with DEFPACKAGE and IN-PACKAGE forms) into
   the current buffer."
  (interactive)
  (goto-char 0)
  (let* ((file (file-name-nondirectory (buffer-file-name)))
         (package (file-name-sans-extension file)))
    (insert ";;;; " file "\n")
    (insert "\n(defpackage #:" package "\n  (:use #:cl))\n\n")
    (insert "(in-package #:" package ")\n\n")))

;;;; make dired-do-async-shell-command capable of multiple programs by using rename-uniquely
(defadvice shell-command (after shell-in-new-buffer (command &optional output-buffer error-buffer))
  (when (get-buffer "*Async Shell Command*")
    (with-current-buffer "*Async Shell Command*"
      (rename-uniquely))))
(ad-activate 'shell-command)

;;;; wierd automatic stuff
(put 'upcase-region 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'erase-buffer 'disabled nil)

;; TODO: fix for cat foo*
(when nil
;;;; Magic to show images inline with iimage-mode in eshell
(defun my/iimage-mode-refresh--eshell/cat (orig-fun &rest args)
  "Display image when using cat on it."
  (let ((image-path (cons default-directory iimage-mode-image-search-path)))
    (dolist (arg args)
      (let ((imagep nil)
            file)
        (with-silent-modifications
          (save-excursion
            (dolist (pair iimage-mode-image-regex-alist)
              (when (and (not imagep)
                         (string-match (car pair) arg)
                         (setq file (match-string (cdr pair) arg))
                         (setq file (locate-file file image-path)))
                (setq imagep t)
                (add-text-properties 0 (length arg)
                                     `(display ,(create-image file)
                                               modification-hooks
                                               (iimage-modification-hook))
                                     arg)
                (eshell-buffered-print arg)
                (eshell-flush)))))
        (when (not imagep)
          (apply orig-fun (list arg)))))
    (eshell-flush)))
(advice-add 'eshell/cat :around #'my/iimage-mode-refresh--eshell/cat)
(add-hook 'eshell-mode-hook #'iimage-mode)
)

;;;; stuff from etu's conf (migrate your own stuff to use use-package in the future)
(when nil
  (use-package php-mode
    :ensure t
    :config
    (progn
      (setq-default php-mode-coding-style 'psr2)
      (use-package flymake-php
        :ensure t
        :config
        (add-hook 'php-mode-hook (lambda () (flymake-mode))))))

  (use-package web-mode
    :ensure t
    :mode "\\.twig\\'"
    :config (progn
              (setq-default web-mode-css-indent-offset 4   ; CSS
                            ;; web-mode-markup-indent-offset 4 ; HTML
                            ;; web-mode-code-indent-offset 4 ; JS/PHP/etc
                            web-mode-markup-indent-offset 2
                            web-mode-code-indent-offset 2
                            )))

  (use-package ac-php-company
    :ensure ac-php
    :config
    (progn
      ;; (setq-default ac-php-tags-path
      ;;               (concat user-emacs-cache-directory "/ac-php"))

      (add-hook 'php-mode-hook
                '(lambda ()
                   (unless (executable-find "ctags")
                     (error "Program: ctags is missing"))

                   ;; Add build company-backends with dabbrev and ac-php
                   (set (make-local-variable 'company-backends)
                        '((company-dabbrev-code
                           company-gtags
                           company-etags
                           company-keywords
                           company-ac-php-backend)))

                   (company-mode t)))

      (define-key php-mode-map (kbd "C-]") 'ac-php-find-symbol-at-point)
      (define-key php-mode-map (kbd "C-t") 'ac-php-location-stack-back)))
;;;; end etu stuff
  )


;;;; Window management
;; winner-mode (undo) (C-x left, C-x right)
(use-package winner
  :defer 1
  :config (winner-mode 1))

;; Make window switching a little easier. C-x-o is a pain.
;; Default keybindings are the arrow keys shifted
(use-package windmove
  :defer t
  :bind (("<S-up>"    . windmove-up)
         ("<S-down>"  . windmove-down)
         ("<S-left>"  . windmove-left)
         ("<S-right>" . windmove-right)
         ("M-s-i"     . windmove-up)
         ("M-s-k"     . windmove-down)
         ("M-s-j"     . windmove-left)
         ("M-s-l"     . windmove-right)))

;; Exchange contents of windows
(use-package buffer-move
  :ensure t
  :defer t
  :bind (("<C-S-up>"    . buf-move-up)
         ("<C-S-down>"  . buf-move-down)
         ("<C-S-left>"  . buf-move-left)
         ("<C-S-right>" . buf-move-right)
         ("C-M-s-i"     . buf-move-up)
         ("C-M-s-k"     . buf-move-down)
         ("C-M-s-j"     . buf-move-left)
         ("C-M-s-l"     . buf-move-right)))
;;;; END Frame management

;; I prefer to view ebuilds with 8 space-sized tabs
(add-hook 'ebuild-mode-hook
          #'(lambda ()
              (setq tab-width 8
                    sh-basic-offset 8))
          t)

(use-package lua-mode :ensure t :defer t)
(use-package rust-mode :ensure t :defer t)
(use-package qml-mode :ensure t :defer t)
(use-package js2-mode :ensure t :defer t)
(use-package yaml-mode :ensure t :defer t)
(use-package go-mode :ensure t :defer t)
(use-package markdown-mode :ensure t :defer t)
(use-package meson-mode :ensure t :defer t)
(use-package cmake-mode :ensure t :defer t)
(use-package fill-column-indicator :ensure t :defer t)
(use-package dts-mode :ensure t :defer t)
(use-package dockerfile-mode :ensure t :defer t)

(use-package groovy-mode
  :ensure t
  :defer t
  :config (setq groovy-indent-offset 2))

(use-package intel-hex-mode
  :ensure t
  :defer t
  :config (setq intel-hex-enable-overwrite nil))

(use-package pdf-tools
  :if (not i-am-headless-server)
  :ensure t
  :defer t
  :init (when (fboundp 'pdf-loader-install) (pdf-loader-install t t)))

(use-package webpaste :ensure t :defer t)
(use-package 0x0 :ensure t :defer t)
(use-package ox-gfm :ensure t :defer t)

(use-package gnuplot
  :defer t
  :commands gnuplot-mode gnuplot-make-buffer
  :mode ("\\.gp" . gnuplot-mode))

(use-package magit
  :ensure t
  :defer 1
  :config (setq magit-log-margin '(t "%Y-%m-%d %H:%M" magit-log-margin-width t 18)))

(use-package swiper
  :ensure t
  :defer t
  :bind (("s-s" . swiper)
         ("C-S-s" . swiper)))

(column-number-mode 1)

(use-package whitespace
  :defer t
  :config
  (setq whitespace-line-column nil      ; Honor fill column
        whitespace-style '(face tabs trailing empty lines-tail))
  :init
  (dolist (i '(c-mode-common-hook
               makefile-mode-hook
               lisp-mode-hook
               sh-mode-hook
               asm-mode-hook
               web-mode-hook
               ebuild-mode-hook
               nix-mode-hook
               bitbake-mode-hook))
    (add-hook i (lambda () (whitespace-mode 1)) t)))

(use-package bookmark
  :defer t
  :config
  ;; save bookmarks every time
  (setq bookmark-save-flag 1))

;; Make emacs stop whining about setting this in .dir-locals
(put 'cscope-initial-directory      'safe-local-variable t)
(put 'python-indent                 'safe-local-variable t)
(put 'web-mode-markup-indent-offset 'safe-local-variable t)
(put 'web-mode-code-indent-offset   'safe-local-variable t)
;; make this unrisky, against better knowing(?)
(put 'cscope-program 'risky-local-variable nil)

(eval-after-load 'rng-loc
  '(add-to-list 'rng-schema-locating-files "~/.emacs.d/schema/schemas.xml"))

(setq dired-listing-switches "-alh")    ; human sizes in dired
(setq list-directory-verbose-switches "-lh")
(setq find-name-arg "-iname")           ; I like to search case-insensitively by default
(setq hanoi-use-faces t)

(when (>= emacs-major-version 26)
  (setq semantic-imenu-index-directory t))

(setq large-file-warning-threshold (* 10000000 5))

(setq custom-file "~/.emacs-custom")
(load custom-file t t)
