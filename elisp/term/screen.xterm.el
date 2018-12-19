;;; screen.el --- terminal initialization for screen and tmux  -*- lexical-binding: t -*-
;; Copyright (C) 1995, 2001-2018 Free Software Foundation, Inc.

(require 'term/screen)

(defun terminal-init-screen.xterm ()
  "Terminal initialization function for screen.xterm"
  (let ((xterm-extra-capabilities xterm-screen-extra-capabilities))
    (tty-run-terminal-initialization (selected-frame) "screen")))

(provide 'term/screen.xterm)

;; screen.el ends here
