(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("gnu" . "https://elpa.gnu.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")
                         ("ELPA" . "https://tromey.com/elpa/")))
(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))
