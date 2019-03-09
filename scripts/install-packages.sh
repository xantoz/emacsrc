#!/bin/sh

exec emacs ~/.config/emacs/init.el --no-site-file --batch -l ~/.config/emacs/scripts/use-package-extract.el -f install-packages 2>&1
