#!/bin/sh
cd "$(dirname "$(realpath "$0")")"

exec emacs ../init.el --no-site-file --script use-package-extract.el -f install-packages 2>&1
