#!/bin/sh
cd "$(dirname "$(realpath "$0")")"

exec emacs ../init.el --no-site-file --script use-package-extract.el -f print-packages 2>&1
