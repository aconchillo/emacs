#!/bin/sh

if [ $# -eq 0 ]
then
  echo "Usage: `basename $0` <package> ..."
  exit 1
fi

emacs -q --batch --eval "(defconst pkgs-to-install '($*))" -l emacs-pkg-install.el
