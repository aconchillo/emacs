#!/bin/sh

./emacs-pkg-install.sh \
  auto-complete \
  color-theme-sanityinc-solarized \
  flx-ido \
  magit \
  markdown-mode \
  muse \
  paradox \
  projectile \
  smex \
  #

echo
echo "Checking for .emacs ..."
if ! test -f $HOME/.emacs
then
    echo "Creating $HOME/.emacs ..."
    ln -s $PWD/dot-emacs.el $HOME/.emacs
fi

echo "Checking for .ercinfo ..."
if ! test -f $HOME/.ercinfo
then
    echo "Creating $HOME/.ercinfo ..."
    ln -s $PWD/dot-ercinfo.el $HOME/.ercinfo
fi

echo
echo "Happy hacking!"
echo
