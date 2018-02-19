#!/bin/sh

install_emacs_packages()
{
    echo "Installing Emacs packages ..."

    ./emacs-pkg-install.sh \
        ack \
        clang-format \
        company \
        company-go \
        company-jedi \
        flx-ido \
        geiser \
        go-guru \
        go-mode \
        go-rename \
        js2-mode \
        json-mode \
        lacarte \
        lua-mode \
        magit \
        markdown-mode \
        muse \
        paredit \
        paradox \
        projectile \
        rtags \
        smex \
        tide \
        vue-mode \
        yaml-mode \
        #
}

install_dependencies()
{
    echo "Cloning missing dependencies ..."

    if ! test -f elisp/doxymacs/lisp/doxymacs.el
    then
        echo "Cloning doxymacs ..."
        curdir=$(pwd)
        mkdir -p elisp/doxymacs
        curl -s -L https://downloads.sourceforge.net/project/doxymacs/doxymacs/1.8.0/doxymacs-1.8.0.tar.gz | tar zx -C elisp/doxymacs --strip-components 1
        cd elisp/doxymacs
        ./configure > /dev/null
        cd $curdir
    fi

    echo "Installing python dependencies ..."
    pip install --user virtualenv
    pip install --user jedi
}

install_startup_files()
{
    echo "Checking for .emacs ..."
    if ! test -f $HOME/.emacs
    then
        echo "Creating $HOME/.emacs ..."
        sed "s#<EMACS_HOME>#$PWD#" < dot-emacs.el.in > dot-emacs.el
        ln -sf $PWD/dot-emacs.el $HOME/.emacs
    fi

    echo "Checking for .ercinfo ..."
    if ! test -f $HOME/.ercinfo
    then
        echo "Creating $HOME/.ercinfo ..."
        ln -sf $PWD/dot-ercinfo.el $HOME/.ercinfo
    fi
}

install_emacs_packages
install_dependencies
install_startup_files

echo
echo "Happy hacking!"
echo
