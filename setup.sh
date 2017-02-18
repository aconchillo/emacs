#!/bin/sh

install_emacs_packages()
{
    echo "Installing Emacs packages ..."

    ./emacs-pkg-install.sh \
        auto-complete \
        company \
        company-go \
        flx-ido \
        geiser \
        go-mode \
        icicles \
        lacarte \
        magit \
        markdown-mode \
        muse \
        paredit \
        paradox \
        projectile \
        smex \
        #
}

install_dependencies()
{
    echo "Cloning missing dependencies ..."

    if ! test -d elisp/doxymacs
    then
        echo "Cloning doxymacs ..."
        curdir=$(pwd)
        mkdir elisp/doxymacs
        curl -L https://downloads.sourceforge.net/project/doxymacs/doxymacs/1.8.0/doxymacs-1.8.0.tar.gz | tar zx -C elisp/doxymacs --strip-components 1
        cd elisp/doxymacs
        ./configure 2&> /dev/null
        cd $curdir
    fi
}

install_startup_files()
{
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
}

install_emacs_packages
install_dependencies
install_startup_files

echo
echo "Happy hacking!"
echo
