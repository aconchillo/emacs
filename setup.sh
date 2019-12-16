#!/bin/bash

install_emacs_packages()
{
    echo "Installing Emacs packages ..."

    ./emacs-pkg-install.sh \
        clang-format \
        company \
        company-lsp \
        cquery \
        flycheck \
        geiser \
        go-rename \
        js2-mode \
        json-mode \
        lsp-mode \
        lsp-ui \
        lua-mode \
        magit \
        markdown-mode \
        meson-mode \
        muse \
        paredit \
        paradox \
        php-mode \
        projectile \
        protobuf-mode \
        smex \
        typescript-mode \
        vue-mode \
        yaml-mode \
        yasnippet \
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
    pip3 install python-language-server

    echo "Installing npm dependencies ..."
    npm i -g typescript
    npm i -g typescript-language-server

    echo "Installing go dependencies ..."
    pushd $GOPATH
    go get -u github.com/saibing/bingo
    popd
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
        cp -f $PWD/dot-ercinfo.el.in $PWD/dot-ercinfo.el
        chmod 600 $PWD/dot-ercinfo.el
        ln -sf $PWD/dot-ercinfo.el $HOME/.ercinfo
    fi
}

install_emacs_packages
install_dependencies
install_startup_files

echo
echo "Happy hacking!"
echo
