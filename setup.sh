#!/bin/sh

install_emacs_packages()
{
    echo "Installing Emacs packages ..."

    ./emacs-pkg-install.sh \
        ack \
        clang-format \
        company \
        company-lsp \
        cquery \
        flx-ido \
	elpy \
        geiser \
        go-guru \
        go-mode \
        go-rename \
        js2-mode \
        json-mode \
        lacarte \
        lsp-go \
        lsp-javascript-typescript \
        lsp-mode \
        lua-mode \
        magit \
        markdown-mode \
        muse \
        paredit \
        paradox \
        projectile \
        protobuf-mode \
        smex \
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
<<<<<<< HEAD
    pip3 install virtualenv virtualenvwrapper

    echo
    echo "***"
    echo "*** For every project create a new python environment and install these packages:"
    echo "***"
    echo "*** $ pip3 rope jedi flake8 autopep8 yapf"
    echo "***"
    echo
=======
    pip install virtualenv

    echo "Installing npm dependencies ..."
    npm i -g javascript-typescript-langserver
    npm i -g typescript-language-server

    echo "Installing go dependencies ..."
    pushd $GOPATH
    go get -u github.com/sourcegraph/go-langserver
    popd
>>>>>>> cleanup and make use of language server protocol
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
