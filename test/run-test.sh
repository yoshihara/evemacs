#!/bin/sh

bootemacs() {
    EMACS=emacs
    if [ "$#" = 1 ]; then
        EMACS=$1
    fi
    ${EMACS} -chdir test --no-site-file --directory `pwd` --script test-evemacs.el
}

bootemacs $@
