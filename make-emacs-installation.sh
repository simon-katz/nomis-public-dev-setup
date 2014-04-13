#!/bin/bash -e

source _fake-emacs.sh

fake_emacs emacs-installation -q -l "`pwd`/_make-emacs-installation.el"
