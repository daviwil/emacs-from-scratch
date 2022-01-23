#!/bin/sh

guix shell -m .tools/manifest.scm --pure -- emacs --with-profile=efs -l .tools/extras.el
#emacs --with-profile=efs -l .tools/extras.el
