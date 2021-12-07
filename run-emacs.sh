#!/bin/sh

# This script is only meant for use in Emacs From Scratch streams.  It starts up
# Emacs with no other configuration than the one in init.el aside from some
# necessary tweaks in stream-tweaks.el that are only meant for use in the live
# stream.

#export EMACSLOADPATH="/run/current-system/profile/share/emacs/site-lisp:/run/current-system/profile/share/emacs/27.1/lisp"
emacs --load /Users/tenglong.tl/Works/Projects/emacs-from-scratch/stream-tweaks.el --load /Users/tenglong.tl/Works/Projects/emacs-from-scratch/init.el /Users/tenglong.tl/Works/Projects/emacs-from-scratch/Emacs.org
