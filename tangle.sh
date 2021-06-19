#!/bin/sh
emacs -Q --batch --eval "
      (let ((file \"organizator.org\"))
        (require 'ob-tangle)
	(with-current-buffer (find-file-noselect file)
	  (org-babel-tangle)))"
