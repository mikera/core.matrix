;;; Directory Local Variables
;;; See Info node `(emacs) Directory Variables' for more information.

((clojure-mode
  (eval put-clojure-indent 'c-for 'defun))
 (emacs-lisp-mode
  (eval put-clojure-indent 'expose-ndarrays 'defun)
  (eval put-clojure-indent 'loop-over-nd 'defun)
  (eval put-clojure-indent 'loop-over-2d 'defun)
  (eval put-clojure-indent 'loop-over-1d 'defun)
  (eval put-clojure-indent 'loop-over 'defun)
  (eval put-clojure-indent 'specialize 'defun)
  (eval put-clojure-indent 'iae-when-not 'defun)))
